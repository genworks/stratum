;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; The brush engine.  A stroke is its samples, (x y pressure) in document
;; pixels, and a brush: radius, hardness, spacing, flow, opacity, color
;; and mode.  Two dual functions do the part the browser and the server
;; must agree on to the last bit: place-dabs lays dabs along the stroke
;; by arc length, and paint-dab accumulates one dab's coverage into a
;; float buffer.  The browser runs them (dual.js) to preview a stroke
;; while it is drawn, painting coverage into a texture that the
;; generated shader composites onto the stroke's layer; when the
;; pointer lifts it sends the SAMPLES, not pixels, and the server runs
;; the same two functions, composites the coverage into the layer's
;; pixels once (one 8-bit round trip per stroke, not per dab) and
;; answers the dirty rectangle, which the browser re-uploads.  That
;; last step has two backends like everything else: composite-stroke!
;; here and the live-stroke block glsl-entry writes, operation for
;; operation.
;;
;; Painting never touches the plan.  A stroke changes a raster layer's
;; own pixel buffer and bumps its paint-version; the program is never
;; rebuilt.  Undo keeps the rectangle each stroke overwrote.
;;

;;; Dab placement.  A dab every SPACING * RADIUS * pressure of arc length
;;; (never less than a quarter pixel), the first on the first sample,
;;; pressure interpolated along each segment.  Writes (x y pressure)
;;; triples into OUT as far as it has room and returns the number of
;;; dabs, so a caller counts with an empty OUT, allocates, and calls
;;; again.  The dabs on the first k samples never depend on later
;;; samples, so a preview places dabs as samples arrive and keeps the
;;; ones it has painted.
(define-dual-function place-dabs (xs ys ps n radius spacing out)
  (declare (type (simple-array double-float (*)) xs ys ps out)
           (type fixnum n) (type double-float radius spacing)
           (optimize (speed 3) (safety 1)))
  (let* ((count 0)
         (room (length out))
         (leftover 0.0d0))
    (when (> n 0)
      (when (< 2 room)
        (setf (aref out 0) (aref xs 0))
        (setf (aref out 1) (aref ys 0))
        (setf (aref out 2) (aref ps 0)))
      (setf count 1)
      (setf leftover (max 0.25d0 (* spacing radius (aref ps 0)))))
    (dotimes (i (- n 1))
      (let* ((x0 (aref xs i))
             (y0 (aref ys i))
             (p0 (aref ps i))
             (dx (- (aref xs (+ i 1)) x0))
             (dy (- (aref ys (+ i 1)) y0))
             (dp (- (aref ps (+ i 1)) p0))
             (seg (sqrt (+ (* dx dx) (* dy dy))))
             (pos 0.0d0))
        (loop while (<= (+ pos leftover) seg)
              do (progn
                   (setf pos (+ pos leftover))
                   (let* ((tt (/ pos seg))
                          (p (+ p0 (* tt dp)))
                          (k (* 3 count)))
                     (when (< (+ k 2) room)
                       (setf (aref out k) (+ x0 (* tt dx)))
                       (setf (aref out (+ k 1)) (+ y0 (* tt dy)))
                       (setf (aref out (+ k 2)) p))
                     (setf count (+ count 1))
                     (setf leftover (max 0.25d0 (* spacing radius p))))))
        (setf leftover (- leftover (- seg pos)))))
    count))

;;; One dab into BUF, a BW x BH coverage buffer whose top-left pixel is
;;; document pixel (OX, OY): a disc of RADIUS at (CX, CY), full inside
;;; RADIUS * HARDNESS and falling smoothly to nothing at the edge (a
;;; hard brush keeps a one-pixel ramp), each pixel's coverage raised
;;; by FLOW * falloff of what it still lacks, so overlapping dabs build
;;; toward full coverage and never past it.  Pixel centers are at
;;; half-integers.
(define-dual-function paint-dab (buf bw bh ox oy cx cy radius hardness flow)
  (declare (type (simple-array double-float (*)) buf)
           (type fixnum bw bh ox oy)
           (type double-float cx cy radius hardness flow)
           (optimize (speed 3) (safety 1)))
  (let* ((inner (min (* radius hardness) (max 0.0d0 (- radius 1.0d0))))
         (r2 (* radius radius))
         (i2 (* inner inner))
         (ramp (- radius inner))
         (bx0 (max 0 (- (floor (- cx radius)) ox)))
         (bx1 (min (- bw 1) (- (ceiling (+ cx radius)) ox)))
         (by0 (max 0 (- (floor (- cy radius)) oy)))
         (by1 (min (- bh 1) (- (ceiling (+ cy radius)) oy))))
    (loop for by from by0 to by1
          do (let* ((py (- (+ by oy 0.5d0) cy))
                    (py2 (* py py))
                    (row (* by bw)))
               (loop for bx from bx0 to bx1
                     do (let* ((px (- (+ bx ox 0.5d0) cx))
                               (d2 (+ (* px px) py2)))
                          (when (< d2 r2)
                            (let* ((f (if (<= d2 i2)
                                          1.0d0
                                          (let* ((u (/ (- radius (sqrt d2)) ramp)))
                                            (* u u (- 3.0d0 (* 2.0d0 u))))))
                                   (v (* flow f))
                                   (k (+ row bx))
                                   (c (aref buf k)))
                              (setf (aref buf k) (+ c (* v (- 1.0d0 c))))))))))
    buf))

;;; ---- the server's half ---------------------------------------------------

(defparameter *undo-depth* 32
  "Integer. How many strokes a document keeps the pixels to undo.")

(defparameter *default-brush*
  '(:radius 24d0 :hardness 0.8d0 :spacing 0.25d0 :flow 1d0 :opacity 1d0
    :color (0 0 0) :mode :paint))

(defun stroke-number (stroke key)
  (float (getf stroke key (getf *default-brush* key)) 1d0))

(defun stroke-vectors (points)
  "Three double-float vectors, x, y and pressure, from POINTS, a sequence
of (x y &optional pressure ...) samples.  A missing pressure is 1."
  (let* ((n (length points))
         (xs (make-array n :element-type 'double-float))
         (ys (make-array n :element-type 'double-float))
         (ps (make-array n :element-type 'double-float)))
    (loop for p in (coerce points 'list)
          for i from 0
          do (setf (aref xs i) (float (first p) 1d0)
                   (aref ys i) (float (second p) 1d0)
                   (aref ps i) (max 0d0 (min 1d0 (float (or (third p) 1) 1d0)))))
    (values xs ys ps)))

(defun stroke-dabs (stroke)
  "The dabs of STROKE as one double-float vector of (x y pressure)
triples: place-dabs once to count, once to fill."
  (multiple-value-bind (xs ys ps) (stroke-vectors (getf stroke :points))
    (let* ((n (length xs))
           (radius (stroke-number stroke :radius))
           (spacing (stroke-number stroke :spacing))
           (count (place-dabs xs ys ps n radius spacing
                              (make-array 0 :element-type 'double-float)))
           (out (make-array (* 3 count) :element-type 'double-float)))
      (place-dabs xs ys ps n radius spacing out)
      out)))

(defun dabs-rect (dabs radius width height)
  "The rectangle (x y w h) the DABS of RADIUS (times each dab's pressure)
can touch, clipped to WIDTH x HEIGHT; nil when they all fall outside."
  (let ((x0 most-positive-fixnum) (y0 most-positive-fixnum)
        (x1 most-negative-fixnum) (y1 most-negative-fixnum))
    (loop for k from 0 below (length dabs) by 3
          do (let ((r (* radius (aref dabs (+ k 2))))
                   (cx (aref dabs k)) (cy (aref dabs (+ k 1))))
               (setf x0 (min x0 (floor (- cx r)))  x1 (max x1 (ceiling (+ cx r)))
                     y0 (min y0 (floor (- cy r)))  y1 (max y1 (ceiling (+ cy r))))))
    (let ((x0 (max 0 x0)) (y0 (max 0 y0))
          (x1 (min (1- width) x1)) (y1 (min (1- height) y1)))
      (when (and (<= x0 x1) (<= y0 y1))
        (list x0 y0 (1+ (- x1 x0)) (1+ (- y1 y0)))))))

(defun stroke-coverage (stroke width height)
  "STROKE rasterized: (values coverage x y w h), a W x H double-float
buffer at X, Y of a WIDTH x HEIGHT document, or nil when the stroke
touches no pixel.  Only the dual functions touch pixels here, so the
browser's preview buffer holds the same numbers."
  (let* ((dabs (stroke-dabs stroke))
         (radius (stroke-number stroke :radius))
         (hardness (max 0d0 (min 1d0 (stroke-number stroke :hardness))))
         (flow (max 0d0 (min 1d0 (stroke-number stroke :flow))))
         (rect (dabs-rect dabs radius width height)))
    (when rect
      (destructuring-bind (x y w h) rect
        (let ((coverage (make-array (* w h) :element-type 'double-float :initial-element 0d0)))
          (loop for k from 0 below (length dabs) by 3
                do (paint-dab coverage w h x y (aref dabs k) (aref dabs (+ k 1))
                              (* radius (aref dabs (+ k 2))) hardness flow))
          (values coverage x y w h))))))

(defun composite-stroke! (pixels width coverage x y w h r g b opacity erase?)
  "Composite a stroke into PIXELS, an RGBA8 sRGB straight-alpha image
WIDTH wide: COVERAGE is W x H doubles at X, Y; each pixel takes paint
of linear color R G B with alpha coverage * OPACITY over it, in
premultiplied linear light, or with ERASE? keeps its color and loses
that much alpha.  The twin of the live-stroke block in glsl-entry."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
           (type (simple-array double-float (*)) coverage)
           (type fixnum width x y w h)
           (type double-float r g b opacity)
           (optimize (speed 3) (safety 1)))
  (let ((decode *srgb-decode*))
    (declare (type (simple-array double-float (256)) decode))
    (dotimes (row h)
      (dotimes (col w)
        (let ((a (* (aref coverage (+ col (* row w))) opacity)))
          (declare (type double-float a))
          (when (> a 0.0)
            (let* ((o (* 4 (+ x col (* width (+ y row)))))
                   (la (/ (float (aref pixels (+ o 3)) 1.0) 255.0)))
              (declare (type fixnum o) (type double-float la))
              (if erase?
                  (setf (aref pixels (+ o 3)) (unit->byte (* la (- 1.0 a))))
                  (let* ((keep (* la (- 1.0 a)))
                         (na (+ a keep))
                         (nr (/ (+ (* r a) (* (aref decode (aref pixels o)) keep)) na))
                         (ng (/ (+ (* g a) (* (aref decode (aref pixels (+ o 1))) keep)) na))
                         (nb (/ (+ (* b a) (* (aref decode (aref pixels (+ o 2))) keep)) na)))
                    (declare (type double-float keep na nr ng nb))
                    (setf (aref pixels o) (unit->byte (linear->srgb nr))
                          (aref pixels (+ o 1)) (unit->byte (linear->srgb ng))
                          (aref pixels (+ o 2)) (unit->byte (linear->srgb nb))
                          (aref pixels (+ o 3)) (unit->byte na)))))))))
    pixels))

(defun stroke-color-linear (stroke)
  "The stroke's :color (sRGB 0-255) as three linear doubles."
  (let ((color (getf stroke :color (getf *default-brush* :color))))
    (mapcar (lambda (c) (srgb->linear (/ (max 0d0 (min 255d0 (float c 1d0))) 255d0)))
            (list (first color) (second color) (third color)))))

(defun stroke-gray (stroke)
  "The value a stroke paints on a mask, in [0,1] on the mask's encoded
scale: the luminance of the stroke's color, so black hides, white
reveals, and a color lands between."
  (destructuring-bind (r g b) (stroke-color-linear stroke)
    (linear->srgb (+ (* 0.2126d0 r) (* 0.7152d0 g) (* 0.0722d0 b)))))

(defun composite-mask-stroke! (mask width coverage x y w h gray opacity)
  "Paint a stroke into MASK, one byte a pixel, WIDTH wide: COVERAGE is
W x H doubles at X, Y; each pixel moves toward GRAY by coverage *
OPACITY.  The twin of the shader's masked()."
  (declare (type (simple-array (unsigned-byte 8) (*)) mask)
           (type (simple-array double-float (*)) coverage)
           (type fixnum width x y w h)
           (type double-float gray opacity)
           (optimize (speed 3) (safety 1)))
  (dotimes (row h mask)
    (dotimes (col w)
      (let ((a (* (aref coverage (+ col (* row w))) opacity)))
        (declare (type double-float a))
        (when (> a 0.0)
          (let* ((o (+ x col (* width (+ y row))))
                 (m (/ (float (aref mask o) 1.0) 255.0)))
            (declare (type fixnum o) (type double-float m))
            (setf (aref mask o) (unit->byte (+ m (* (- gray m) a))))))))))

(defun push-undo! (doc entry)
  (let ((entries (cons entry (the-object doc undo-entries))))
    (the-object doc (set-slot! :undo-entries
                               (if (> (length entries) *undo-depth*)
                                   (subseq entries 0 *undo-depth*)
                                   entries)))))

(defun apply-brush-stroke (layer stroke &key (target :layer))
  "Paint STROKE into LAYER -- its pixels (TARGET :layer, a raster layer)
or its mask (TARGET :mask, any layer with one) -- and return the dirty
rectangle (x y w h), or nil when the stroke touched no pixel.  STROKE is
a plist: :points, a sequence of (x y pressure ...) samples in document
pixels; :radius; :hardness; :spacing (a fraction of the radius); :flow;
:opacity; :color (sRGB 0-255); :mode (:paint or :erase, for the pixels;
a mask takes the color's luminance either way).

Dabs are placed and rasterized by the dual functions place-dabs and
paint-dab, which the browser's preview also runs, and composited once by
composite-stroke! (linear light) or composite-mask-stroke!, whose twins
the generated shader carries for the preview; so the committed stroke
is what the preview showed, and the server's copy is the one kept.  The
layer gets its own buffer on its first stroke (the pixel cache's copy
is never painted), the rectangle's old bytes go on the document's undo
list, and the layer's paint-version or mask-version is bumped so the
renders see the change; the plan is untouched."
  (ecase target
    (:layer (unless (eql (the-object layer kind) :raster)
              (error "~a is not a raster layer." (the-object layer layer-name))))
    (:mask (unless (the-object layer has-mask?)
             (error "~a has no mask." (the-object layer layer-name)))))
  (let ((width (the-object layer doc-width))
        (height (the-object layer doc-height)))
    (multiple-value-bind (coverage x y w h) (stroke-coverage stroke width height)
      (when coverage
        (let ((doc (the-object layer parent))
              (opacity (max 0d0 (min 1d0 (stroke-number stroke :opacity)))))
          (ecase target
            (:layer
             (let ((pixels (the-object layer (ensure-own-pixels!)))
                   (erase? (eql (getf stroke :mode :paint) :erase)))
               (push-undo! doc (list (the-object layer index) :layer x y w h
                                     (rgba-rect pixels width height x y w h 4)))
               (destructuring-bind (r g b) (stroke-color-linear stroke)
                 (composite-stroke! pixels width coverage x y w h r g b opacity erase?))
               (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))))
            (:mask
             (let ((mask (the-object layer (ensure-own-mask!))))
               (push-undo! doc (list (the-object layer index) :mask x y w h
                                     (rgba-rect mask width height x y w h 1)))
               (composite-mask-stroke! mask width coverage x y w h (stroke-gray stroke) opacity)
               (the-object layer (set-slot! :mask-version (1+ (the-object layer mask-version)))))))
          (list x y w h))))))

(defun undo-stroke! (doc)
  "Put back the bytes the last stroke on DOC overwrote, exactly, and
return (layer-index target x y w h); nil when nothing is left to undo."
  (let ((entry (first (the-object doc undo-entries))))
    (when entry
      (the-object doc (set-slot! :undo-entries (rest (the-object doc undo-entries))))
      (destructuring-bind (index target x y w h bytes) entry
        (let ((layer (the-object doc (layer index)))
              (width (the-object doc doc-width)))
          (ecase target
            (:layer
             (replace-rect! (the-object layer own-pixels) width x y w h bytes 4)
             (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version)))))
            (:mask
             (replace-rect! (the-object layer own-mask) width x y w h bytes 1)
             (the-object layer (set-slot! :mask-version (1+ (the-object layer mask-version))))))
          (list index target x y w h))))))
