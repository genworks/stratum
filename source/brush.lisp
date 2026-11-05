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

(defun stroke-coverage (stroke width height &optional selection)
  "STROKE rasterized: (values coverage x y w h), a W x H double-float
buffer at X, Y of a WIDTH x HEIGHT document, or nil when the stroke
touches no pixel.  Only the dual functions touch pixels here, so the
browser's preview buffer holds the same numbers; a SELECTION multiplies
the result as the browser multiplies its own (clip-coverage!)."
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
          (clip-coverage! coverage x y w h selection width)
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

(defun clone-source-rect (pixels width height sx sy w h)
  "The W x H rectangle of PIXELS (RGBA8, WIDTH x HEIGHT) at SX, SY as a
fresh buffer, transparent black wherever it reaches beyond the image:
what the Clone Stamp paints from, copied before the stroke lands so a
stroke over its own source reads the pixels as they were."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
           (type fixnum width height sx sy w h))
  (let ((out (make-array (* 4 w h) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (row h out)
      (let ((y (+ sy row)))
        (when (and (<= 0 y) (< y height))
          (let ((x0 (max 0 sx)) (x1 (min width (+ sx w))))
            (when (< x0 x1)
              (replace out pixels
                       :start1 (* 4 (+ (- x0 sx) (* w row)))
                       :start2 (* 4 (+ x0 (* width y)))
                       :end2 (* 4 (+ x1 (* width y)))))))))))

(defun composite-clone-stroke! (pixels width coverage x y w h source opacity)
  "Composite a Clone Stamp stroke into PIXELS as composite-stroke! does,
the paint at each pixel being SOURCE's pixel there (a W x H RGBA8
buffer from clone-source-rect) in place of one color: its alpha times
the coverage times OPACITY over the pixel, in premultiplied linear
light.  The twin of the clone branch of the live-stroke block in
glsl-entry."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels source)
           (type (simple-array double-float (*)) coverage)
           (type fixnum width x y w h)
           (type double-float opacity)
           (optimize (speed 3) (safety 1)))
  (let ((decode *srgb-decode*))
    (declare (type (simple-array double-float (256)) decode))
    (dotimes (row h)
      (dotimes (col w)
        (let* ((i (+ col (* row w)))
               (s (* 4 i))
               (a (* (aref coverage i) opacity (/ (float (aref source (+ s 3)) 1.0) 255.0))))
          (declare (type fixnum i s) (type double-float a))
          (when (> a 0.0)
            (let* ((o (* 4 (+ x col (* width (+ y row)))))
                   (la (/ (float (aref pixels (+ o 3)) 1.0) 255.0))
                   (keep (* la (- 1.0 a)))
                   (na (+ a keep))
                   (nr (/ (+ (* (aref decode (aref source s)) a) (* (aref decode (aref pixels o)) keep)) na))
                   (ng (/ (+ (* (aref decode (aref source (+ s 1))) a) (* (aref decode (aref pixels (+ o 1))) keep)) na))
                   (nb (/ (+ (* (aref decode (aref source (+ s 2))) a) (* (aref decode (aref pixels (+ o 2))) keep)) na)))
              (declare (type fixnum o) (type double-float la keep na nr ng nb))
              (setf (aref pixels o) (unit->byte (linear->srgb nr))
                    (aref pixels (+ o 1)) (unit->byte (linear->srgb ng))
                    (aref pixels (+ o 2)) (unit->byte (linear->srgb nb))
                    (aref pixels (+ o 3)) (unit->byte na)))))))
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

(defparameter *undo-bytes* (* 128 1024 1024)
  "Integer. The most bytes a document's undo list keeps; the oldest
entries go first.  A whole 4K layer is 33 MB.  (512 MB until
2026-11-03: with documents per session on a 1 GB heap, four sessions'
worth of 1080p undo entries exhausted it; relieve-memory! in web.lisp
trims these lists first when the heap runs short.)")

(defun entry-bytes (entry)
  "The bytes an undo ENTRY holds: a layer or mask entry's, or all of a
mode change's (:mode from to . entries) or a prompt's (:prompt specs
selection mode . items)."
  (case (first entry)
    (:mode (reduce #'+ (cdddr entry) :key #'entry-bytes))
    (:prompt (reduce #'+ (nthcdr 4 entry) :key #'entry-bytes))
    (t (length (seventh entry)))))

(defun trimmed-entries (entry entries)
  "ENTRY in front of ENTRIES, kept to *undo-depth* entries and
*undo-bytes* bytes."
  (let ((bytes 0)
        (kept '()))
    (loop for e in (cons entry entries)
          for n from 1
          do (incf bytes (entry-bytes e))
             (when (or (> n *undo-depth*) (and (> n 1) (> bytes *undo-bytes*)))
               (return))
             (push e kept))
    (nreverse kept)))

;;; A prompt is one undo.  Its forms may add and delete layers, set
;;; opacities and blend modes and the selection, none of which the undo
;;; list holds, and a change to the stack empties that list.  So while
;;; a prompt runs the document keeps a journal: the stack's specs, the
;;; selection and the mode as they were, and the bytes of every in-place
;;; change, by array, since the layer objects and their indices do not
;;; last.  The specs hold the arrays themselves; the bytes put those
;;; arrays back.  At the end the journal is one entry, (:prompt specs
;;; selection mode . items), on the undo list as it stood when the
;;; prompt began: undoing it restores the stack those older entries were
;;; made on, so they stay good.  An item is (array channels x y w h
;;; bytes), newest first.

(defstruct (prompt-journal (:constructor make-prompt-journal (specs selection mode undo)))
  specs selection mode undo (items nil))

(defun begin-prompt-journal! (doc)
  "A journal for a prompt on DOC, now running on it; nil when one is
running already, whose journal this prompt's changes then join."
  (unless (the-object doc prompt-journal)
    (let ((journal (make-prompt-journal (the-object doc specs-now) (the-object doc selection)
                                        (the-object doc color-mode) (the-object doc undo-entries))))
      (the-object doc (set-slot! :prompt-journal journal))
      journal)))

(defun end-prompt-journal! (doc journal changed?)
  "The prompt is over: with CHANGED? its journal goes on the undo list
as one entry, over the list as it stood when the prompt began."
  (when journal
    (the-object doc (set-slot! :prompt-journal nil))
    (when changed?
      (the-object doc (set-slot! :undo-entries
                                 (trimmed-entries (list* :prompt (prompt-journal-specs journal)
                                                         (prompt-journal-selection journal)
                                                         (prompt-journal-mode journal)
                                                         (prompt-journal-items journal))
                                                  (prompt-journal-undo journal))))
      (the-object doc (set-slot! :redo-entries nil)))))

(defun note-bytes! (doc array channels x y w h bytes)
  "BYTES, the W x H rectangle of ARRAY at X, Y before a change, into the
running prompt's journal, unless an older item holds that rectangle: it
is the older bytes that go back, so a whole layer is kept once however
many filters a prompt runs on it.  No array is one the journal's specs
do not hold, which goes when they come back."
  (let ((journal (the-object doc prompt-journal)))
    (when (and journal array
               (notany (lambda (item)
                         (destructuring-bind (a c ix iy iw ih b) item
                           (declare (ignore c b))
                           (and (eq a array) (<= ix x) (<= iy y)
                                (<= (+ x w) (+ ix iw)) (<= (+ y h) (+ iy ih)))))
                       (prompt-journal-items journal)))
      (push (list array channels x y w h bytes) (prompt-journal-items journal)))))

(defun note-entry! (doc entry)
  "An undo ENTRY's bytes into the running prompt's journal."
  (when (the-object doc prompt-journal)
    (case (first entry)
      (:mode (dolist (e (cdddr entry)) (note-entry! doc e)))
      (:prompt nil)
      (t (destructuring-bind (index target x y w h bytes) entry
           (let ((layer (the-object doc (layer index))))
             (if (eql target :layer)
                 (note-bytes! doc (the-object layer own-pixels) 4 x y w h bytes)
                 (note-bytes! doc (the-object layer own-mask) 1 x y w h bytes))))))))

(defun push-undo! (doc entry &key keep-redo)
  "ENTRY, (layer-index target x y w h bytes), onto DOC's undo list, which
keeps at most *undo-depth* entries and *undo-bytes* bytes.  A new change
empties the redo list; an undo being redone (KEEP-REDO) does not."
  (note-entry! doc entry)
  (the-object doc (set-slot! :undo-entries (trimmed-entries entry (the-object doc undo-entries))))
  (unless keep-redo
    (the-object doc (set-slot! :redo-entries nil))))

(defun push-redo! (doc entry)
  (the-object doc (set-slot! :redo-entries (trimmed-entries entry (the-object doc redo-entries)))))

;;; ---- adjustments baked into a layer ---------------------------------------
;;;
;;; Image > Adjustments: the same arithmetic the adjustment layers do on
;;; the composited picture, done once to one raster layer's own pixels,
;;; on the encoded scale, alpha untouched.  A table adjustment on 8-bit
;;; pixels is a 256-entry byte table, which is what the shader's lut()
;;; gives at the integer positions.  The viewer previews through the
;;; shader's adjustment stage (glsl-entry); this is its twin.

(defun byte-table (lut)
  (let ((table (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256 table)
      (setf (aref table i) (unit->byte (float (aref lut i) 1d0))))))

(defun apply-tables! (pixels red green blue)
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels red green blue)
           (optimize (speed 3) (safety 1)))
  (loop for o of-type fixnum from 0 below (length pixels) by 4
        do (setf (aref pixels o) (aref red (aref pixels o))
                 (aref pixels (+ o 1)) (aref green (aref pixels (+ o 1)))
                 (aref pixels (+ o 2)) (aref blue (aref pixels (+ o 2)))))
  pixels)

(defun apply-hue-saturation! (pixels k0 k1 k2)
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
           (type double-float k0 k1 k2)
           (optimize (speed 3) (safety 1)))
  (loop for o of-type fixnum from 0 below (length pixels) by 4
        do (multiple-value-bind (r g b)
               (hue-saturation (/ (float (aref pixels o) 1.0) 255.0)
                               (/ (float (aref pixels (+ o 1)) 1.0) 255.0)
                               (/ (float (aref pixels (+ o 2)) 1.0) 255.0)
                               k0 k1 k2)
             (declare (type double-float r g b))
             (setf (aref pixels o) (unit->byte r)
                   (aref pixels (+ o 1)) (unit->byte g)
                   (aref pixels (+ o 2)) (unit->byte b))))
  pixels)

(defun adjust-layer! (layer kind &key settings curves)
  "Bake an adjustment into raster LAYER's pixels and return the
rectangle it changed, the whole layer.  KIND is :levels (SETTINGS a
plist of :in-black :in-white :gamma :out-black :out-white), :curves
(CURVES a plist of :master :red :green :blue point lists) or
:hue-saturation (SETTINGS :hue in degrees, :saturation, :lightness).
The layer's pixels before go on the undo list."
  (unless (eql (the-object layer kind) :raster)
    (error "~a is not a raster layer." (the-object layer layer-name)))
  (let* ((pixels (the-object layer (ensure-own-pixels!)))
         (doc (the-object layer parent))
         (w (the-object layer doc-width))
         (h (the-object layer doc-height)))
    (flet ((setting (key default) (float (getf settings key default) 1d0)))
      (push-undo! doc (list (the-object layer index) :layer 0 0 w h (copy-seq pixels)))
      (ecase kind
        (:levels
         (let ((table (byte-table (levels-lut (setting :in-black 0) (setting :in-white 1)
                                              (setting :gamma 1)
                                              (setting :out-black 0) (setting :out-white 1)
                                              256))))
           (apply-tables! pixels table table table)))
        (:curves
         (let ((master (getf curves :master *identity-curve*)))
           (apply-tables! pixels
                          (byte-table (channel-lut master (getf curves :red *identity-curve*)))
                          (byte-table (channel-lut master (getf curves :green *identity-curve*)))
                          (byte-table (channel-lut master (getf curves :blue *identity-curve*))))))
        (:hue-saturation
         (apply-hue-saturation! pixels (/ (setting :hue 0) 360) (setting :saturation 0)
                                (setting :lightness 0))))
      (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))
      (list 0 0 w h))))

(defun apply-brush-stroke (layer stroke &key (target :layer))
  "Paint STROKE into LAYER -- its pixels (TARGET :layer, a raster layer)
or its mask (TARGET :mask, any layer with one) -- and return the dirty
rectangle (x y w h), or nil when the stroke touched no pixel.  STROKE is
a plist: :points, a sequence of (x y pressure ...) samples in document
pixels; :radius; :hardness; :spacing (a fraction of the radius); :flow;
:opacity; :color (sRGB 0-255); :mode (:paint or :erase, for the pixels;
a mask takes the color's luminance either way); :clone, (dx dy) for the
Clone Stamp: the paint at each pixel is the layer's own pixel dx, dy
away, as it was before the stroke, transparent beyond the layer; only
the pixels take it, not a mask.  With :heal as well, the Healing Brush:
the source's texture on the destination's tone (heal-stroke!).  :history,
the History Brush: the paint is the document's history composite at
the same place (history-pixels, history.lisp).

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
        (height (the-object layer doc-height))
        (doc (the-object layer parent)))
    (multiple-value-bind (coverage x y w h)
        (stroke-coverage stroke width height (the-object doc selection))
      (when coverage
        (let ((opacity (max 0d0 (min 1d0 (stroke-number stroke :opacity)))))
          (ecase target
            (:layer
             (let ((pixels (the-object layer (ensure-own-pixels!)))
                   (erase? (eql (getf stroke :mode :paint) :erase))
                   (clone (getf stroke :clone)))
               (push-undo! doc (list (the-object layer index) :layer x y w h
                                     (rgba-rect pixels width height x y w h 4)))
               (cond ((and (getf stroke :history) (not erase?))
                      ;; the History Brush: the history composite at the
                      ;; same place, as the document's mode takes it
                      (let ((source (doc-pixels doc (clone-source-rect (history-pixels doc) width height x y w h))))
                        (composite-clone-stroke! pixels width coverage x y w h source opacity)))
                     ((and clone (not erase?) (getf stroke :heal))
                      (heal-stroke! pixels width height coverage x y w h
                                    (round (first clone)) (round (second clone))
                                    (stroke-number stroke :radius) opacity))
                     ((and clone (not erase?))
                      (let ((source (clone-source-rect pixels width height
                                                       (+ x (round (first clone))) (+ y (round (second clone))) w h)))
                        (composite-clone-stroke! pixels width coverage x y w h source opacity)))
                     (t (destructuring-bind (r g b)
                            (stroke-color-linear
                             (list :color (doc-color doc (getf stroke :color (getf *default-brush* :color)))))
                          (composite-stroke! pixels width coverage x y w h r g b opacity erase?))))
               (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))))
            (:mask
             (let ((mask (the-object layer (ensure-own-mask!))))
               (push-undo! doc (list (the-object layer index) :mask x y w h
                                     (rgba-rect mask width height x y w h 1)))
               (composite-mask-stroke! mask width coverage x y w h (stroke-gray stroke) opacity)
               (the-object layer (set-slot! :mask-version (1+ (the-object layer mask-version)))))))
          (list x y w h))))))

(defun swap-prompt-entry! (doc entry)
  "A prompt's entry put back: the bytes, newest first, then the stack,
the selection and the mode.  The mirror image, for the other list.  The
stack goes in by its slot: replace-specs! would empty the lists, whose
older entries this stack is the one for."
  (destructuring-bind (specs selection mode &rest items) (rest entry)
    (let ((width (the-object doc doc-width))
          (height (the-object doc doc-height))
          (now-specs (the-object doc specs-now))
          (now-selection (the-object doc selection))
          (now-mode (the-object doc color-mode))
          (mirror '()))
      (dolist (item items)
        (destructuring-bind (array channels x y w h bytes) item
          (let ((now (rgba-rect array width height x y w h channels)))
            (replace-rect! array width x y w h bytes channels)
            (note-bytes! doc array channels x y w h now)
            (push (list array channels x y w h now) mirror))))
      (the-object doc (set-slot! :layer-specs specs))
      (the-object doc (set-slot! :selection selection))
      (the-object doc (set-slot! :color-mode mode))
      (list* :prompt now-specs now-selection now-mode mirror))))

(defun swap-entry! (doc entry)
  "Put ENTRY's bytes back into its layer and return the entry holding
what they replaced: the mirror image, for the other list.  A mode
change's entry, (:mode from to . entries), puts every layer back and
the mode with them; a prompt's, the document as it was."
  (when (eql (first entry) :prompt)
    (return-from swap-entry! (swap-prompt-entry! doc entry)))
  (when (eql (first entry) :mode)
    (destructuring-bind (from to &rest entries) (rest entry)
      (let ((swapped (mapcar (lambda (e) (swap-entry! doc e)) entries)))
        (the-object doc (set-slot! :color-mode from))
        (return-from swap-entry! (list* :mode to from swapped)))))
  (destructuring-bind (index target x y w h bytes) entry
    (let* ((layer (the-object doc (layer index)))
           (width (the-object doc doc-width))
           (height (the-object doc doc-height)))
      (ecase target
        (:layer
         (let ((now (rgba-rect (the-object layer own-pixels) width height x y w h 4)))
           (note-bytes! doc (the-object layer own-pixels) 4 x y w h now)
           (replace-rect! (the-object layer own-pixels) width x y w h bytes 4)
           (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))
           (list index target x y w h now)))
        (:mask
         (let ((now (rgba-rect (the-object layer own-mask) width height x y w h 1)))
           (note-bytes! doc (the-object layer own-mask) 1 x y w h now)
           (replace-rect! (the-object layer own-mask) width x y w h bytes 1)
           (the-object layer (set-slot! :mask-version (1+ (the-object layer mask-version))))
           (list index target x y w h now)))))))

(defun undo-stroke! (doc)
  "Put back the bytes the last change on DOC overwrote, exactly, keep
what they replaced for redo, and return (layer-index target x y w h);
nil when nothing is left to undo."
  (let ((entry (first (the-object doc undo-entries))))
    (when entry
      (the-object doc (set-slot! :undo-entries (rest (the-object doc undo-entries))))
      (push-redo! doc (swap-entry! doc entry))
      (if (member (first entry) '(:mode :prompt)) entry (butlast entry)))))

(defun redo-stroke! (doc)
  "Put back what the last undo took away, exactly, and return
(layer-index target x y w h); nil when nothing is left to redo."
  (let ((entry (first (the-object doc redo-entries))))
    (when entry
      (the-object doc (set-slot! :redo-entries (rest (the-object doc redo-entries))))
      (push-undo! doc (swap-entry! doc entry) :keep-redo t)
      (if (member (first entry) '(:mode :prompt)) entry (butlast entry)))))
