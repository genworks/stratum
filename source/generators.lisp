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
;; Procedural sources, so a document needs no files: a raster layer or
;; a mask names a generator and its arguments.  Raster pixels are RGBA8,
;; sRGB, straight alpha, row-major from the top; masks are one byte a
;; pixel.  Results are memoized on (spec width height), so a document
;; that repeats a source holds one copy of its pixels.
;;

(defvar *pixel-cache* (make-hash-table :test 'equal))

(defun clear-pixel-cache () (clrhash *pixel-cache*))

(defun make-rgba (w h)
  (make-array (* w h 4) :element-type '(unsigned-byte 8) :initial-element 0))

(defun make-gray (w h)
  (make-array (* w h) :element-type '(unsigned-byte 8) :initial-element 0))

(defun rgba-rect (pixels width height x y w h)
  "The W x H rectangle of RGBA PIXELS (WIDTH x HEIGHT) at X, Y, rows from
the top, as a fresh vector -- what the viewer re-uploads after an edit
to part of a layer.  The rectangle must lie inside the image."
  (unless (and (<= 0 x) (<= 0 y) (< 0 w) (< 0 h) (<= (+ x w) width) (<= (+ y h) height))
    (error "Rectangle ~a,~a ~ax~a is not inside ~ax~a." x y w h width height))
  (let ((out (make-rgba w h)))
    (dotimes (row h out)
      (replace out pixels
               :start1 (* 4 w row)
               :start2 (* 4 (+ x (* width (+ y row))))
               :end2 (* 4 (+ x w (* width (+ y row))))))))

(defun generate-pixels (spec w h)
  "RGBA8 pixels for the raster SPEC, (generator &key ...)."
  (let ((key (list :rgba spec w h)))
    (or (gethash key *pixel-cache*)
        (setf (gethash key *pixel-cache*)
              (destructuring-bind (kind &rest args) spec
                (ecase kind
                  (:gradient (apply #'gen-gradient w h args))
                  (:discs (apply #'gen-discs w h args))
                  (:clouds (apply #'gen-clouds w h args))
                  (:stripes (apply #'gen-stripes w h args))))))))

(defun generate-mask (spec w h)
  "One byte a pixel for the mask SPEC, (generator &key ...)."
  (let ((key (list :mask spec w h)))
    (or (gethash key *pixel-cache*)
        (setf (gethash key *pixel-cache*)
              (destructuring-bind (kind &rest args) spec
                (ecase kind
                  (:radial (apply #'mask-radial w h args))
                  (:linear (apply #'mask-linear w h args))))))))

(defun lcg (seed)
  "A deterministic source of doubles in [0,1): the same SEED draws the
same picture on every implementation."
  (let ((s (logand seed #xffffffff)))
    (lambda ()
      (setf s (logand (+ (* s 1664525) 1013904223) #xffffffff))
      (/ s 4294967296d0))))

(defun smoothstep (e0 e1 x)
  (let ((u (max 0d0 (min 1d0 (/ (- x e0) (- e1 e0))))))
    (* u u (- 3 (* 2 u)))))

;;; Directional ramp: TT runs 0 to 1 across the canvas along ANGLE
;;; (degrees, 0 = left to right, 90 = top to bottom).
(defun ramp-fn (w h angle)
  (let* ((th (* (float angle 1d0) (/ pi 180)))
         (dx (cos th)) (dy (sin th))
         (corners (list 0d0 (* w dx) (* h dy) (+ (* w dx) (* h dy))))
         (lo (reduce #'min corners))
         (span (max 1d-9 (- (reduce #'max corners) lo))))
    (lambda (x y)
      (/ (- (+ (* (+ x 0.5d0) dx) (* (+ y 0.5d0) dy)) lo) span))))

(defun gen-gradient (w h &key (from '(40 60 120 255)) (to '(250 170 90 255)) (angle 90))
  (let ((px (make-rgba w h))
        (ramp (ramp-fn w h angle))
        (a (map 'vector (lambda (v) (float v 1d0)) from))
        (b (map 'vector (lambda (v) (float v 1d0)) to)))
    (dotimes (y h px)
      (dotimes (x w)
        (let ((tt (max 0d0 (min 1d0 (funcall ramp x y))))
              (o (* 4 (+ x (* y w)))))
          (dotimes (c 4)
            (setf (aref px (+ o c))
                  (round (+ (* (- 1 tt) (aref a c)) (* tt (aref b c)))))))))))

(defun gen-discs (w h &key (count 20) (seed 1) (min-r 0.04) (max-r 0.16)
                        (softness 0.25) (alpha 230)
                        (palette '((230 80 70) (250 200 60) (70 160 220) (120 200 120))))
  "COUNT soft-edged discs over transparency, each painted over the last."
  (let ((px (make-rgba w h))
        (rnd (lcg seed))
        (m (min w h)))
    (dotimes (i count px)
      (let* ((cx (* w (funcall rnd)))
             (cy (* h (funcall rnd)))
             (r (* m (+ min-r (* (- max-r min-r) (funcall rnd)))))
             (inner (* r (- 1 softness)))
             (col (nth (mod i (length palette)) palette))
             (opacity (/ alpha 255d0)))
        (loop for y from (max 0 (floor (- cy r))) to (min (1- h) (ceiling (+ cy r)))
              do (loop for x from (max 0 (floor (- cx r))) to (min (1- w) (ceiling (+ cx r)))
                       do (let* ((d (sqrt (+ (expt (- (+ x 0.5d0) cx) 2)
                                             (expt (- (+ y 0.5d0) cy) 2))))
                                 (sa (* opacity (cond ((<= d inner) 1d0)
                                                      ((>= d r) 0d0)
                                                      (t (/ (- r d) (- r inner)))))))
                            (when (> sa 0)
                              (let* ((o (* 4 (+ x (* y w))))
                                     (da (/ (aref px (+ o 3)) 255d0))
                                     (oa (+ sa (* da (- 1 sa)))))
                                (dotimes (c 3)
                                  (setf (aref px (+ o c))
                                        (round (/ (+ (* (nth c col) sa)
                                                     (* (aref px (+ o c)) da (- 1 sa)))
                                                  oa))))
                                (setf (aref px (+ o 3)) (round (* 255 oa))))))))))))

(defun make-value-noise (seed)
  "Smooth value noise on the integer lattice, from a seeded permutation."
  (let ((rnd (lcg seed))
        (perm (make-array 512 :element-type 'fixnum))
        (vals (make-array 256 :element-type 'double-float)))
    (dotimes (i 256)
      (setf (aref perm i) i
            (aref vals i) (funcall rnd)))
    (loop for i from 255 downto 1
          do (rotatef (aref perm i) (aref perm (floor (* (funcall rnd) (1+ i))))))
    (dotimes (i 256) (setf (aref perm (+ i 256)) (aref perm i)))
    (flet ((at (ix iy) (aref vals (aref perm (+ ix (aref perm iy))))))
      (lambda (x y)
        (multiple-value-bind (ix fx) (floor x)
          (multiple-value-bind (iy fy) (floor y)
            (let* ((x0 (logand ix 255)) (x1 (logand (1+ ix) 255))
                   (y0 (logand iy 255)) (y1 (logand (1+ iy) 255))
                   (u (* fx fx (- 3 (* 2 fx))))
                   (v (* fy fy (- 3 (* 2 fy))))
                   (a (+ (at x0 y0) (* u (- (at x1 y0) (at x0 y0)))))
                   (b (+ (at x0 y1) (* u (- (at x1 y1) (at x0 y1))))))
              (+ a (* v (- b a))))))))))

(defun gen-clouds (w h &key (seed 7) (scale 0.004) (octaves 5)
                         (color '(255 255 255)) (threshold 0.5) (spread 0.18) (alpha 255))
  "Fractal value noise as coverage of COLOR.  The noise is evaluated on a
grid four pixels apart and interpolated, which a smooth field allows."
  (let* ((noise (make-value-noise seed))
         (step 4)
         (gw (+ 2 (ceiling w step)))
         (gh (+ 2 (ceiling h step)))
         (grid (make-array (list gh gw) :element-type 'double-float))
         (px (make-rgba w h))
         (norm (loop for o below octaves sum (expt 0.5d0 o))))
    (dotimes (gy gh)
      (dotimes (gx gw)
        (setf (aref grid gy gx)
              (/ (loop for o below octaves
                       for f = (* scale step (expt 2 o))
                       sum (* (expt 0.5d0 o) (funcall noise (* gx f) (* gy f))))
                 norm))))
    (dotimes (y h px)
      (multiple-value-bind (gy fy) (floor y step)
        (dotimes (x w)
          (multiple-value-bind (gx fx) (floor x step)
            (let* ((u (/ fx (float step 1d0)))
                   (v (/ fy (float step 1d0)))
                   (top (+ (aref grid gy gx) (* u (- (aref grid gy (1+ gx)) (aref grid gy gx)))))
                   (bot (+ (aref grid (1+ gy) gx) (* u (- (aref grid (1+ gy) (1+ gx)) (aref grid (1+ gy) gx)))))
                   (n (+ top (* v (- bot top))))
                   (cov (smoothstep (- threshold spread) (+ threshold spread) n))
                   (o (* 4 (+ x (* y w)))))
              (setf (aref px o) (first color)
                    (aref px (+ o 1)) (second color)
                    (aref px (+ o 2)) (third color)
                    (aref px (+ o 3)) (round (* alpha cov))))))))))

(defun gen-stripes (w h &key (period 48) (duty 0.5) (angle 30)
                          (color '(20 20 30)) (alpha 160))
  "Hard-edged parallel stripes: the aliasing test."
  (let* ((px (make-rgba w h))
         (th (* (float angle 1d0) (/ pi 180)))
         (dx (cos th)) (dy (sin th)))
    (dotimes (y h px)
      (dotimes (x w)
        (let ((phase (mod (/ (+ (* x dx) (* y dy)) period) 1d0))
              (o (* 4 (+ x (* y w)))))
          (when (< phase duty)
            (setf (aref px o) (first color)
                  (aref px (+ o 1)) (second color)
                  (aref px (+ o 2)) (third color)
                  (aref px (+ o 3)) alpha)))))))

(defun mask-radial (w h &key (cx 0.5) (cy 0.5) (inner 0.2) (outer 0.6) invert?)
  "White inside INNER, black beyond OUTER; radii as fractions of the
shorter side."
  (let ((m (make-gray w h))
        (r0 (* inner (min w h)))
        (r1 (* outer (min w h)))
        (x0 (* cx w)) (y0 (* cy h)))
    (dotimes (y h m)
      (dotimes (x w)
        (let* ((d (sqrt (+ (expt (- (+ x 0.5d0) x0) 2) (expt (- (+ y 0.5d0) y0) 2))))
               (v (- 1 (smoothstep r0 r1 d))))
          (setf (aref m (+ x (* y w))) (round (* 255 (if invert? (- 1 v) v)))))))))

(defun mask-linear (w h &key (angle 0) (from 0.3) (to 0.7) invert?)
  "Black before FROM and white after TO along ANGLE, as in gen-gradient."
  (let ((m (make-gray w h))
        (ramp (ramp-fn w h angle)))
    (dotimes (y h m)
      (dotimes (x w)
        (let ((v (smoothstep from to (funcall ramp x y))))
          (setf (aref m (+ x (* y w))) (round (* 255 (if invert? (- 1 v) v)))))))))
