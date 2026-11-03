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
;; Filters: Gaussian Blur, Unsharp Mask, Add Noise.  A filter is baked
;; into one raster layer's pixels, inside the selection when there is
;; one, with the whole layer on the undo list; the browser previews it
;; through a replacement texture of its own making (the filter passes
;; in viewer.js, from *filter-glsl*), which the main shader swaps in
;; for the layer's pixels.  Each filter here is the twin of those
;; passes: the blur separable with the same discrete kernel in
;; premultiplied linear light, edges clamped, rows then columns, the
;; intermediate in single floats as the GPU keeps it; the sharpening
;; the original plus amount times its difference from the blur; the
;; noise from the same integer hash of pixel, channel and seed, so the
;; preview and the bake draw the same numbers.
;;

(defun gaussian-kernel (sigma)
  "The normalized weights over -extent..extent, and the extent."
  (let* ((extent (max 1 (ceiling (* 3 sigma))))
         (weights (make-array (1+ (* 2 extent)) :element-type 'double-float))
         (sum 0d0))
    (loop for i from (- extent) to extent
          for w = (exp (- (/ (* i i) (* 2d0 sigma sigma))))
          do (setf (aref weights (+ i extent)) w)
             (incf sum w))
    (dotimes (k (length weights))
      (setf (aref weights k) (/ (aref weights k) sum)))
    (values weights extent)))

(defun parallel-ranges (count fn)
  "Call FN with START and END over [0, COUNT) in up to *render-threads*
slices, each in its own thread, and wait."
  (let* ((slices (max 1 (min *render-threads* count)))
         (step (ceiling count slices))
         (ranges (loop for start from 0 below count by step
                       collect (list start (min count (+ start step))))))
    (if (rest ranges)
        (mapc #'bt:join-thread
              (mapcar (lambda (range)
                        (bt:make-thread (lambda () (apply fn range)) :name "stratum filter"))
                      ranges))
        (apply fn (first ranges)))
    (values)))

(defun decode-premul (pixels w h)
  "PIXELS (RGBA8 straight) as premultiplied linear single floats."
  (let ((decode *srgb-decode*)
        (premul (make-array (* 4 w h) :element-type 'single-float)))
    (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
             (type (simple-array single-float (*)) premul)
             (type (simple-array double-float (256)) decode))
    (dotimes (i (* w h) premul)
      (let* ((o (* 4 i))
             (a (/ (float (aref pixels (+ o 3)) 1d0) 255d0)))
        (setf (aref premul o) (float (* a (aref decode (aref pixels o))) 1f0)
              (aref premul (+ o 1)) (float (* a (aref decode (aref pixels (+ o 1)))) 1f0)
              (aref premul (+ o 2)) (float (* a (aref decode (aref pixels (+ o 2)))) 1f0)
              (aref premul (+ o 3)) (float a 1f0))))))

(defun encode-premul (premul w h)
  "Premultiplied linear single floats back to RGBA8 straight."
  (let ((out (make-rgba w h)))
    (declare (type (simple-array single-float (*)) premul)
             (type (simple-array (unsigned-byte 8) (*)) out))
    (dotimes (i (* w h) out)
      (let* ((o (* 4 i))
             (a (float (aref premul (+ o 3)) 1d0)))
        (if (> a 0d0)
            (setf (aref out o) (unit->byte (linear->srgb (/ (float (aref premul o) 1d0) a)))
                  (aref out (+ o 1)) (unit->byte (linear->srgb (/ (float (aref premul (+ o 1)) 1d0) a)))
                  (aref out (+ o 2)) (unit->byte (linear->srgb (/ (float (aref premul (+ o 2)) 1d0) a)))
                  (aref out (+ o 3)) (unit->byte a))
            (setf (aref out o) 0 (aref out (+ o 1)) 0 (aref out (+ o 2)) 0 (aref out (+ o 3)) 0))))))

(defun gaussian-blur-premul (premul w h sigma)
  "PREMUL (W x H, premultiplied linear single floats) blurred by SIGMA
pixels, rows then columns: a fresh array of the same kind."
  (multiple-value-bind (weights extent) (gaussian-kernel (float sigma 1d0))
    (declare (type (simple-array double-float (*)) weights) (type fixnum extent))
    (let ((pass1 (make-array (* 4 w h) :element-type 'single-float))
          (pass2 (make-array (* 4 w h) :element-type 'single-float)))
      (declare (type (simple-array single-float (*)) premul pass1 pass2)
               (type fixnum w h))
      (parallel-ranges h
                       (lambda (start end)
                         (declare (type fixnum start end))
                         (loop for y of-type fixnum from start below end
                               do (dotimes (x w)
                                    (let ((r 0d0) (g 0d0) (b 0d0) (a 0d0))
                                      (declare (type double-float r g b a))
                                      (loop for i of-type fixnum from (- extent) to extent
                                            do (let* ((sx (max 0 (min (1- w) (+ x i))))
                                                      (so (* 4 (+ sx (* y w))))
                                                      (wt (aref weights (+ i extent))))
                                                 (incf r (* wt (aref premul so)))
                                                 (incf g (* wt (aref premul (+ so 1))))
                                                 (incf b (* wt (aref premul (+ so 2))))
                                                 (incf a (* wt (aref premul (+ so 3))))))
                                      (let ((o (* 4 (+ x (* y w)))))
                                        (setf (aref pass1 o) (float r 1f0)
                                              (aref pass1 (+ o 1)) (float g 1f0)
                                              (aref pass1 (+ o 2)) (float b 1f0)
                                              (aref pass1 (+ o 3)) (float a 1f0))))))))
      (parallel-ranges w
                       (lambda (start end)
                         (declare (type fixnum start end))
                         (loop for x of-type fixnum from start below end
                               do (dotimes (y h)
                                    (let ((r 0d0) (g 0d0) (b 0d0) (a 0d0))
                                      (declare (type double-float r g b a))
                                      (loop for i of-type fixnum from (- extent) to extent
                                            do (let* ((sy (max 0 (min (1- h) (+ y i))))
                                                      (so (* 4 (+ x (* sy w))))
                                                      (wt (aref weights (+ i extent))))
                                                 (incf r (* wt (aref pass1 so)))
                                                 (incf g (* wt (aref pass1 (+ so 1))))
                                                 (incf b (* wt (aref pass1 (+ so 2))))
                                                 (incf a (* wt (aref pass1 (+ so 3))))))
                                      (let ((o (* 4 (+ x (* y w)))))
                                        (setf (aref pass2 o) (float r 1f0)
                                              (aref pass2 (+ o 1)) (float g 1f0)
                                              (aref pass2 (+ o 2)) (float b 1f0)
                                              (aref pass2 (+ o 3)) (float a 1f0))))))))
      pass2)))

(defun gaussian-blur-rgba (pixels w h sigma)
  "PIXELS (W x H RGBA8, straight alpha) blurred by SIGMA pixels: a fresh
RGBA8 vector."
  (encode-premul (gaussian-blur-premul (decode-premul pixels w h) w h sigma) w h))

(defun unsharp-mask-rgba (pixels w h radius amount threshold)
  "PIXELS sharpened: each color channel moved away from its blur (RADIUS)
by AMOUNT (1 = 100%) of the difference, where the difference exceeds
THRESHOLD levels; alpha untouched."
  (let* ((premul (decode-premul pixels w h))
         (blur (gaussian-blur-premul premul w h (max 0.1d0 (float radius 1d0))))
         (out (make-array (* 4 w h) :element-type 'single-float))
         (amount (float amount 1d0))
         (limit (/ (float threshold 1d0) 255d0)))
    (declare (type (simple-array single-float (*)) premul blur out))
    (dotimes (i (* w h))
      (let* ((o (* 4 i))
             (dr (- (aref premul o) (aref blur o)))
             (dg (- (aref premul (+ o 1)) (aref blur (+ o 1))))
             (db (- (aref premul (+ o 2)) (aref blur (+ o 2))))
             (on (> (max (abs dr) (abs dg) (abs db)) limit)))
        (setf (aref out o) (if on (float (max 0d0 (+ (aref premul o) (* amount dr))) 1f0) (aref premul o))
              (aref out (+ o 1)) (if on (float (max 0d0 (+ (aref premul (+ o 1)) (* amount dg))) 1f0) (aref premul (+ o 1)))
              (aref out (+ o 2)) (if on (float (max 0d0 (+ (aref premul (+ o 2)) (* amount db))) 1f0) (aref premul (+ o 2)))
              (aref out (+ o 3)) (aref premul (+ o 3)))))
    (encode-premul out w h)))

(defun noise-hash (x y c seed)
  "A number in [0, 1) from pixel X, Y, channel C and SEED, the integer
hash the filter shader computes in 32-bit unsigned arithmetic."
  (let* ((m #xffffffff)
         (h (logand (+ (* x 374761393) (* y 668265263) (* c 1274126177) (* seed 2246822519)) m)))
    (setf h (logand (* (logxor h (ash h -13)) 1274126177) m))
    (setf h (logxor h (ash h -16)))
    (/ (float (logand h #xffffff) 1d0) 16777216d0)))

(defun add-noise-rgba (pixels w h amount monochrome seed)
  "PIXELS with uniform noise of AMOUNT (1 = a full 255 levels either
way) added to each encoded color channel, one draw per pixel when
MONOCHROME, else one per channel; alpha untouched."
  (let ((out (copy-seq pixels))
        (scale (* 255d0 (float amount 1d0))))
    (declare (type (simple-array (unsigned-byte 8) (*)) out))
    (dotimes (y h out)
      (dotimes (x w)
        (let ((o (* 4 (+ x (* y w)))))
          (dotimes (c 3)
            (let* ((r (noise-hash x y (if monochrome 0 c) seed))
                   (v (+ (aref pixels (+ o c)) (* scale (- (* 2d0 r) 1d0)))))
              (setf (aref out (+ o c)) (unit->byte (/ v 255d0))))))))))

(defun blend-by-selection! (pixels filtered width height selection)
  "PIXELS replaced by FILTERED where SELECTION covers, blended by its
coverage in premultiplied linear light; everywhere without one."
  (if (null selection)
      (replace pixels filtered)
      (let ((mask (getf selection :mask))
            (decode *srgb-decode*))
        (declare (type (simple-array (unsigned-byte 8) (*)) pixels filtered mask)
                 (type (simple-array double-float (256)) decode))
        (dotimes (i (* width height) pixels)
          (let ((s (/ (float (aref mask i) 1d0) 255d0)))
            (when (> s 0d0)
              (let* ((o (* 4 i))
                     (a0 (/ (float (aref pixels (+ o 3)) 1d0) 255d0))
                     (a1 (/ (float (aref filtered (+ o 3)) 1d0) 255d0))
                     (a (+ (* a0 (- 1d0 s)) (* a1 s))))
                (flet ((channel (k)
                         (/ (+ (* a0 (- 1d0 s) (aref decode (aref pixels (+ o k))))
                               (* a1 s (aref decode (aref filtered (+ o k)))))
                            a)))
                  (if (> a 0d0)
                      (setf (aref pixels o) (unit->byte (linear->srgb (channel 0)))
                            (aref pixels (+ o 1)) (unit->byte (linear->srgb (channel 1)))
                            (aref pixels (+ o 2)) (unit->byte (linear->srgb (channel 2)))
                            (aref pixels (+ o 3)) (unit->byte a))
                      (setf (aref pixels o) 0 (aref pixels (+ o 1)) 0
                            (aref pixels (+ o 2)) 0 (aref pixels (+ o 3)) 0))))))))))

(defun filter-layer! (layer kind &key settings)
  "Bake the filter KIND into raster LAYER inside the selection, with
SETTINGS a plist: :gaussian-blur (:radius), :unsharp-mask (:radius
:amount in percent :threshold in levels), :add-noise (:amount in
percent, :monochrome, :seed).  The whole layer on the undo list;
returns it as the rectangle."
  (unless (eql (the-object layer kind) :raster)
    (error "~a is not a raster layer." (the-object layer layer-name)))
  (let* ((doc (the-object layer parent))
         (w (the-object doc doc-width))
         (h (the-object doc doc-height))
         (pixels (the-object layer (ensure-own-pixels!))))
    (flet ((setting (key default) (let ((v (getf settings key))) (if v v default))))
      (let ((filtered
              (ecase kind
                (:gaussian-blur (gaussian-blur-rgba pixels w h (max 0.1d0 (float (setting :radius 1) 1d0))))
                (:unsharp-mask (unsharp-mask-rgba pixels w h (setting :radius 2)
                                                  (/ (float (setting :amount 100) 1d0) 100d0)
                                                  (setting :threshold 0)))
                (:add-noise (add-noise-rgba pixels w h (/ (float (setting :amount 10) 1d0) 100d0)
                                            (let ((m (setting :monochrome nil))) (and m (not (eql m 0))))
                                            (round (setting :seed 1)))))))
        (push-undo! doc (list (the-object layer index) :layer 0 0 w h (copy-seq pixels)))
        (blend-by-selection! pixels filtered w h (the-object doc selection))
        (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))
        (list 0 0 w h)))))
