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

;;; One plane at a time.  The channels of a blur never meet, so a layer
;;; can go through a channel at a time with the numbers of
;;; gaussian-blur-premul to the last bit, at a quarter of the floats: a
;;; 5184 x 3456 photo is 72 MB a plane and 287 MB as four channels, and
;;; three of those at once did not fit the heap.

(defun make-plane (count)
  (make-array count :element-type 'single-float))

(defun premul-plane! (pixels channel plane count)
  "PLANE filled with CHANNEL of PIXELS (RGBA8 straight) as decode-premul
gives it, 3 being alpha."
  (let ((decode *srgb-decode*))
    (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
             (type (simple-array single-float (*)) plane)
             (type (simple-array double-float (256)) decode)
             (type fixnum channel count))
    (parallel-ranges
     count
     (lambda (start end)
       (declare (type fixnum start end))
       (loop for i of-type fixnum from start below end
             do (let* ((o (* 4 i))
                       (a (/ (float (aref pixels (+ o 3)) 1d0) 255d0)))
                  (setf (aref plane i)
                        (if (= channel 3)
                            (float a 1f0)
                            (float (* a (aref decode (aref pixels (+ o channel)))) 1f0)))))))
    plane))

(defparameter *blur-block* 1024
  "Columns the column pass works through at a time: their sums stay in
the first level of cache and the rows the kernel reaches in the second.")

(defun blur-rows! (source target w start end weights extent)
  "Rows START to END of SOURCE (W wide) through WEIGHTS into TARGET.
Past the kernel's reach of either edge no tap needs clamping, and four
pixels at a time keep four sums going: one alone waits on each addition
for the last.  Each pixel's taps are added in order, so the numbers do
not depend on how they are grouped."
  (declare (type (simple-array single-float (*)) source target)
           (type (simple-array double-float (*)) weights)
           (type fixnum w start end extent)
           (optimize (speed 3) (safety 0)))
  (let* ((taps (+ 1 (* 2 extent)))
         (left (min extent w))
         (right (max left (- w extent))))
    (declare (type fixnum taps left right))
    (flet ((clamped (x base)
             (declare (type fixnum x base))
             (let ((v 0d0))
               (declare (type double-float v))
               (dotimes (k taps)
                 (let ((sx (max 0 (min (1- w) (+ x k (- extent))))))
                   (declare (type fixnum sx))
                   (incf v (* (aref weights k) (aref source (+ base sx))))))
               (setf (aref target (+ base x)) (float v 1f0)))))
      (declare (inline clamped))
      (loop for y of-type fixnum from start below end
            do (let ((base (* y w))
                     (x left))
                 (declare (type fixnum base x))
                 (dotimes (i left) (clamped i base))
                 (loop while (<= (+ x 4) right)
                       do (let ((at (+ base x (- extent)))
                                (v0 0d0) (v1 0d0) (v2 0d0) (v3 0d0))
                            (declare (type fixnum at) (type double-float v0 v1 v2 v3))
                            (dotimes (k taps)
                              (let ((weight (aref weights k))
                                    (p (+ at k)))
                                (declare (type double-float weight) (type fixnum p))
                                (incf v0 (* weight (aref source p)))
                                (incf v1 (* weight (aref source (+ p 1))))
                                (incf v2 (* weight (aref source (+ p 2))))
                                (incf v3 (* weight (aref source (+ p 3))))))
                            (let ((o (+ base x)))
                              (declare (type fixnum o))
                              (setf (aref target o) (float v0 1f0)
                                    (aref target (+ o 1)) (float v1 1f0)
                                    (aref target (+ o 2)) (float v2 1f0)
                                    (aref target (+ o 3)) (float v3 1f0)))
                            (incf x 4)))
                 (loop while (< x right)
                       do (clamped x base)
                          (incf x))
                 (loop for i of-type fixnum from right below w
                       do (clamped i base)))))))

(defun blur-columns! (source target w h start end weights extent)
  "Rows START to END of TARGET, the columns of SOURCE (W x H) through
WEIGHTS.  A row at a time, tap by tap, into a row of sums, so memory is
read along its rows: down a column every read is another cache line."
  (declare (type (simple-array single-float (*)) source target)
           (type (simple-array double-float (*)) weights)
           (type fixnum w h start end extent)
           (optimize (speed 3) (safety 0)))
  (let* ((taps (+ 1 (* 2 extent)))
         (block-size (max 1 (min w *blur-block*)))
         (sums (make-array block-size :element-type 'double-float)))
    (declare (type fixnum taps block-size)
             (type (simple-array double-float (*)) sums))
    (loop for x0 of-type fixnum from 0 below w by block-size
          do (let ((n (min block-size (- w x0))))
               (declare (type fixnum n))
               (loop for y of-type fixnum from start below end
                     do (fill sums 0d0)
                        (dotimes (k taps)
                          (let* ((sy (max 0 (min (1- h) (+ y k (- extent)))))
                                 (from (+ x0 (* sy w)))
                                 (weight (aref weights k)))
                            (declare (type fixnum sy from) (type double-float weight))
                            (dotimes (i n)
                              (incf (aref sums i) (* weight (aref source (+ from i)))))))
                        (let ((to (+ x0 (* y w))))
                          (declare (type fixnum to))
                          (dotimes (i n)
                            (setf (aref target (+ to i)) (float (aref sums i) 1f0)))))))))

(defun constant-plane (plane)
  "The value every element of PLANE holds, or nil when they differ: the
alpha of an opaque photograph."
  (declare (type (simple-array single-float (*)) plane)
           (optimize (speed 3) (safety 0)))
  (when (plusp (length plane))
    (let ((value (aref plane 0)))
      (and (every (lambda (v) (declare (type single-float v)) (= v value)) plane) value))))

(defun blur-plane! (plane scratch w h sigma)
  "PLANE (W x H single floats) blurred in place by SIGMA pixels, rows
into SCRATCH and columns back, as gaussian-blur-premul blurs a channel,
to the last bit.  A plane of one value is worked out once: every pixel
of it adds the same taps of the same value."
  (multiple-value-bind (weights extent) (gaussian-kernel (float sigma 1d0))
    (declare (type (simple-array double-float (*)) weights) (type fixnum extent)
             (type (simple-array single-float (*)) plane scratch)
             (type fixnum w h))
    (let ((constant (constant-plane plane)))
      (if constant
          (flet ((through (value)
                   (let ((v 0d0))
                     (dotimes (k (length weights) (float v 1f0))
                       (incf v (* (aref weights k) value))))))
            (fill plane (through (through constant))))
          (progn
            (parallel-ranges h (lambda (start end) (blur-rows! plane scratch w start end weights extent)))
            (parallel-ranges h (lambda (start end) (blur-columns! scratch plane w h start end weights extent))))))
    plane))

(defun encode-channel! (out channel plane alpha count)
  "Channel CHANNEL of OUT from PLANE over ALPHA, as encode-premul does;
3 is alpha itself, and an empty pixel is zero in every channel."
  (declare (type (simple-array (unsigned-byte 8) (*)) out)
           (type (simple-array single-float (*)) plane alpha)
           (type fixnum channel count))
  (parallel-ranges
   count
   (lambda (start end)
     (declare (type fixnum start end))
     (loop for i of-type fixnum from start below end
           do (let ((a (float (aref alpha i) 1d0))
                    (o (+ channel (* 4 i))))
                (setf (aref out o)
                      (cond ((<= a 0d0) 0)
                            ((= channel 3) (unit->byte a))
                            (t (unit->byte (linear->srgb (/ (float (aref plane i) 1d0) a)))))))))))

(defun gaussian-blur-rgba (pixels w h sigma)
  "PIXELS (W x H RGBA8, straight alpha) blurred by SIGMA pixels: a fresh
RGBA8 vector."
  (let* ((count (* w h))
         (alpha (make-plane count))
         (plane (make-plane count))
         (scratch (make-plane count))
         (out (make-rgba w h)))
    (premul-plane! pixels 3 alpha count)
    (blur-plane! alpha scratch w h sigma)
    (dotimes (channel 3)
      (premul-plane! pixels channel plane count)
      (blur-plane! plane scratch w h sigma)
      (encode-channel! out channel plane alpha count))
    (encode-channel! out 3 alpha alpha count)
    out))

(defun unsharp-mask-rgba (pixels w h radius amount threshold)
  "PIXELS sharpened: each color channel moved away from its blur (RADIUS)
by AMOUNT (1 = 100%) of the difference, where the difference exceeds
THRESHOLD levels; alpha untouched.  A channel at a time, on two float
planes.  With a threshold, whether a pixel sharpens is a matter of all
three channels, so a first round of blurs marks the pixels; with none,
a pixel that no channel moves is left as it is by the arithmetic."
  (let* ((count (* w h))
         (sigma (max 0.1d0 (float radius 1d0)))
         (amount (float amount 1d0))
         (limit (/ (float threshold 1d0) 255d0))
         (plane (make-plane count))
         (scratch (make-plane count))
         (marks (when (> limit 0d0)
                  (make-array count :element-type '(unsigned-byte 8) :initial-element 0)))
         (out (make-rgba w h))
         (decode *srgb-decode*))
    (declare (type (simple-array single-float (*)) plane scratch)
             (type (simple-array (unsigned-byte 8) (*)) pixels out)
             (type (simple-array double-float (256)) decode)
             (type double-float amount limit)
             (type fixnum count))
    (flet ((each-pixel (fn)
             ;; FN of the pixel's index, its alpha and CHANNEL's premultiplied value
             (lambda (channel)
               (declare (type fixnum channel))
               (parallel-ranges
                count
                (lambda (start end)
                  (declare (type fixnum start end))
                  (loop for i of-type fixnum from start below end
                        do (let* ((o (* 4 i))
                                  (a0 (/ (float (aref pixels (+ o 3)) 1d0) 255d0)))
                             (funcall fn i channel (float a0 1f0)
                                      (float (* a0 (aref decode (aref pixels (+ o channel)))) 1f0)))))))))
      (when marks
        (let ((mark (each-pixel (lambda (i channel a p)
                                  (declare (ignore channel a) (type fixnum i) (type single-float p))
                                  (when (> (abs (- p (aref plane i))) limit)
                                    (setf (aref marks i) 1))))))
          (dotimes (channel 3)
            (blur-plane! (premul-plane! pixels channel plane count) scratch w h sigma)
            (funcall mark channel))))
      (let ((sharpen (each-pixel
                      (lambda (i channel a p)
                        (declare (type fixnum i channel) (type single-float a p))
                        (let* ((d (- p (aref plane i)))
                               (v (if (or (null marks) (= (aref marks i) 1))
                                      (float (max 0d0 (+ p (* amount d))) 1f0)
                                      p))
                               (alpha (float a 1d0))
                               (o (* 4 i)))
                          (setf (aref out (+ o channel))
                                (if (> alpha 0d0) (unit->byte (linear->srgb (/ (float v 1d0) alpha))) 0))
                          (when (= channel 2)
                            (setf (aref out (+ o 3)) (if (> alpha 0d0) (unit->byte alpha) 0))))))))
        (dotimes (channel 3)
          (blur-plane! (premul-plane! pixels channel plane count) scratch w h sigma)
          (funcall sharpen channel))))
    out))

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
