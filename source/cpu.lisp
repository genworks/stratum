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
;; The CPU backend: the same plan glsl.lisp turns into a shader becomes
;; a lambda form here, unrolled one block per layer and handed to
;; COMPILE.  It is the reference the GPU is checked against, and the
;; path for export.  Each block mirrors its GLSL counterpart operation
;; for operation; change one and change the other.
;;

(declaim (inline lut-lookup))
(defun lut-lookup (luts row x)
  "Linear interpolation in the 256-entry table ROW of LUTS, as the
shader's lut() does it."
  (declare (type (simple-array double-float (*)) luts)
           (type fixnum row) (type double-float x)
           (optimize (speed 3) (safety 0)))
  (let* ((q (* (min 1.0 (max 0.0 x)) 255.0))
         (i (truncate q))
         (f (- q (float i 1.0)))
         (j (min (1+ i) 255))
         (base (* row 256)))
    (declare (type double-float q f) (type fixnum i j base))
    (+ (* (aref luts (+ base i)) (- 1.0 f))
       (* (aref luts (+ base j)) f))))

(defun tex-var (i) (intern (format nil "TEX~d" i) :stratum))
(defun mask-var (i) (intern (format nil "MASK~d" i) :stratum))

(defun lisp-mask (mask)
  (if mask
      `(/ (float (aref ,(mask-var mask) idx) 1.0) 255.0)
      1.0))

(defun lisp-blend (mode)
  "Forms over backdrop B? and source S? for each channel."
  (flet ((each (fn) (mapcar fn '(br bg bb) '(sr sg sb))))
    (ecase mode
      (:normal (each (lambda (b s) (declare (ignore b)) s)))
      (:multiply (each (lambda (b s) `(* ,b ,s))))
      (:screen (each (lambda (b s) `(- (+ ,b ,s) (* ,b ,s)))))
      (:add (each (lambda (b s) `(min 1.0 (+ ,b ,s))))))))

(defun lisp-entry (entry)
  (destructuring-bind (kind &key tex blend mask param lut) entry
    (ecase kind
      (:raster
       (let ((tv (tex-var tex)))
         (destructuring-bind (mr mg mb) (lisp-blend blend)
           `(let* ((p (* 4 idx))
                   (sr (aref decode (aref ,tv p)))
                   (sg (aref decode (aref ,tv (+ p 1))))
                   (sb (aref decode (aref ,tv (+ p 2))))
                   (a (* (/ (float (aref ,tv (+ p 3)) 1.0) 255.0)
                         (aref opacity ,param) ,(lisp-mask mask)))
                   (br (if (> ao 0.0) (/ kr ao) 0.0))
                   (bg (if (> ao 0.0) (/ kg ao) 0.0))
                   (bb (if (> ao 0.0) (/ kb ao) 0.0)))
              (declare (type fixnum p)
                       (type double-float sr sg sb a br bg bb)
                       (ignorable br bg bb))
              (setf kr (+ (* (+ (* (- 1.0 ao) sr) (* ao ,mr)) a) (* kr (- 1.0 a)))
                    kg (+ (* (+ (* (- 1.0 ao) sg) (* ao ,mg)) a) (* kg (- 1.0 a)))
                    kb (+ (* (+ (* (- 1.0 ao) sb) (* ao ,mb)) a) (* kb (- 1.0 a)))
                    ao (+ a (* ao (- 1.0 a))))))))
      (:curves
       `(let* ((a (* (aref opacity ,param) ,(lisp-mask mask)))
               (nr (srgb->linear (lut-lookup luts ,lut (linear->srgb (if (> ao 0.0) (/ kr ao) 0.0)))))
               (ng (srgb->linear (lut-lookup luts ,(+ lut 1) (linear->srgb (if (> ao 0.0) (/ kg ao) 0.0)))))
               (nb (srgb->linear (lut-lookup luts ,(+ lut 2) (linear->srgb (if (> ao 0.0) (/ kb ao) 0.0))))))
          (declare (type double-float a nr ng nb))
          (setf kr (+ (* kr (- 1.0 a)) (* nr ao a))
                kg (+ (* kg (- 1.0 a)) (* ng ao a))
                kb (+ (* kb (- 1.0 a)) (* nb ao a))))))))

(defun plan->lisp (plan)
  "A lambda form compositing PLAN into OUT, sRGB RGBA8 straight alpha,
for the pixels from START below END.  Called with the raster and mask
pixel vectors, the curve tables, the per-layer opacities and the linear
background.  Pixels are independent, so disjoint ranges may run at once
(run-kernel)."
  (let ((tex-ids (remove-duplicates
                  (loop for e in plan when (eql (first e) :raster)
                        collect (getf (rest e) :tex))))
        (mask-ids (remove nil (remove-duplicates
                               (loop for e in plan collect (getf (rest e) :mask))))))
    `(lambda (rasters masks luts opacity bg-r bg-g bg-b out start end)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type simple-vector rasters masks)
                (type (simple-array double-float (*)) luts opacity)
                (type double-float bg-r bg-g bg-b)
                (type (simple-array (unsigned-byte 8) (*)) out)
                (type fixnum start end)
                (ignorable rasters masks luts opacity))
       (let ((decode *srgb-decode*)
             ,@(loop for i in tex-ids collect `(,(tex-var i) (svref rasters ,i)))
             ,@(loop for j in mask-ids collect `(,(mask-var j) (svref masks ,j))))
         (declare (type (simple-array double-float (256)) decode)
                  (ignorable decode)
                  ,@(when (or tex-ids mask-ids)
                      `((type (simple-array (unsigned-byte 8) (*))
                              ,@(mapcar #'tex-var tex-ids)
                              ,@(mapcar #'mask-var mask-ids)))))
         (loop for idx of-type fixnum from start below end do
           (let ((kr bg-r) (kg bg-g) (kb bg-b) (ao 1.0))
             (declare (type double-float kr kg kb ao))
             ,@(mapcar #'lisp-entry plan)
             (let ((o (* 4 idx)))
               (declare (type fixnum o))
               (setf (aref out o) (unit->byte (linear->srgb (if (> ao 0.0) (/ kr ao) 0.0)))
                     (aref out (+ o 1)) (unit->byte (linear->srgb (if (> ao 0.0) (/ kg ao) 0.0)))
                     (aref out (+ o 2)) (unit->byte (linear->srgb (if (> ao 0.0) (/ kb ao) 0.0)))
                     (aref out (+ o 3)) (unit->byte ao)))))
         out))))

(defparameter *render-threads* 8
  "Integer. How many threads a CPU render splits its pixels across; 1
renders in the calling thread.")

(defun run-kernel (kernel pixel-count &rest args)
  "Call KERNEL with ARGS and a START and END, over [0, PIXEL-COUNT) in up
to *render-threads* contiguous slices, each in its own thread, and wait
for them all.  The kernel reads only its inputs and writes only its own
slice of the output, so the slices need no locking."
  (let* ((slices (max 1 (min *render-threads* (ceiling pixel-count 65536))))
         (step (ceiling pixel-count slices))
         (ranges (loop for start from 0 below pixel-count by step
                       collect (list start (min pixel-count (+ start step))))))
    (if (rest ranges)
        (mapc #'bt:join-thread
              (mapcar (lambda (range)
                        (bt:make-thread (lambda () (apply kernel (append args range)))
                                        :name "stratum render"))
                      ranges))
        (apply kernel (append args (first ranges))))
    (values)))
