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
;; for operation; change one and change the other.  The shader's
;; live-stroke stage has no block here: a stroke reaches the CPU as
;; committed pixels, and its twin is composite-stroke! in brush.lisp.
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

(declaim (inline hue-channel hue-saturation))
(defun hue-channel (p q tt)
  (declare (type double-float p q tt))
  (let ((tt (- tt (ffloor tt))))
    (cond ((< tt (/ 1.0 6.0)) (+ p (* (- q p) 6.0 tt)))
          ((< tt 0.5) q)
          ((< tt (/ 2.0 3.0)) (+ p (* (- q p) (- (/ 2.0 3.0) tt) 6.0)))
          (t p))))

(defun hue-saturation (r g b k0 k1 k2)
  "R G B (encoded, in [0,1]) through HSL: hue turned by K0 (turns),
saturation scaled by 1 + K1, lightness toward white or black by K2, as
the shader's hue_saturation() does it."
  (declare (type double-float r g b k0 k1 k2)
           (optimize (speed 3) (safety 0)))
  (let* ((mx (max r g b))
         (mn (min r g b))
         (l (* (+ mx mn) 0.5))
         (d (- mx mn))
         (h 0.0)
         (s 0.0))
    (declare (type double-float mx mn l d h s))
    (when (> d 0.0)
      (setf s (if (> l 0.5) (/ d (- 2.0 mx mn)) (/ d (+ mx mn)))
            h (/ (cond ((= mx r) (+ (/ (- g b) d) (if (< g b) 6.0 0.0)))
                       ((= mx g) (+ (/ (- b r) d) 2.0))
                       (t (+ (/ (- r g) d) 4.0)))
                 6.0)))
    (setf h (+ h k0)
          h (- h (ffloor h))
          s (min 1.0 (max 0.0 (* s (+ 1.0 k1)))))
    (multiple-value-bind (nr ng nb)
        (if (<= s 0.0)
            (values l l l)
            (let* ((q (if (< l 0.5) (* l (+ 1.0 s)) (- (+ l s) (* l s))))
                   (p (- (* 2.0 l) q)))
              (values (hue-channel p q (+ h (/ 1.0 3.0)))
                      (hue-channel p q h)
                      (hue-channel p q (- h (/ 1.0 3.0))))))
      (declare (type double-float nr ng nb))
      (if (> k2 0.0)
          (values (+ nr (* (- 1.0 nr) k2)) (+ ng (* (- 1.0 ng) k2)) (+ nb (* (- 1.0 nb) k2)))
          (values (* nr (+ 1.0 k2)) (* ng (+ 1.0 k2)) (* nb (+ 1.0 k2)))))))

(defun tex-var (i) (intern (format nil "TEX~d" i) :stratum))
(defun mask-var (i) (intern (format nil "MASK~d" i) :stratum))

(defun lisp-mask (mask)
  (if mask
      `(/ (float (aref ,(mask-var mask) idx) 1.0) 255.0)
      1.0))

(declaim (inline color-blend))
(defun color-blend (br bg bb sr sg sb)
  "The Color blend on linear light: the source's hue and saturation at
the backdrop's luminance, three values (blend_color in the shader is
its twin)."
  (declare (type double-float br bg bb sr sg sb)
           (optimize (speed 3) (safety 0)))
  (flet ((luminance (r g b) (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))
    (declare (inline luminance))
    (let* ((l (luminance br bg bb))
           (d (- l (luminance sr sg sb)))
           (r (+ sr d))
           (g (+ sg d))
           (b (+ sb d))
           (n (min r g b)))
      (declare (type double-float l d r g b n))
      (when (< n 0.0)
        (let ((k (/ l (- l n))))
          (setf r (+ l (* (- r l) k))
                g (+ l (* (- g l) k))
                b (+ l (* (- b l) k)))))
      (let ((x (max r g b)))
        (when (> x 1.0)
          (let ((k (/ (- 1.0 l) (- x l))))
            (setf r (+ l (* (- r l) k))
                  g (+ l (* (- g l) k))
                  b (+ l (* (- b l) k))))))
      (values r g b))))

(defun lisp-blend (mode)
  "Forms over backdrop B? and source S? for each channel; for :color,
which no channel decides alone, the three values of color-blend, which
lisp-entry binds."
  (flet ((each (fn) (mapcar fn '(br bg bb) '(sr sg sb))))
    (ecase mode
      (:normal (each (lambda (b s) (declare (ignore b)) s)))
      (:multiply (each (lambda (b s) `(* ,b ,s))))
      (:screen (each (lambda (b s) `(- (+ ,b ,s) (* ,b ,s)))))
      (:add (each (lambda (b s) `(min 1.0 (+ ,b ,s)))))
      (:color '(cr cg cb)))))

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
              ,(let ((store `(setf kr (+ (* (+ (* (- 1.0 ao) sr) (* ao ,mr)) a) (* kr (- 1.0 a)))
                                   kg (+ (* (+ (* (- 1.0 ao) sg) (* ao ,mg)) a) (* kg (- 1.0 a)))
                                   kb (+ (* (+ (* (- 1.0 ao) sb) (* ao ,mb)) a) (* kb (- 1.0 a)))
                                   ao (+ a (* ao (- 1.0 a))))))
                 (if (eql blend :color)
                     `(multiple-value-bind (cr cg cb) (color-blend br bg bb sr sg sb)
                        (declare (type double-float cr cg cb))
                        ,store)
                     store))))))
      (:hue-saturation
       `(let* ((a (* (aref opacity ,param) ,(lisp-mask mask))))
          (declare (type double-float a))
          (multiple-value-bind (hr hg hb)
              (hue-saturation (linear->srgb (if (> ao 0.0) (/ kr ao) 0.0))
                              (linear->srgb (if (> ao 0.0) (/ kg ao) 0.0))
                              (linear->srgb (if (> ao 0.0) (/ kb ao) 0.0))
                              (aref params ,(* 4 param))
                              (aref params ,(+ 1 (* 4 param)))
                              (aref params ,(+ 2 (* 4 param))))
            (declare (type double-float hr hg hb))
            (let ((nr (srgb->linear hr)) (ng (srgb->linear hg)) (nb (srgb->linear hb)))
              (declare (type double-float nr ng nb))
              (setf kr (+ (* kr (- 1.0 a)) (* nr ao a))
                    kg (+ (* kg (- 1.0 a)) (* ng ao a))
                    kb (+ (* kb (- 1.0 a)) (* nb ao a)))))))
      ((:curves :levels)
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
pixel vectors, the curve and levels tables, the per-layer opacities and
four-number parameters, the linear background and its alpha (1 for a
document as shown, 0 to composite a stack over transparency, as a merge
does).  Pixels are independent, so disjoint ranges may run at once
(run-kernel)."
  (let ((tex-ids (remove-duplicates
                  (loop for e in plan when (eql (first e) :raster)
                        collect (getf (rest e) :tex))))
        (mask-ids (remove nil (remove-duplicates
                               (loop for e in plan collect (getf (rest e) :mask))))))
    `(lambda (rasters masks luts opacity params bg-r bg-g bg-b bg-a out start end)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type simple-vector rasters masks)
                (type (simple-array double-float (*)) luts opacity params)
                (type double-float bg-r bg-g bg-b bg-a)
                (type (simple-array (unsigned-byte 8) (*)) out)
                (type fixnum start end)
                (ignorable rasters masks luts opacity params))
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
           (let ((kr (* bg-r bg-a)) (kg (* bg-g bg-a)) (kb (* bg-b bg-a)) (ao bg-a))
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
