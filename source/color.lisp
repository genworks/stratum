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
;; The sRGB transfer curve (IEC 61966-2-1).  Pixels are stored sRGB
;; encoded and every blend happens on linear light.  The GPU decodes
;; through SRGB8_ALPHA8 textures and encodes in the shader with the
;; formula linear->srgb uses here, so the two backends agree to within
;; a level.  The CPU side computes in double-float, Gendl's float
;; (gendl-asdf compiles every file with *read-default-float-format*
;; bound to double-float); the GPU's float32 is the coarser of the two.
;;

(declaim (inline srgb->linear linear->srgb unit->byte))

(defun srgb->linear (c)
  (declare (type double-float c))
  (if (<= c 0.04045)
      (/ c 12.92)
      (expt (/ (+ c 0.055) 1.055) 2.4)))

(defun linear->srgb (c)
  (declare (type double-float c))
  (let ((c (min 1.0 (max 0.0 c))))
    (if (<= c 0.0031308)
        (* c 12.92)
        (- (* 1.055 (expt c (/ 1.0 2.4))) 0.055))))

(defun unit->byte (v)
  (declare (type double-float v))
  (cl:the fixnum (truncate (+ 0.5 (* 255.0 (min 1.0 (max 0.0 v)))))))

(defparameter *srgb-decode*
  (let ((lut (make-array 256 :element-type 'double-float)))
    (dotimes (i 256 lut)
      (setf (aref lut i) (srgb->linear (/ (float i 1.0) 255.0)))))
  "Byte to linear light, for the CPU kernel's texel reads.")
