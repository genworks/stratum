;;;; -*- coding: utf-8 -*-
;;;; gradient.lisp -- the Gradient tool: a ramp between two colors laid
;;;; over a raster layer inside the selection.
;;;;
;;;; Copyright 2026 Genworks International
;;;;
;;;; This file is part of Stratum.
;;;;
;;;; Stratum is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; Stratum is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public
;;;; License along with Stratum.  If not, see
;;;; <https://www.gnu.org/licenses/>.

(in-package :stratum)

;;; The ramp is the classic one: the two colors interpolated on the
;;; encoded (sRGB) scale, where a half-way gray between black and white
;;; is 128, as every editor's users expect of it; the result is then
;;; composited over the layer in premultiplied linear light, so a ramp
;;; to transparency fades the way a brush stroke does.  gradient-rgba
;;; is the twin of pass 4 of *filter-glsl* (glsl.lisp): the viewer runs
;;; that pass for its live preview while the pointer drags, into the
;;; replacement stage of the main shader, and the server bakes this
;;; one when the pointer lifts.  Both read every pixel's center, on a
;;; frame where 0 is the start of the drag and 1 its end:
;;;
;;;   :linear     the projection onto the drag, clamped to [0, 1]
;;;   :reflected  the projection's distance from the start, clamped
;;;   :radial     the distance from the start, clamped
;;;   :diamond    the larger of |along| and |across|, clamped
;;;   :angle      the angle around the start, 0 along the drag, a full
;;;               turn = 1

(defparameter *gradient-shapes* '(:linear :reflected :radial :diamond :angle))

(declaim (inline gradient-position))
(defun gradient-position (shape u v)
  "Where a pixel at U along and V across the drag (both in units of the
drag's length) falls on the ramp of SHAPE, 0 to 1."
  (declare (type double-float u v))
  (ecase shape
    (:linear (min 1d0 (max 0d0 u)))
    (:reflected (min 1d0 (abs u)))
    (:radial (min 1d0 (sqrt (+ (* u u) (* v v)))))
    (:diamond (min 1d0 (max (abs u) (abs v))))
    (:angle (let ((a (atan v u)))
              (/ (if (< a 0d0) (+ a (* 2d0 pi)) a) (* 2d0 pi))))))

(defun gradient-rgba (pixels w h x0 y0 x1 y1 start end shape opacity)
  "PIXELS (RGBA8 straight, W x H) with the ramp from START at (X0, Y0)
to END at (X1, Y1) laid over every pixel at OPACITY: a fresh image of
the same kind.  START and END are (r g b a), sRGB 0-255 straight; the
ramp interpolates those encoded values and composites in premultiplied
linear light.  SHAPE is one of *gradient-shapes*.  A drag of no length
paints START everywhere."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
           (type fixnum w h))
  (let* ((out (make-rgba w h))
         (decode *srgb-decode*)
         (x0 (float x0 1d0)) (y0 (float y0 1d0))
         (dx (- (float x1 1d0) x0)) (dy (- (float y1 1d0) y0))
         (len2 (+ (* dx dx) (* dy dy)))
         (opacity (max 0d0 (min 1d0 (float opacity 1d0)))))
    (declare (type (simple-array (unsigned-byte 8) (*)) out)
             (type (simple-array double-float (256)) decode)
             (type double-float x0 y0 dx dy len2 opacity))
    (flet ((unit (c) (/ (max 0d0 (min 255d0 (float c 1d0))) 255d0)))
      (let ((r0 (unit (first start))) (g0 (unit (second start))) (b0 (unit (third start)))
            (a0 (unit (or (fourth start) 255)))
            (r1 (unit (first end))) (g1 (unit (second end))) (b1 (unit (third end)))
            (a1 (unit (or (fourth end) 255))))
        (declare (type double-float r0 g0 b0 a0 r1 g1 b1 a1))
        (dotimes (y h out)
          (dotimes (x w)
            (let* ((qx (- (+ (float x 1d0) 0.5d0) x0))
                   (qy (- (+ (float y 1d0) 0.5d0) y0))
                   (u (if (> len2 0d0) (/ (+ (* qx dx) (* qy dy)) len2) 0d0))
                   (v (if (> len2 0d0) (/ (- (* qy dx) (* qx dy)) len2) 0d0))
                   (tt (gradient-position shape u v))
                   (s (- 1d0 tt))
                   (sa (* opacity (+ (* a0 s) (* a1 tt))))
                   (o (* 4 (+ x (* y w))))
                   (da (/ (float (aref pixels (+ o 3)) 1d0) 255d0))
                   (keep (* da (- 1d0 sa)))
                   (na (+ sa keep)))
              (declare (type double-float qx qy u v tt s sa da keep na) (type fixnum o))
              (flet ((channel (k e0 e1)
                       (declare (type double-float e0 e1) (type fixnum k))
                       (/ (+ (* sa (srgb->linear (+ (* e0 s) (* e1 tt))))
                             (* keep (aref decode (aref pixels (+ o k)))))
                          na)))
                (if (> na 0d0)
                    (setf (aref out o) (unit->byte (linear->srgb (channel 0 r0 r1)))
                          (aref out (+ o 1)) (unit->byte (linear->srgb (channel 1 g0 g1)))
                          (aref out (+ o 2)) (unit->byte (linear->srgb (channel 2 b0 b1)))
                          (aref out (+ o 3)) (unit->byte na))
                    (setf (aref out o) 0 (aref out (+ o 1)) 0
                          (aref out (+ o 2)) 0 (aref out (+ o 3)) 0))))))))))

(defun gradient-layer! (layer x0 y0 x1 y1 &key (shape :linear) (start '(0 0 0 255)) (end '(255 255 255 255))
                                             (opacity 1))
  "The Gradient tool's commit: the ramp from START at (X0, Y0) to END
at (X1, Y1), of SHAPE, laid over raster LAYER at OPACITY inside the
document's selection, or everywhere without one.  The whole layer on
the undo list; returns it as the rectangle."
  (unless (eql (the-object layer kind) :raster)
    (error "~a is not a raster layer." (the-object layer layer-name)))
  (unless (member shape *gradient-shapes*)
    (error "~s is not a gradient shape." shape))
  (let* ((doc (the-object layer parent))
         (w (the-object doc doc-width))
         (h (the-object doc doc-height))
         (pixels (the-object layer (ensure-own-pixels!)))
         (ramped (gradient-rgba pixels w h x0 y0 x1 y1 start end shape opacity)))
    (push-undo! doc (list (the-object layer index) :layer 0 0 w h (copy-seq pixels)))
    (blend-by-selection! pixels ramped w h (the-object doc selection))
    (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))
    (list 0 0 w h)))
