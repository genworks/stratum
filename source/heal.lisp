;;;; -*- coding: utf-8 -*-
;;;; heal.lisp -- the Healing Brush: the Clone Stamp's sampling with the
;;;; destination's tone kept.
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

;;; A healing stroke paints, at each pixel, the source's texture on the
;;; destination's tone: the destination's blur plus what the source
;;; rises above its own blur,
;;;
;;;   paint = blur(destination) + source - blur(source)
;;;
;;; in linear light, clamped, with the source's alpha; so a blemish
;;; painted over from clean skin nearby takes the skin's grain and keeps
;;; the local shading, where the Clone Stamp would paste the source's
;;; color as it is.  The blur's sigma follows the brush (heal-sigma): a
;;; small brush keeps fine texture, a large one broad shading.  The
;;; viewer's preview samples a blur of the whole layer that the filter
;;; passes compute once at the stroke's start (heal_px in the shader);
;;; the server blurs just the two rectangles, each widened by the
;;; kernel's reach so the result is the same numbers (blur-rect-straight).

(defun heal-sigma (radius)
  "The healing blur's sigma for a brush of RADIUS: half of it, at least
one pixel.  The viewer's healSigma is the same rule."
  (max 1d0 (* 0.5d0 (float radius 1d0))))

(defun blur-rect-straight (pixels width height x y w h sigma)
  "The W x H rectangle at X, Y of PIXELS (RGBA8, WIDTH x HEIGHT) blurred
by SIGMA as gaussian-blur-rgba blurs the whole layer, as straight linear
single floats (r g b a), zero where the rectangle reaches beyond the
layer: the same numbers as the viewer's blur passes give at those
pixels.  Only the rectangle widened by the kernel's reach is blurred;
the edges clamp where the layer's do."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
           (type fixnum width height x y w h))
  (let* ((extent (nth-value 1 (gaussian-kernel (float sigma 1d0))))
         (ex0 (max 0 (- x extent))) (ey0 (max 0 (- y extent)))
         (ex1 (min width (+ x w extent))) (ey1 (min height (+ y h extent)))
         (ew (- ex1 ex0)) (eh (- ey1 ey0))
         (out (make-array (* 4 w h) :element-type 'single-float :initial-element 0f0)))
    (declare (type fixnum extent ex0 ey0 ex1 ey1 ew eh))
    (when (and (> ew 0) (> eh 0))
      (let* ((region (rgba-rect pixels width height ex0 ey0 ew eh 4))
             (blurred (gaussian-blur-premul (decode-premul region ew eh) ew eh sigma)))
        (declare (type (simple-array single-float (*)) blurred))
        (dotimes (row h)
          (let ((py (+ y row)))
            (when (and (<= ey0 py) (< py ey1))
              (dotimes (col w)
                (let ((px (+ x col)))
                  (when (and (<= ex0 px) (< px ex1))
                    (let* ((so (* 4 (+ (- px ex0) (* ew (- py ey0)))))
                           (o (* 4 (+ col (* w row))))
                           (a (aref blurred (+ so 3))))
                      (when (> a 0f0)
                        (setf (aref out o) (/ (aref blurred so) a)
                              (aref out (+ o 1)) (/ (aref blurred (+ so 1)) a)
                              (aref out (+ o 2)) (/ (aref blurred (+ so 2)) a)
                              (aref out (+ o 3)) a)))))))))))
    out))

(defun composite-heal-stroke! (pixels width coverage x y w h source blur-source blur-dest opacity)
  "Composite a Healing Brush stroke into PIXELS as composite-clone-stroke!
does, the paint at each pixel being BLUR-DEST + SOURCE - BLUR-SOURCE
in linear light, clamped, with SOURCE's alpha: SOURCE is the W x H RGBA8
source rectangle (clone-source-rect), BLUR-SOURCE and BLUR-DEST the
straight linear blurs of the source and destination rectangles
(blur-rect-straight).  The twin of heal_px in the shader."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels source)
           (type (simple-array single-float (*)) blur-source blur-dest)
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
                   (na (+ a keep)))
              (declare (type fixnum o) (type double-float la keep na))
              (flet ((channel (k)
                       (declare (type fixnum k))
                       (let ((paint (max 0.0 (min 1.0 (+ (float (aref blur-dest (+ s k)) 1.0)
                                                           (- (aref decode (aref source (+ s k)))
                                                              (float (aref blur-source (+ s k)) 1.0)))))))
                         (declare (type double-float paint))
                         (/ (+ (* paint a) (* (aref decode (aref pixels (+ o k))) keep)) na))))
                (setf (aref pixels o) (unit->byte (linear->srgb (channel 0)))
                      (aref pixels (+ o 1)) (unit->byte (linear->srgb (channel 1)))
                      (aref pixels (+ o 2)) (unit->byte (linear->srgb (channel 2)))
                      (aref pixels (+ o 3)) (unit->byte na))))))))
    pixels))

(defun heal-stroke! (pixels width height coverage x y w h dx dy radius opacity)
  "The Healing Brush's commit into PIXELS: the source rectangle DX, DY
away copied before the stroke lands, the two rectangles blurred by the
brush's sigma, and the stroke composited."
  (let* ((sx (+ x dx)) (sy (+ y dy))
         (sigma (heal-sigma radius))
         (source (clone-source-rect pixels width height sx sy w h))
         (blur-source (blur-rect-straight pixels width height sx sy w h sigma))
         (blur-dest (blur-rect-straight pixels width height x y w h sigma)))
    (composite-heal-stroke! pixels width coverage x y w h source blur-source blur-dest opacity)))
