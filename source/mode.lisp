;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;; Image > Mode.  A document is RGBA8 in either mode; Grayscale turns
;;; every raster layer to gray once (the luminance of each pixel in
;;; linear light, encoded, with its alpha kept) and from then on turns
;;; whatever is painted, filled, placed or pasted to gray on its way in
;;; (doc-color, gray-rgba!), so no color can get back into the picture
;;; until RGB Color is picked again, which changes nothing but the mode.
;;; A mode change is one step on the undo list: a (:mode from to . layer
;;; entries) entry that swap-entry! (brush.lisp) puts back whole.

(defun color-gray (color)
  "The gray (one byte) a COLOR (sRGB 0-255, r g b ...) turns into: its
luminance in linear light, encoded.  The viewer's grayOf is the same."
  (destructuring-bind (r g b &rest more) color
    (declare (ignore more))
    (let ((decode *srgb-decode*))
      (unit->byte (linear->srgb (+ (* 0.2126d0 (aref decode (round (max 0 (min 255 r)))))
                                   (* 0.7152d0 (aref decode (round (max 0 (min 255 g)))))
                                   (* 0.0722d0 (aref decode (round (max 0 (min 255 b)))))))))))

(defun doc-color (doc color)
  "COLOR as DOC takes it: itself in RGB, its gray (with the alpha COLOR
carries, if any) in Grayscale."
  (if (eql (the-object doc color-mode) :grayscale)
      (let ((g (color-gray color)))
        (if (fourth color) (list g g g (fourth color)) (list g g g)))
      color))

(defun gray-rgba! (pixels)
  "PIXELS (RGBA8 straight) turned to gray in place: each pixel's
luminance in linear light, encoded, its alpha kept."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
           (optimize (speed 3) (safety 1)))
  (let ((decode *srgb-decode*))
    (declare (type (simple-array double-float (256)) decode))
    (loop for o of-type fixnum from 0 below (length pixels) by 4
          do (let ((g (unit->byte (linear->srgb (+ (* 0.2126d0 (aref decode (aref pixels o)))
                                                   (* 0.7152d0 (aref decode (aref pixels (+ o 1))))
                                                   (* 0.0722d0 (aref decode (aref pixels (+ o 2)))))))))
               (setf (aref pixels o) g (aref pixels (+ o 1)) g (aref pixels (+ o 2)) g)))
    pixels))

(defun doc-pixels (doc pixels)
  "PIXELS as DOC takes them: themselves in RGB, grayed in Grayscale."
  (if (eql (the-object doc color-mode) :grayscale) (gray-rgba! pixels) pixels))

(defun set-color-mode! (doc mode)
  "Image > Mode: DOC to MODE, :rgb or :grayscale.  To Grayscale every
raster layer's pixels turn gray, each layer whole in one undo entry
with the mode change; to RGB nothing but the mode changes.  Returns
the undo entry made, (:mode from to . layer-entries), or nil when the
document was in MODE already."
  (unless (member mode '(:rgb :grayscale)) (error "~s is not an image mode." mode))
  (let ((from (the-object doc color-mode)))
    (unless (eql from mode)
      (let ((entries
              (when (eql mode :grayscale)
                (let ((w (the-object doc doc-width)) (h (the-object doc doc-height)))
                  (loop for layer in (the-object doc layer-list)
                        when (eql (the-object layer kind) :raster)
                          collect (let ((pixels (the-object layer (ensure-own-pixels!))))
                                    (prog1 (list (the-object layer index) :layer 0 0 w h (copy-seq pixels))
                                      (gray-rgba! pixels)
                                      (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version)))))))))))
        (the-object doc (set-slot! :color-mode mode))
        (let ((entry (list* :mode from mode entries)))
          (push-undo! doc entry)
          entry)))))
