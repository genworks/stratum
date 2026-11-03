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
;; Image > Crop, Image Size and Canvas Size.  A document's width and
;; height are inputs of its whole tree, so a change of size makes a NEW
;; document under the same name: every layer is first materialized
;; (its pixels and its mask, as they are now, carried in its spec),
;; each is cropped, resampled or placed on the new canvas, and the new
;; document is built from the result.  Nothing of the old document
;; survives but the specs, which is what spec-now is for.  Resampling
;; is bilinear in premultiplied linear light, so edges and transparency
;; interpolate without fringes; masks interpolate on their own scale.
;;

(defun place-rgba (pixels w h nw nh ox oy &optional (channels 4))
  "PIXELS (W x H, CHANNELS bytes a pixel) on a transparent NW x NH
canvas with its top-left at OX, OY (negative crops): Canvas Size and
Crop."
  (let ((out (make-array (* nw nh channels) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (row h out)
      (let ((y (+ oy row)))
        (when (and (<= 0 y) (< y nh))
          (let ((x0 (max 0 ox)) (x1 (min nw (+ ox w))))
            (when (< x0 x1)
              (replace out pixels
                       :start1 (* channels (+ x0 (* nw y)))
                       :start2 (* channels (+ (- x0 ox) (* w row)))
                       :end2 (* channels (+ (- x1 ox) (* w row)))))))))))

(defun resample-rgba (pixels w h nw nh)
  "PIXELS (W x H RGBA8, straight alpha) resampled to NW x NH, bilinear
in premultiplied linear light.  The same size comes back unchanged."
  (if (and (= w nw) (= h nh))
      (copy-seq pixels)
      (let ((out (make-rgba nw nh))
            (decode *srgb-decode*)
            (sx (/ (float w 1d0) nw))
            (sy (/ (float h 1d0) nh)))
        (declare (type (simple-array (unsigned-byte 8) (*)) pixels out)
                 (type (simple-array double-float (256)) decode))
        (flet ((premul (x y k)
                 ;; linear premultiplied channel K (3 = alpha) of pixel X, Y
                 (let* ((o (* 4 (+ x (* y w))))
                        (a (/ (float (aref pixels (+ o 3)) 1d0) 255d0)))
                   (if (= k 3) a (* a (aref decode (aref pixels (+ o k))))))))
          (dotimes (ny nh out)
            (let* ((fy (- (* (+ ny 0.5d0) sy) 0.5d0))
                   (y0 (max 0 (min (1- h) (floor fy))))
                   (y1 (min (1- h) (1+ y0)))
                   (ty (max 0d0 (min 1d0 (- fy y0)))))
              (dotimes (nx nw)
                (let* ((fx (- (* (+ nx 0.5d0) sx) 0.5d0))
                       (x0 (max 0 (min (1- w) (floor fx))))
                       (x1 (min (1- w) (1+ x0)))
                       (tx (max 0d0 (min 1d0 (- fx x0))))
                       (o (* 4 (+ nx (* ny nw)))))
                  (flet ((sample (k)
                           (+ (* (+ (* (premul x0 y0 k) (- 1d0 tx)) (* (premul x1 y0 k) tx)) (- 1d0 ty))
                              (* (+ (* (premul x0 y1 k) (- 1d0 tx)) (* (premul x1 y1 k) tx)) ty))))
                    (let ((a (sample 3)))
                      (if (> a 0d0)
                          (setf (aref out o) (unit->byte (linear->srgb (/ (sample 0) a)))
                                (aref out (+ o 1)) (unit->byte (linear->srgb (/ (sample 1) a)))
                                (aref out (+ o 2)) (unit->byte (linear->srgb (/ (sample 2) a)))
                                (aref out (+ o 3)) (unit->byte a))
                          (setf (aref out o) 0 (aref out (+ o 1)) 0
                                (aref out (+ o 2)) 0 (aref out (+ o 3)) 0))))))))))))

(defun resample-gray (mask w h nw nh)
  "MASK (W x H, one byte a pixel) resampled to NW x NH, bilinear."
  (if (and (= w nw) (= h nh))
      (copy-seq mask)
      (let ((out (make-gray nw nh))
            (sx (/ (float w 1d0) nw))
            (sy (/ (float h 1d0) nh)))
        (dotimes (ny nh out)
          (let* ((fy (- (* (+ ny 0.5d0) sy) 0.5d0))
                 (y0 (max 0 (min (1- h) (floor fy))))
                 (y1 (min (1- h) (1+ y0)))
                 (ty (max 0d0 (min 1d0 (- fy y0)))))
            (dotimes (nx nw)
              (let* ((fx (- (* (+ nx 0.5d0) sx) 0.5d0))
                     (x0 (max 0 (min (1- w) (floor fx))))
                     (x1 (min (1- w) (1+ x0)))
                     (tx (max 0d0 (min 1d0 (- fx x0)))))
                (flet ((at (x y) (float (aref mask (+ x (* y w))) 1d0)))
                  (setf (aref out (+ nx (* ny nw)))
                        (round (+ (* (+ (* (at x0 y0) (- 1d0 tx)) (* (at x1 y0) tx)) (- 1d0 ty))
                                  (* (+ (* (at x0 y1) (- 1d0 tx)) (* (at x1 y1) tx)) ty))))))))))))

(defun materialized-specs (doc)
  "DOC's layer specs with every raster layer's pixels and every mask's
bytes written in, as they are now."
  (mapcar (lambda (layer)
            (let ((spec (the-object layer spec-now)))
              (when (eql (the-object layer kind) :raster)
                (setf spec (plist-merge (list :pixels (the-object layer pixels) :source '(:solid)) spec)))
              (when (the-object layer has-mask?)
                (setf spec (plist-merge (list :mask-pixels (the-object layer mask-pixels)) spec)))
              spec))
          (the-object doc layer-list)))

(defun remake-document (doc nw nh pixel-fn mask-fn)
  "A new document of NW x NH under DOC's name, each layer's pixels
through PIXEL-FN and each mask through MASK-FN, both (bytes w h) -> bytes."
  (let ((w (the-object doc doc-width))
        (h (the-object doc doc-height)))
    (make-object 'image-document
                 :doc-name (the-object doc doc-name)
                 :doc-width nw
                 :doc-height nh
                 :background-srgb (the-object doc background-srgb)
                 :color-mode (the-object doc color-mode)
                 :layer-specs
                 (mapcar (lambda (spec)
                           (let ((spec (copy-list spec)))
                             (when (getf spec :pixels)
                               (setf (getf spec :pixels) (funcall pixel-fn (getf spec :pixels) w h)))
                             (when (getf spec :mask-pixels)
                               (setf (getf spec :mask-pixels) (funcall mask-fn (getf spec :mask-pixels) w h)))
                             spec))
                         (materialized-specs doc)))))

(defun crop-document (doc x y w h)
  "Image > Crop: DOC remade as the W x H rectangle at X, Y."
  (remake-document doc w h
                   (lambda (px ow oh) (place-rgba px ow oh w h (- x) (- y) 4))
                   (lambda (m ow oh) (place-rgba m ow oh w h (- x) (- y) 1))))

(defun resize-document (doc nw nh)
  "Image > Image Size: DOC remade at NW x NH, resampled."
  (remake-document doc nw nh
                   (lambda (px ow oh) (resample-rgba px ow oh nw nh))
                   (lambda (m ow oh) (resample-gray m ow oh nw nh))))

;;; Image > Image Rotation.  The quarter turns, the half turn and the
;;; flips move whole pixels, exactly; Arbitrary rotates by any angle
;;; onto a canvas grown to hold the turned picture, bilinear in
;;; premultiplied linear light like Image Size, the exposed corners
;;; transparent.  Angles are degrees, clockwise positive, as the screen
;;; turns.

(defun turned-size (w h turn)
  "(values nw nh) of a W x H picture after TURN."
  (ecase turn
    ((:cw :ccw) (values h w))
    ((:180 :flip-h :flip-v) (values w h))))

(defun turn-rgba (pixels w h turn &optional (channels 4))
  "PIXELS (W x H, CHANNELS bytes a pixel) turned by TURN, :cw, :ccw, :180,
:flip-h or :flip-v: a fresh buffer of the turned size, pixels moved
whole."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels) (type fixnum w h channels))
  (multiple-value-bind (nw nh) (turned-size w h turn)
    (let ((out (make-array (* nw nh channels) :element-type '(unsigned-byte 8))))
      (flet ((source (nx ny)
               (ecase turn
                 (:cw (values ny (- h 1 nx)))
                 (:ccw (values (- w 1 ny) nx))
                 (:180 (values (- w 1 nx) (- h 1 ny)))
                 (:flip-h (values (- w 1 nx) ny))
                 (:flip-v (values nx (- h 1 ny))))))
        (dotimes (ny nh out)
          (dotimes (nx nw)
            (multiple-value-bind (x y) (source nx ny)
              (let ((so (* channels (+ x (* y w))))
                    (o (* channels (+ nx (* ny nw)))))
                (dotimes (k channels)
                  (setf (aref out (+ o k)) (aref pixels (+ so k))))))))))))

(defun rotated-size (w h degrees)
  "(values nw nh): the canvas that holds a W x H picture turned by DEGREES."
  (let* ((a (* pi (/ (float degrees 1d0) 180d0)))
         (c (abs (cos a))) (s (abs (sin a))))
    (values (max 1 (ceiling (- (+ (* w c) (* h s)) 1d-9)))
            (max 1 (ceiling (- (+ (* w s) (* h c)) 1d-9))))))

(defun rotate-rgba (pixels w h degrees &optional (channels 4))
  "PIXELS (W x H, CHANNELS 4 for RGBA8 straight alpha or 1 for a mask)
turned by DEGREES clockwise about its center onto the canvas of
rotated-size, bilinear -- in premultiplied linear light for pixels,
on its own scale for a mask -- transparent (zero) beyond the picture."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels) (type fixnum w h channels))
  (multiple-value-bind (nw nh) (rotated-size w h degrees)
    (let* ((out (make-array (* nw nh channels) :element-type '(unsigned-byte 8) :initial-element 0))
           (decode *srgb-decode*)
           (a (* pi (/ (float degrees 1d0) 180d0)))
           (c (cos a)) (s (sin a))
           (cx (* 0.5d0 w)) (cy (* 0.5d0 h))
           (ncx (* 0.5d0 nw)) (ncy (* 0.5d0 nh)))
      (declare (type (simple-array double-float (256)) decode) (type double-float c s cx cy ncx ncy))
      (flet ((premul (x y k)
               (if (or (< x 0) (< y 0) (>= x w) (>= y h))
                   0d0
                   (let ((o (* channels (+ x (* y w)))))
                     (if (= channels 1)
                         (float (aref pixels o) 1d0)
                         (let ((alpha (/ (float (aref pixels (+ o 3)) 1d0) 255d0)))
                           (if (= k 3) alpha (* alpha (aref decode (aref pixels (+ o k)))))))))))
        (dotimes (ny nh out)
          (dotimes (nx nw)
            ;; the source point of this pixel's center: turned back by
            ;; -DEGREES about the centers
            (let* ((dx (- (+ nx 0.5d0) ncx)) (dy (- (+ ny 0.5d0) ncy))
                   (sx (- (+ cx (* c dx) (* s dy)) 0.5d0))
                   (sy (- (+ cy (- (* c dy) (* s dx))) 0.5d0))
                   (x0 (floor sx)) (y0 (floor sy))
                   (tx (- sx x0)) (ty (- sy y0))
                   (o (* channels (+ nx (* ny nw)))))
              (when (and (> (+ x0 1) -1) (> (+ y0 1) -1) (< x0 w) (< y0 h))
                (flet ((sample (k)
                         (+ (* (+ (* (premul x0 y0 k) (- 1d0 tx)) (* (premul (1+ x0) y0 k) tx)) (- 1d0 ty))
                            (* (+ (* (premul x0 (1+ y0) k) (- 1d0 tx)) (* (premul (1+ x0) (1+ y0) k) tx)) ty))))
                  (if (= channels 1)
                      (setf (aref out o) (max 0 (min 255 (round (sample 0)))))
                      (let ((alpha (sample 3)))
                        (when (> alpha 0d0)
                          (setf (aref out o) (unit->byte (linear->srgb (/ (sample 0) alpha)))
                                (aref out (+ o 1)) (unit->byte (linear->srgb (/ (sample 1) alpha)))
                                (aref out (+ o 2)) (unit->byte (linear->srgb (/ (sample 2) alpha)))
                                (aref out (+ o 3)) (unit->byte alpha))))))))))))))

(defun rotate-document (doc turn)
  "Image > Image Rotation: DOC remade turned by TURN, :cw, :ccw, :180,
:flip-h or :flip-v, every layer and mask moved whole."
  (multiple-value-bind (nw nh) (turned-size (the-object doc doc-width) (the-object doc doc-height) turn)
    (remake-document doc nw nh
                     (lambda (px ow oh) (turn-rgba px ow oh turn 4))
                     (lambda (m ow oh) (turn-rgba m ow oh turn 1)))))

(defun rotate-document-arbitrary (doc degrees)
  "Image > Image Rotation > Arbitrary: DOC remade turned by DEGREES
clockwise on a canvas grown to hold it; a multiple of 90 turns whole
pixels (rotate-document)."
  (let ((degrees (float degrees 1d0)))
    (multiple-value-bind (quarters rest) (round degrees 90)
      (if (< (abs rest) 1d-9)
          (ecase (mod quarters 4)
            (0 (remake-document doc (the-object doc doc-width) (the-object doc doc-height)
                                (lambda (px ow oh) (declare (ignore ow oh)) (copy-seq px))
                                (lambda (m ow oh) (declare (ignore ow oh)) (copy-seq m))))
            (1 (rotate-document doc :cw))
            (2 (rotate-document doc :180))
            (3 (rotate-document doc :ccw)))
          (multiple-value-bind (nw nh) (rotated-size (the-object doc doc-width) (the-object doc doc-height) degrees)
            (remake-document doc nw nh
                             (lambda (px ow oh) (rotate-rgba px ow oh degrees 4))
                             (lambda (m ow oh) (rotate-rgba m ow oh degrees 1))))))))

(defun canvas-size-document (doc nw nh &optional (ax 0.5d0) (ay 0.5d0))
  "Image > Canvas Size: DOC remade on an NW x NH canvas, the old picture
anchored at AX, AY (0 left or top, 1/2 center, 1 right or bottom),
padded with transparency or cropped."
  (let ((ox (round (* ax (- nw (the-object doc doc-width)))))
        (oy (round (* ay (- nh (the-object doc doc-height))))))
    (remake-document doc nw nh
                     (lambda (px ow oh) (place-rgba px ow oh nw nh ox oy 4))
                     (lambda (m ow oh) (place-rgba m ow oh nw nh ox oy 1)))))
