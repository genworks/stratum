;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; Select Subject (the API's): a person or thing selected from a box
;; drawn roughly around it, by Segment Anything 2.1 (Meta, Apache-2.0),
;; the Hiera-Tiny model as exported to ONNX by samexporter.  A model that
;; knows what a person is needs no outline: a box clipped into a head or
;; short of an arm still gives the whole person, where an outline drawn
;; by eye loses what it misses and color alone cannot always bring it
;; back.  The encoder reads the whole picture stretched to 1024 x 1024
;; (two seconds or so on the CPU) and its answer is kept, so more boxes
;; on the same picture cost a decoder run each, under a tenth of a
;; second.  The decoder's three candidate masks come at 256 x 256; the
;; one it scores best is stretched to the picture, cleared of specks and
;; small holes, and its blocky edge put on the subject's own by
;; refined-mask.
;;

(defparameter *subject-model-directory*
  (or (uiop:getenv "STRATUM_SUBJECT_MODEL") "/projects/.models/sam2.1-hiera-tiny/")
  "Where the encoder and decoder ONNX files are.")

(defparameter *subject-encoder-file* "sam2.1_hiera_tiny.encoder.onnx")
(defparameter *subject-decoder-file* "sam2.1_hiera_tiny.decoder.onnx")

(defparameter *subject-side* 1024 "The encoder's picture is this square.")
(defparameter *subject-mask-side* 256 "The decoder's masks are this square.")

(defparameter *subject-specks* 0.01d0
  "Pieces of the mask, and holes in it, smaller than this share of the
subject are taken away or filled.")

(defvar *subject-model* :unknown
  "The (encoder . decoder) sessions, nil when there is no model, or
:unknown before the first asking.")

(defvar *subject-encodings* '()
  "The encoder's answers for the last pictures it read, newest first:
((w h hash) . tensors), by the pixels' hash rather than the array, so a
picture since painted over is not held alive by being remembered.")

(defparameter *subject-encodings-kept* 2)

(defvar *subject-lock* (bt:make-lock "stratum subject")
  "Held while the model loads and runs: one picture at a time, and the
kept answers with it.")

(defun subject-model? ()
  "Whether this server can select a subject by model."
  (bt:with-lock-held (*subject-lock*)
    (when (eq *subject-model* :unknown)
      (setf *subject-model*
            (let ((encoder (merge-pathnames *subject-encoder-file* *subject-model-directory*))
                  (decoder (merge-pathnames *subject-decoder-file* *subject-model-directory*)))
              (and (probe-file encoder) (probe-file decoder) (onnx?)
                   (handler-case (cons (onnx-session encoder) (onnx-session decoder))
                     (error () nil))))))
    *subject-model*))

(defun subject-input (pixels w h)
  "PIXELS (W x H, RGBA) as the encoder takes them: stretched bilinearly to
the square, each channel less ImageNet's mean and over its deviation,
one plane after another; foreign floats, the caller's to free."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels) (type fixnum w h))
  (let* ((side *subject-side*)
         (buffer (cffi:foreign-alloc :float :count (* 3 side side)))
         (means #(0.485d0 0.456d0 0.406d0))
         (deviations #(0.229d0 0.224d0 0.225d0))
         (sx (/ w (float side 1d0)))
         (sy (/ h (float side 1d0))))
    (declare (type fixnum side) (type double-float sx sy))
    (dotimes (y side buffer)
      (let* ((fy (max 0d0 (min (- h 1d0) (- (* (+ y 0.5d0) sy) 0.5d0))))
             (y0 (floor fy)) (y1 (min (1- h) (1+ y0))) (ty (- fy y0)))
        (declare (type double-float fy ty) (type fixnum y0 y1))
        (dotimes (x side)
          (let* ((fx (max 0d0 (min (- w 1d0) (- (* (+ x 0.5d0) sx) 0.5d0))))
                 (x0 (floor fx)) (x1 (min (1- w) (1+ x0))) (tx (- fx x0)))
            (declare (type double-float fx tx) (type fixnum x0 x1))
            (dotimes (c 3)
              (flet ((at (xx yy) (aref pixels (+ c (* 4 (+ xx (* yy w)))))))
                (let ((v (/ (+ (* (- 1 ty) (+ (* (- 1 tx) (at x0 y0)) (* tx (at x1 y0))))
                               (* ty (+ (* (- 1 tx) (at x0 y1)) (* tx (at x1 y1)))))
                            255d0)))
                  (setf (cffi:mem-aref buffer :float (+ x (* side y) (* c side side)))
                        (float (/ (- v (aref means c)) (aref deviations c)) 1f0)))))))))))

(defun subject-encoding (pixels w h)
  "The encoder's answer for PIXELS: the three tensors the decoder takes,
kept for the next box on the same picture.  Called with *subject-lock*
held."
  (let ((key (list w h (pixels-hash pixels))))
    (or (cdr (assoc key *subject-encodings* :test #'equal))
      (let ((input (subject-input pixels w h))
            (side *subject-side*))
        (unwind-protect
             (let* ((tensor (onnx-tensor input (list 1 3 side side)))
                    (answer (unwind-protect
                                 (onnx-run (car *subject-model*) (list (cons "image" tensor))
                                           '("high_res_feats_0" "high_res_feats_1" "image_embed"))
                              (onnx-release tensor))))
               (push (cons key answer) *subject-encodings*)
               (when (> (length *subject-encodings*) *subject-encodings-kept*)
                 (mapc #'onnx-release (cdr (car (last *subject-encodings*))))
                 (setf *subject-encodings* (butlast *subject-encodings*)))
               answer)
          (cffi:foreign-free input))))))

(defun subject-logits (encoding prompts)
  "The decoder's best mask for PROMPTS, ((x y label) ...) in the
encoder's square (label 1 a point on the subject, 0 a point off it, 2
and 3 a box's corners): 256 x 256 single floats, positive inside."
  (destructuring-bind (feats-0 feats-1 embed) encoding
    (let* ((n (length prompts))
           (side *subject-mask-side*)
           (coords (cffi:foreign-alloc :float :count (* 2 n)))
           (kinds (cffi:foreign-alloc :float :count n))
           (prior (cffi:foreign-alloc :float :count (* side side) :initial-element 0f0))
           (has-prior (cffi:foreign-alloc :float :count 1 :initial-element 0f0))
           (tensors '()))
      (unwind-protect
           (progn
             (loop for (x y label) in prompts for i from 0
                   do (setf (cffi:mem-aref coords :float (* 2 i)) (float x 1f0)
                            (cffi:mem-aref coords :float (1+ (* 2 i))) (float y 1f0)
                            (cffi:mem-aref kinds :float i) (float label 1f0)))
             (setf tensors (list (onnx-tensor coords (list 1 n 2))
                                 (onnx-tensor kinds (list 1 n))
                                 (onnx-tensor prior (list 1 1 side side))
                                 (onnx-tensor has-prior (list 1))))
             (destructuring-bind (masks scores)
                 (onnx-run (cdr *subject-model*)
                           (list* (cons "image_embed" embed) (cons "high_res_feats_0" feats-0)
                                  (cons "high_res_feats_1" feats-1)
                                  (mapcar #'cons '("point_coords" "point_labels" "mask_input" "has_mask_input")
                                          tensors))
                           '("masks" "iou_predictions"))
               (unwind-protect
                    (multiple-value-bind (mask-data dimensions) (onnx-floats masks)
                      (unless (equal (last dimensions 2) (list side side))
                        (error "The subject model answered masks of ~a, not ~a square." dimensions side))
                      (let* ((score-data (onnx-floats scores))
                             (candidates (first (last dimensions 3)))
                             (best (loop with best = 0
                                         for k from 1 below candidates
                                         when (> (cffi:mem-aref score-data :float k)
                                                 (cffi:mem-aref score-data :float best))
                                           do (setf best k)
                                         finally (return best)))
                             (out (make-array (* side side) :element-type 'single-float)))
                        (dotimes (i (* side side) out)
                          (setf (aref out i) (cffi:mem-aref mask-data :float (+ i (* best side side)))))))
                 (onnx-release masks)
                 (onnx-release scores))))
        (mapc #'onnx-release tensors)
        (mapc #'cffi:foreign-free (list coords kinds prior has-prior))))))

(defun stretched-logits (logits w h)
  "LOGITS (the decoder's square) stretched bilinearly to W x H: 255 where
positive, 0 elsewhere."
  (declare (type (simple-array single-float (*)) logits) (type fixnum w h))
  (let* ((side *subject-mask-side*)
         (mask (make-gray w h))
         (sx (/ side (float w 1d0)))
         (sy (/ side (float h 1d0))))
    (declare (type fixnum side) (type double-float sx sy))
    (parallel-ranges
     h
     (lambda (start end)
       (loop for y of-type fixnum from start below end
             do (let* ((fy (max 0d0 (min (- side 1d0) (- (* (+ y 0.5d0) sy) 0.5d0))))
                       (y0 (floor fy)) (y1 (min (1- side) (1+ y0))) (ty (- fy y0)))
                  (declare (type double-float fy ty) (type fixnum y0 y1))
                  (dotimes (x w)
                    (let* ((fx (max 0d0 (min (- side 1d0) (- (* (+ x 0.5d0) sx) 0.5d0))))
                           (x0 (floor fx)) (x1 (min (1- side) (1+ x0))) (tx (- fx x0)))
                      (declare (type double-float fx tx) (type fixnum x0 x1))
                      (flet ((at (xx yy) (float (aref logits (+ xx (* yy side))) 1d0)))
                        (when (plusp (+ (* (- 1 ty) (+ (* (- 1 tx) (at x0 y0)) (* tx (at x1 y0))))
                                        (* ty (+ (* (- 1 tx) (at x0 y1)) (* tx (at x1 y1))))))
                          (setf (aref mask (+ x (* y w))) 255)))))))))
    mask))

(defun without-specks! (mask w h share)
  "MASK (W x H, 0 or 255) with every piece of it smaller than SHARE of
what it selects taken away, and every hole in it that is smaller and
does not reach the picture's edge filled.  MASK, changed."
  (let* ((side (map '(simple-array (unsigned-byte 8) (*)) (lambda (b) (if (>= b 128) 1 0)) mask))
         (limit (* share (count 1 side))))
    (flet ((settle (value keep?)
             (multiple-value-bind (labels count) (connected-pieces side w h value)
               (declare (type (simple-array fixnum (*)) labels))
               (let ((sizes (make-array count :element-type 'fixnum :initial-element 0))
                     (kept (make-array count :initial-element nil)))
                 (dotimes (y h)
                   (dotimes (x w)
                     (let ((piece (aref labels (+ x (* y w)))))
                       (unless (minusp piece)
                         (incf (aref sizes piece))
                         (when (funcall keep? x y) (setf (aref kept piece) t))))))
                 (dotimes (k (* w h))
                   (let ((piece (aref labels k)))
                     (unless (or (minusp piece) (aref kept piece) (>= (aref sizes piece) limit))
                       (setf (aref side k) (- 1 value)
                             (aref mask k) (* 255 (- 1 value))))))))))
      (settle 1 (constantly nil))
      (settle 0 (lambda (x y) (or (= x 0) (= y 0) (= x (1- w)) (= y (1- h))))))
    mask))

(defun subject-mask (pixels w h box &key points exclude)
  "The subject inside BOX (x y w h in PIXELS' own coordinates) as a mask,
W x H bytes, 0 or 255; POINTS ((x y) ...) on the subject and EXCLUDE
points off it steer the model.  Nil when the model finds nothing."
  (destructuring-bind (bx by bw bh) box
    (let* ((side (float *subject-side* 1d0))
           (sx (/ side w)) (sy (/ side h))
           (prompts (append (list (list (* bx sx) (* by sy) 2)
                                  (list (* (+ bx bw) sx) (* (+ by bh) sy) 3))
                            (loop for (x y) in points collect (list (* x sx) (* y sy) 1))
                            (loop for (x y) in exclude collect (list (* x sx) (* y sy) 0))))
           (logits (bt:with-lock-held (*subject-lock*)
                     (subject-logits (subject-encoding pixels w h) prompts)))
           (mask (without-specks! (stretched-logits logits w h) w h *subject-specks*))
           (rect (mask-bounds mask w h)))
      (when rect
        (refined-mask mask rect w h pixels (max 3 (round (max w h) 340)) 2)))))

(defun select-subject! (doc x y w h &key points exclude (mode :new))
  "The subject in the W x H box at X, Y of the composited picture
selected, by model, combined with the selection by MODE."
  (unless (subject-model?)
    (error "This server has no subject model: outline the subject with select-polygon and refine-selection instead."))
  (let ((dw (the-object doc doc-width))
        (dh (the-object doc doc-height)))
    (unless (and (realp x) (realp y) (realp w) (realp h) (> w 0) (> h 0)
                 (< x dw) (< y dh) (> (+ x w) 0) (> (+ y h) 0))
      (error "A subject's box is x y w h, inside the picture's ~d x ~d, not ~s." dw dh (list x y w h)))
    (dolist (list (list points exclude))
      (unless (and (listp list)
                   (every (lambda (p) (and (consp p) (= 2 (length p)) (every #'realp p))) list))
        (error "Points are ((x y) ...), not ~s." list)))
    (let ((mask (subject-mask (the-object doc (render-cpu)) dw dh (list x y w h)
                              :points points :exclude exclude)))
      (unless mask
        (error "The model found no subject in that box: draw it around the subject, with a little margin."))
      (set-selection-mask! doc mask mode))))

(defun subject-state ()
  "Whether this server selects subjects by model, for the model that plans."
  (if (subject-model?)
      "can select a subject by model with select-subject"
      "has no subject model: select-subject will fail, so outline subjects with select-polygon and refine-selection"))
