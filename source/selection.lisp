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
;; The selection: an 8-bit coverage over the document, like a mask, with
;; its bounding rectangle, kept on the document's `selection' slot as
;; (:mask bytes :rect (x y w h)); nil when nothing is selected, which
;; paints everywhere.  A rectangle (the marquee), a polygon (the lasso),
;; All, Inverse and Deselect each make one.  A stroke's coverage and a
;; fill are multiplied by it, here with clip-coverage! and in the
;; browser with clipCoverage, the same arithmetic, so painting under a
;; selection stays bit-exact between the two.  The shader draws the
;; marching ants from the same bytes; the CPU render never sees them,
;; since a selection is not part of the picture.
;;

(defun rasterize-rect (w h x y rw rh)
  "A W x H mask, 255 inside the RW x RH rectangle at X, Y, clipped."
  (let ((mask (make-gray w h))
        (x0 (max 0 x)) (y0 (max 0 y))
        (x1 (min w (+ x rw))) (y1 (min h (+ y rh))))
    (loop for row from y0 below y1
          do (fill mask 255 :start (+ x0 (* row w)) :end (+ x1 (* row w))))
    mask))

(defun rasterize-polygon (w h points)
  "A W x H mask, 255 at every pixel whose center lies inside the polygon
POINTS ((x y) ...), even-odd, as the lasso draws it."
  (let* ((mask (make-gray w h))
         (pts (mapcar (lambda (p) (cons (float (first p) 1d0) (float (second p) 1d0))) points))
         (n (length pts)))
    (when (>= n 3)
      (let ((edges (loop for i below n
                         collect (cons (nth i pts) (nth (mod (1+ i) n) pts))))
            (crossings (make-array 64 :adjustable t :fill-pointer 0)))
        (dotimes (row h)
          (let ((yc (+ row 0.5d0)))
            (setf (fill-pointer crossings) 0)
            (dolist (e edges)
              (let ((x0 (car (car e))) (y0 (cdr (car e)))
                    (x1 (car (cdr e))) (y1 (cdr (cdr e))))
                (when (or (and (<= y0 yc) (< yc y1)) (and (<= y1 yc) (< yc y0)))
                  (vector-push-extend (+ x0 (* (- yc y0) (/ (- x1 x0) (- y1 y0)))) crossings))))
            (let ((xs (sort (coerce crossings 'list) #'<)))
              (loop for (xa xb) on xs by #'cddr
                    while xb
                    do (let ((start (max 0 (ceiling (- xa 0.5d0))))
                             (end (min w (ceiling (- xb 0.5d0)))))
                         (when (< start end)
                           (fill mask 255 :start (+ start (* row w)) :end (+ end (* row w)))))))))))
    mask))

(defun mask-bounds (mask w h)
  "The rectangle (x y w h) holding every nonzero byte of MASK (W x H), or
nil when it has none."
  (let ((x0 w) (y0 h) (x1 -1) (y1 -1))
    (dotimes (row h)
      (let ((base (* row w)))
        (dotimes (col w)
          (unless (zerop (aref mask (+ base col)))
            (setf x0 (min x0 col) x1 (max x1 col) y0 (min y0 row) y1 (max y1 row))))))
    (when (>= x1 0)
      (list x0 y0 (1+ (- x1 x0)) (1+ (- y1 y0))))))

(defun make-selection (mask w h)
  "The selection plist for MASK, or nil when it selects nothing."
  (let ((rect (mask-bounds mask w h)))
    (and rect (list :mask mask :rect rect))))

(defun select-rect! (doc x y w h)
  (the-object doc (set-slot! :selection
                             (make-selection (rasterize-rect (the-object doc doc-width)
                                                             (the-object doc doc-height) x y w h)
                                             (the-object doc doc-width) (the-object doc doc-height)))))

(defun select-polygon! (doc points)
  (the-object doc (set-slot! :selection
                             (make-selection (rasterize-polygon (the-object doc doc-width)
                                                                (the-object doc doc-height) points)
                                             (the-object doc doc-width) (the-object doc doc-height)))))

(defun select-all! (doc)
  (select-rect! doc 0 0 (the-object doc doc-width) (the-object doc doc-height)))

(defun select-none! (doc)
  (the-object doc (set-slot! :selection nil)))

(defun select-inverse! (doc)
  "Everything the selection does not cover; with nothing selected, all."
  (let ((selection (the-object doc selection))
        (w (the-object doc doc-width))
        (h (the-object doc doc-height)))
    (if (null selection)
        (select-all! doc)
        (let ((mask (copy-seq (getf selection :mask))))
          (dotimes (i (length mask))
            (setf (aref mask i) (- 255 (aref mask i))))
          (the-object doc (set-slot! :selection (make-selection mask w h)))))))

(defun clip-coverage! (coverage x y w h selection width)
  "COVERAGE (W x H doubles at X, Y of a WIDTH-wide document) multiplied
in place by SELECTION's mask, byte over 255, as the browser's
clipCoverage does it; nothing when SELECTION is nil."
  (when selection
    (let ((mask (getf selection :mask)))
      (declare (type (simple-array (unsigned-byte 8) (*)) mask)
               (type (simple-array double-float (*)) coverage))
      (dotimes (row h)
        (dotimes (col w)
          (let ((i (+ col (* row w)))
                (s (aref mask (+ x col (* width (+ y row))))))
            (setf (aref coverage i) (* (aref coverage i) (/ (float s 1d0) 255d0))))))))
  coverage)

(defun fill-selection! (layer color opacity &optional erase?)
  "Edit > Fill: COLOR (sRGB 0-255) at OPACITY over raster LAYER inside
the document's selection, or everywhere without one; with ERASE?, Edit
> Clear, the pixels lose that much alpha instead.  The rectangle
changed, on the undo list."
  (unless (eql (the-object layer kind) :raster)
    (error "~a is not a raster layer." (the-object layer layer-name)))
  (let* ((doc (the-object layer parent))
         (width (the-object doc doc-width))
         (height (the-object doc doc-height))
         (selection (the-object doc selection))
         (rect (if selection (getf selection :rect) (list 0 0 width height)))
         (pixels (the-object layer (ensure-own-pixels!))))
    (destructuring-bind (x y w h) rect
      (let ((coverage (make-array (* w h) :element-type 'double-float :initial-element 1d0)))
        (clip-coverage! coverage x y w h selection width)
        (push-undo! doc (list (the-object layer index) :layer x y w h
                              (rgba-rect pixels width height x y w h 4)))
        (destructuring-bind (r g b) (stroke-color-linear (list :color color))
          (composite-stroke! pixels width coverage x y w h r g b
                             (max 0d0 (min 1d0 (float opacity 1d0))) erase?))
        (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))
        rect))))
