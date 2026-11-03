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

(defun select-translate! (doc dx dy)
  "The selection moved by DX, DY and clipped; nil when nothing is left."
  (let ((selection (the-object doc selection))
        (w (the-object doc doc-width))
        (h (the-object doc doc-height)))
    (when selection
      (let ((old (getf selection :mask))
            (mask (make-gray w h)))
        (dotimes (y h)
          (let ((sy (- y dy)))
            (when (and (<= 0 sy) (< sy h))
              (let ((x0 (max 0 dx)) (x1 (min w (+ w dx))))
                (when (< x0 x1)
                  (replace mask old :start1 (+ x0 (* y w)) :end1 (+ x1 (* y w))
                                    :start2 (+ (- x0 dx) (* sy w))))))))
        (the-object doc (set-slot! :selection (make-selection mask w h)))))))

;;; ---- the floating layer: Move, Cut, Copy, Paste ------------------------

(defun move-pixels! (layer dx dy)
  "The Move tool: raster LAYER's selected pixels (all of them without a
selection) moved by DX, DY, integers.  Each pixel is what the cut
leaves (its alpha times one minus the selection) with the moved copy
composited over it, in premultiplied linear light; the twin of the
shader's floating-layer stage.  The whole layer goes on the undo list,
the selection moves with the pixels, and the rectangle is the whole
layer."
  (unless (eql (the-object layer kind) :raster)
    (error "~a is not a raster layer." (the-object layer layer-name)))
  (let* ((doc (the-object layer parent))
         (w (the-object doc doc-width))
         (h (the-object doc doc-height))
         (pixels (the-object layer (ensure-own-pixels!)))
         (src (copy-seq pixels))
         (selection (the-object doc selection))
         (sel (and selection (getf selection :mask)))
         (decode *srgb-decode*))
    (declare (type (simple-array (unsigned-byte 8) (*)) pixels src)
             (type (simple-array double-float (256)) decode)
             (type fixnum w h dx dy))
    (push-undo! doc (list (the-object layer index) :layer 0 0 w h src))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((i (+ x (* y w)))
               (o (* 4 i))
               (sh (if sel (/ (float (aref sel i) 1d0) 255d0) 1d0))
               (ba (* (/ (float (aref src (+ o 3)) 1d0) 255d0) (- 1d0 sh)))
               (qx (- x dx)) (qy (- y dy))
               (inq (and (<= 0 qx) (< qx w) (<= 0 qy) (< qy h)))
               (qi (if inq (+ qx (* qy w)) 0))
               (qo (* 4 qi))
               (st (if inq (if sel (/ (float (aref sel qi) 1d0) 255d0) 1d0) 0d0))
               (ma (if inq (* (/ (float (aref src (+ qo 3)) 1d0) 255d0) st) 0d0))
               (na (+ ma (* ba (- 1d0 ma)))))
          (declare (type double-float sh ba st ma na))
          (if (> na 0d0)
              (flet ((channel (k)
                       (/ (+ (* (aref decode (aref src (+ qo k))) ma)
                             (* (aref decode (aref src (+ o k))) ba (- 1d0 ma)))
                          na)))
                (setf (aref pixels o) (unit->byte (linear->srgb (channel 0)))
                      (aref pixels (+ o 1)) (unit->byte (linear->srgb (channel 1)))
                      (aref pixels (+ o 2)) (unit->byte (linear->srgb (channel 2)))
                      (aref pixels (+ o 3)) (unit->byte na)))
              (setf (aref pixels o) 0 (aref pixels (+ o 1)) 0
                    (aref pixels (+ o 2)) 0 (aref pixels (+ o 3)) 0)))))
    (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))
    (select-translate! doc dx dy)
    (list 0 0 w h)))

;;; Free Transform: the same cut-and-composite as Move, the copy sampled
;;; through an affine matrix.  The matrix (a b c d e f) maps a source
;;; point to its destination, x' = ax + by + c, y' = dx + ey + f, in
;;; pixel coordinates with centers at half-integers; its inverse finds,
;;; for each destination pixel, the source point to sample, bilinear in
;;; premultiplied linear light with the selection as a weight, as the
;;; shader's float_sample() does it.

(defun affine-inverse (m)
  "The inverse of the affine M (a b c d e f), or nil when singular."
  (destructuring-bind (a b c d e f) (mapcar (lambda (v) (float v 1d0)) m)
    (let ((det (- (* a e) (* b d))))
      (unless (< (abs det) 1d-12)
        (list (/ e det) (/ (- b) det) (/ (- (* b f) (* c e)) det)
              (/ (- d) det) (/ a det) (/ (- (* c d) (* a f)) det))))))

(defun transform-pixels! (layer matrix)
  "Free Transform committed: raster LAYER's selected pixels (all without
a selection) through MATRIX (a b c d e f), each destination pixel being
what the cut leaves with the resampled copy over it; the selection goes
through the same matrix.  The whole layer on the undo list; returns it
as the rectangle."
  (unless (eql (the-object layer kind) :raster)
    (error "~a is not a raster layer." (the-object layer layer-name)))
  (let* ((doc (the-object layer parent))
         (w (the-object doc doc-width))
         (h (the-object doc doc-height))
         (inv (or (affine-inverse matrix) (error "The transform is singular.")))
         (pixels (the-object layer (ensure-own-pixels!)))
         (src (copy-seq pixels))
         (selection (the-object doc selection))
         (sel (and selection (getf selection :mask)))
         (new-mask (and sel (make-gray w h)))
         (decode *srgb-decode*))
    (declare (type (simple-array (unsigned-byte 8) (*)) pixels src)
             (type (simple-array double-float (256)) decode)
             (type fixnum w h))
    (destructuring-bind (ia ib ic id ie iff) inv
      (declare (type double-float ia ib ic id ie iff))
      (push-undo! doc (list (the-object layer index) :layer 0 0 w h src))
      (flet ((sample (qx qy)
               ;; premultiplied linear (r g b a) and the selection at the source point
               (let* ((fx (- qx 0.5d0)) (fy (- qy 0.5d0))
                      (bx (floor fx)) (by (floor fy))
                      (tx (- fx bx)) (ty (- fy by))
                      (r 0d0) (g 0d0) (b 0d0) (a 0d0) (s 0d0))
                 (declare (type double-float fx fy tx ty r g b a s) (type fixnum bx by))
                 (dotimes (j 2)
                   (dotimes (k 2)
                     (let ((cx (+ bx k)) (cy (+ by j))
                           (wgt (* (if (= k 0) (- 1d0 tx) tx) (if (= j 0) (- 1d0 ty) ty))))
                       (when (and (<= 0 cx) (< cx w) (<= 0 cy) (< cy h))
                         (let* ((i (+ cx (* cy w)))
                                (o (* 4 i))
                                (sv (if sel (/ (float (aref sel i) 1d0) 255d0) 1d0))
                                (pa (* (/ (float (aref src (+ o 3)) 1d0) 255d0) sv)))
                           (incf r (* (aref decode (aref src o)) pa wgt))
                           (incf g (* (aref decode (aref src (+ o 1))) pa wgt))
                           (incf b (* (aref decode (aref src (+ o 2))) pa wgt))
                           (incf a (* pa wgt))
                           (incf s (* sv wgt)))))))
                 (values r g b a s))))
        (dotimes (y h)
          (dotimes (x w)
            (let* ((i (+ x (* y w)))
                   (o (* 4 i))
                   (px (+ x 0.5d0)) (py (+ y 0.5d0))
                   (qx (+ (* ia px) (* ib py) ic))
                   (qy (+ (* id px) (* ie py) iff))
                   (sh (if sel (/ (float (aref sel i) 1d0) 255d0) 1d0))
                   (ba (* (/ (float (aref src (+ o 3)) 1d0) 255d0) (- 1d0 sh))))
              (multiple-value-bind (mr mg mb ma ms) (sample qx qy)
                (let ((na (+ ma (* ba (- 1d0 ma)))))
                  (if (> na 0d0)
                      (flet ((channel (m k) (/ (+ m (* (aref decode (aref src (+ o k))) ba (- 1d0 ma))) na)))
                        (setf (aref pixels o) (unit->byte (linear->srgb (channel mr 0)))
                              (aref pixels (+ o 1)) (unit->byte (linear->srgb (channel mg 1)))
                              (aref pixels (+ o 2)) (unit->byte (linear->srgb (channel mb 2)))
                              (aref pixels (+ o 3)) (unit->byte na)))
                      (setf (aref pixels o) 0 (aref pixels (+ o 1)) 0
                            (aref pixels (+ o 2)) 0 (aref pixels (+ o 3)) 0)))
                (when new-mask (setf (aref new-mask i) (unit->byte ms))))))))
      (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))
      (when new-mask
        (the-object doc (set-slot! :selection (make-selection new-mask w h))))
      (list 0 0 w h))))

(defvar *clipboard* nil
  "Plist (:pixels rgba :w :h :x :y): what Copy or Cut took, with where it
came from, shared by every document as an editor's clipboard is.")

(defun copy-selection! (layer)
  "Copy: raster LAYER's pixels inside the selection's rectangle, their
alpha times the selection, onto *clipboard*; the whole layer without
one.  Returns (x y w h)."
  (unless (eql (the-object layer kind) :raster)
    (error "~a is not a raster layer." (the-object layer layer-name)))
  (let* ((doc (the-object layer parent))
         (width (the-object doc doc-width))
         (height (the-object doc doc-height))
         (selection (the-object doc selection))
         (rect (if selection (getf selection :rect) (list 0 0 width height))))
    (destructuring-bind (x y w h) rect
      (let ((pixels (rgba-rect (the-object layer pixels) width height x y w h 4)))
        (when selection
          (let ((mask (getf selection :mask)))
            (dotimes (row h)
              (dotimes (col w)
                (let ((o (+ 3 (* 4 (+ col (* row w)))))
                      (s (aref mask (+ x col (* width (+ y row))))))
                  (setf (aref pixels o) (unit->byte (* (/ (float (aref pixels o) 1d0) 255d0)
                                                       (/ (float s 1d0) 255d0)))))))))
        (setf *clipboard* (list :pixels pixels :w w :h h :x x :y y))
        rect))))

(defun cut-selection! (layer)
  "Cut: Copy, then Clear."
  (copy-selection! layer)
  (fill-selection! layer '(0 0 0) 1 t))

(defun paste-clipboard! (doc &optional above)
  "Paste: a new raster layer above ABOVE (or on top) holding the
clipboard's pixels where they came from, or centered when they do not
fit; the new layer's index.  Nothing without a clipboard."
  (unless *clipboard* (error "The clipboard is empty."))
  (let* ((width (the-object doc doc-width))
         (height (the-object doc doc-height))
         (cw (getf *clipboard* :w)) (ch (getf *clipboard* :h))
         (fits (and (<= (+ (getf *clipboard* :x) cw) width) (<= (+ (getf *clipboard* :y) ch) height)))
         (x (if fits (getf *clipboard* :x) (floor (- width cw) 2)))
         (y (if fits (getf *clipboard* :y) (floor (- height ch) 2)))
         (pixels (make-rgba width height)))
    (dotimes (row ch)
      (let ((py (+ y row)))
        (when (and (<= 0 py) (< py height))
          (let ((x0 (max 0 x)) (x1 (min width (+ x cw))))
            (when (< x0 x1)
              (replace pixels (getf *clipboard* :pixels)
                       :start1 (* 4 (+ x0 (* width py)))
                       :start2 (* 4 (+ (- x0 x) (* cw row)))
                       :end2 (* 4 (+ (- x1 x) (* cw row)))))))))
    (the-object doc (add-layer! (plist-merge (list :pixels pixels) (the-object doc (new-layer-spec :raster)))
                                above))))

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
