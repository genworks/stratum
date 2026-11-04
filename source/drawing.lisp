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
;; A Gendl model's drawing as a layer, in process: the drawing system's
;; svg lens projects the model's edges (base-view: any of the standard
;; views, hidden lines drawn, removed or dashed) and writes them as
;; <path> polylines and cubic curves inside translated <g> groups; this
;; reads those and draws them anti-aliased (Xiaolin Wu's lines) into a
;; coverage canvas the size of the document, which becomes a layer of
;; the ink color with the coverage as its alpha.  No Ghostscript, no
;; subprocess, no file.
;;
;; A layer's source is the spec (:drawing :source NAME :projection
;; :trimetric ...), NAME naming a registered Gendl model
;; (register-drawing-source), so the layer rasterizes at whatever size
;; the document has and the spec travels in a .stratum file like any
;; other generator's.  Text (<text>, the view labels and dimensions) is
;; not drawn yet: a stroke font is the next step.
;;

;;; The registry: a name to a function that makes the model, and the
;;; model once made.

(defvar *drawing-sources* (make-hash-table :test 'equal)
  "Name -> (maker . model): a defvar, so a reload keeps what was registered.")

(defun register-drawing-source (name maker)
  "Register the Gendl model MAKER makes (a function of no arguments
returning a GDL object) under NAME, for (:drawing :source NAME ...)."
  (setf (gethash name *drawing-sources*) (cons maker nil))
  name)

(defun drawing-source (name)
  "The model registered under NAME, made on first use."
  (let ((entry (gethash name *drawing-sources*)))
    (unless entry (error "No Gendl model is registered as ~s." name))
    (or (cdr entry) (setf (cdr entry) (funcall (car entry))))))

(defun drawing-source-names ()
  (let ((names '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *drawing-sources*)
    (sort names #'string<)))

(defparameter *drawing-projections*
  '(:top :bottom :left :right :front :rear :trimetric :tri-r-r :tri-l :tri-l-r)
  "The standard views of the drawing system, as geom-base names them.")

;;; The svg, in memory.

(defun drawing-svg (object &key (projection :trimetric) (hidden-lines :draw))
  "The svg of OBJECT's leaves as the drawing system draws them on a
single-view sheet: PROJECTION one of *drawing-projections*,
HIDDEN-LINES :draw, :remove or :dashed."
  (unless (member projection *drawing-projections*)
    (error "~s is not a standard view; one of ~{~(~a~)~^, ~}." projection *drawing-projections*))
  (let ((drawing (make-object 'gdl-user::single-view-drawing
                              :display-objects nil
                              :display-object-roots (list object)
                              :projection-vector (getf geom-base::*standard-views* projection)
                              :hidden-lines hidden-lines)))
    (with-output-to-string (s)
      (with-format (svg s :page-width (the-object drawing width)
                          :page-length (the-object drawing length)
                          :background-color :white)
        (write-the-object drawing cad-output)))))

;;; Reading the svg: attributes, numbers, paths, groups.

(defun svg-attribute (tag name)
  "The value of attribute NAME in the element text TAG, or nil."
  (let* ((key (format nil " ~a=\"" name))
         (start (search key tag)))
    (when start
      (let* ((from (+ start (length key)))
             (end (position #\" tag :start from)))
        (and end (subseq tag from end))))))

(defun svg-number (text start end)
  "TEXT from START below END as a double, or nil; digits, a sign, a
point and an exponent only, so the reader sees nothing else."
  (when (and (< start end)
             (loop for i from start below end always (find (char text i) "0123456789.-+eE")))
    (let ((n (ignore-errors
              (let ((*read-default-float-format* 'double-float)
                    (*read-eval* nil))
                (read-from-string text t nil :start start :end end)))))
      (and (realp n) (float n 1d0)))))

(defun svg-numbers (text)
  "Every number in TEXT in order; separators are anything but the
characters of a number (an e is part of a number only inside one)."
  (let ((numbers '()) (start nil))
    (flet ((flush (end)
             (when start
               (let ((n (svg-number text start end)))
                 (when n (push n numbers)))
               (setf start nil))))
      (loop for i from 0 below (length text)
            for c = (char text i)
            do (if (or (digit-char-p c) (find c ".-+eE"))
                   (if (and (find c "eE") (null start))
                       (flush i)
                       (unless start (setf start i)))
                   (flush i))
            finally (flush (length text))))
    (nreverse numbers)))

(defparameter *curve-segments* 16
  "How many straight pieces a cubic curve is drawn as.")

(defun svg-path-polylines (d)
  "The polylines of a path's D attribute as lists of (x . y): M and L
with implicit line-to, C (cubic curves, flattened), Z, absolute and
relative."
  (let ((polylines '()) (current '()) (command #\M) (relative nil) (cx 0d0) (cy 0d0))
    (flet ((close-current ()
             (when (cdr current) (push (nreverse current) polylines))
             (setf current '()))
           (point (x y)
             (if relative (values (+ cx x) (+ cy y)) (values x y))))
      (labels ((add-point (x y)
                 (setf cx x cy y)
                 (push (cons x y) current))
               (take (numbers)
                 (case command
                   ((#\M #\L)
                    (loop for (x y) on numbers by #'cddr
                          when y do (multiple-value-bind (px py) (point x y) (add-point px py))))
                   (#\C
                    (loop for (x1 y1 x2 y2 x y) on numbers by (lambda (l) (nthcdr 6 l))
                          when y
                            do (multiple-value-bind (p1x p1y) (point x1 y1)
                                 (multiple-value-bind (p2x p2y) (point x2 y2)
                                   (multiple-value-bind (px py) (point x y)
                                     (let ((x0 cx) (y0 cy))
                                       (loop for k from 1 to *curve-segments*
                                             do (let* ((u (/ k (float *curve-segments* 1d0)))
                                                       (v (- 1 u))
                                                       (bx (+ (* v v v x0) (* 3 v v u p1x) (* 3 v u u p2x) (* u u u px)))
                                                       (by (+ (* v v v y0) (* 3 v v u p1y) (* 3 v u u p2y) (* u u u py))))
                                                  (push (cons bx by) current)))
                                       (setf cx px cy py))))))))))
        (loop with i = 0
              while (< i (length d))
              do (let ((c (char d i)))
                   (cond ((find c "MmLlCc")
                          (when (char-equal c #\M) (close-current))
                          (setf command (char-upcase c) relative (lower-case-p c))
                          (incf i))
                         ((find c "Zz")
                          (when current (push (first (last current)) current))
                          (close-current)
                          (incf i))
                         ((member c '(#\Space #\, #\Newline #\Tab #\Return))
                          (incf i))
                         (t
                          (let ((end (or (position-if (lambda (ch) (find ch "MmLlCcZz")) d :start i)
                                         (length d))))
                            (take (svg-numbers (subseq d i end)))
                            (setf i end))))))
        (close-current)))
    (nreverse polylines)))

(defun svg-segments (svg)
  "Every line segment of the drawing SVG (a string) translated by the
<g> groups it sits in, as a list of (x0 y0 x1 y1 dashed?); the sheet's
width and height as second and third values."
  (let ((segments '()) (offsets (list (cons 0d0 0d0)))
        (width 612d0) (height 792d0))
    (loop with start = 0
          for open = (position #\< svg :start start)
          while open
          do (let* ((close (or (position #\> svg :start open) (length svg)))
                    (tag (subseq svg open (1+ close))))
               (cond
                 ((and (> (length tag) 4) (string= "<svg" tag :end2 4))
                  (let ((box (svg-numbers (or (svg-attribute tag "viewBox") ""))))
                    (when (= (length box) 4)
                      (setf width (third box) height (fourth box)))))
                 ((string= "<g>" tag) (push (first offsets) offsets))
                 ((and (> (length tag) 2) (string= "<g" tag :end2 2)
                       (member (char tag 2) '(#\Space #\Newline #\Tab #\Return)))
                  (let* ((transform (or (svg-attribute tag "transform") ""))
                         (numbers (and (search "translate" transform) (svg-numbers transform)))
                         (parent (first offsets)))
                    (push (cons (+ (car parent) (or (first numbers) 0d0))
                                (+ (cdr parent) (or (second numbers) 0d0)))
                          offsets)))
                 ((and (> (length tag) 3) (string= "</g" tag :end2 3))
                  (when (cdr offsets) (pop offsets)))
                 ((and (> (length tag) 5) (string= "<path" tag :end2 5))
                  (let ((d (svg-attribute tag "d"))
                        (dashed (and (svg-attribute tag "stroke-dasharray") t))
                        (dx (car (first offsets))) (dy (cdr (first offsets))))
                    (when d
                      (dolist (polyline (svg-path-polylines d))
                        (loop for (a b) on polyline
                              when b do (push (list (+ dx (car a)) (+ dy (cdr a))
                                                    (+ dx (car b)) (+ dy (cdr b)) dashed)
                                              segments)))))))
               (setf start (1+ close))))
    (values (nreverse segments) width height)))

;;; Drawing: a coverage canvas, anti-aliased lines.

(defun make-coverage (width height)
  (make-array (* width height) :element-type 'double-float :initial-element 0d0))

(defun plot-coverage (canvas width height x y coverage)
  "Ink pixel (X, Y) by COVERAGE in [0, 1]; the heavier ink wins."
  (declare (type (simple-array double-float (*)) canvas) (type fixnum width height x y))
  (when (and (<= 0 x) (< x width) (<= 0 y) (< y height))
    (let ((i (+ x (* width y)))
          (c (min 1d0 (max 0d0 coverage))))
      (when (> c (aref canvas i))
        (setf (aref canvas i) c)))))

(defun draw-line (canvas width height x0 y0 x1 y1 &key (weight 1d0))
  "An anti-aliased line (Xiaolin Wu), WEIGHT the ink strength in [0, 1]."
  (let ((steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
    (when steep (rotatef x0 y0) (rotatef x1 y1))
    (when (> x0 x1) (rotatef x0 x1) (rotatef y0 y1))
    (let* ((dx (- x1 x0)) (dy (- y1 y0))
           (gradient (if (zerop dx) 1d0 (/ dy dx))))
      (flet ((put (x y c)
               (if steep
                   (plot-coverage canvas width height y x (* weight c))
                   (plot-coverage canvas width height x y (* weight c)))))
        (let* ((xend (round x0)) (yend (+ y0 (* gradient (- xend x0))))
               (xgap (- 1 (- (+ x0 0.5d0) (floor (+ x0 0.5d0)))))
               (xpxl1 xend) (ypxl1 (floor yend)))
          (put xpxl1 ypxl1 (* (- 1 (- yend ypxl1)) xgap))
          (put xpxl1 (1+ ypxl1) (* (- yend ypxl1) xgap))
          (let* ((intery (+ yend gradient))
                 (xend2 (round x1)) (yend2 (+ y1 (* gradient (- xend2 x1))))
                 (xgap2 (- (+ x1 0.5d0) (floor (+ x1 0.5d0))))
                 (xpxl2 xend2) (ypxl2 (floor yend2)))
            (put xpxl2 ypxl2 (* (- 1 (- yend2 ypxl2)) xgap2))
            (put xpxl2 (1+ ypxl2) (* (- yend2 ypxl2) xgap2))
            (loop for x from (1+ xpxl1) below xpxl2
                  do (let ((iy (floor intery)))
                       (put x iy (- 1 (- intery iy)))
                       (put x (1+ iy) (- intery iy)))
                     (incf intery gradient))))))))

(defun draw-dashed-line (canvas width height x0 y0 x1 y1 &key (dash 6d0) (weight 0.55d0))
  (let ((length (sqrt (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2)))))
    (if (< length (* 2 dash))
        (draw-line canvas width height x0 y0 x1 y1 :weight weight)
        (let ((ux (/ (- x1 x0) length)) (uy (/ (- y1 y0) length)))
          (loop for s from 0d0 below length by (* 2 dash)
                do (let ((e (min length (+ s dash))))
                     (draw-line canvas width height
                                (+ x0 (* ux s)) (+ y0 (* uy s)) (+ x0 (* ux e)) (+ y0 (* uy e))
                                :weight weight)))))))

(defun segments-coverage (segments width height &key (margin 0.05d0) (weight 1.5d0) sheet)
  "SEGMENTS drawn into a WIDTH x HEIGHT coverage canvas: the segments'
own extent fitted inside a MARGIN (a fraction of the shorter side) and
centered, or with SHEET (sheet-width sheet-height) the whole sheet
scaled to fit.  WEIGHT is the line's thickness in pixels, 1 to 2."
  (let ((canvas (make-coverage width height)))
    (when segments
      (let* ((left (if sheet 0d0 (reduce #'min segments :key (lambda (s) (min (first s) (third s))))))
             (top (if sheet 0d0 (reduce #'min segments :key (lambda (s) (min (second s) (fourth s))))))
             (right (if sheet (first sheet) (reduce #'max segments :key (lambda (s) (max (first s) (third s))))))
             (bottom (if sheet (second sheet) (reduce #'max segments :key (lambda (s) (max (second s) (fourth s))))))
             (m (* margin (min width height)))
             (scale (/ (- (min width height) (* 2 m))
                       (max (- right left) (- bottom top) 1d-9)))
             (scale (min scale (/ (- width (* 2 m)) (max (- right left) 1d-9))
                         (/ (- height (* 2 m)) (max (- bottom top) 1d-9))))
             (ox (/ (- width (* scale (- right left))) 2))
             (oy (/ (- height (* scale (- bottom top))) 2))
             (second-pass (max 0d0 (min 1d0 (- weight 1d0)))))
        (flet ((px (x) (+ ox (* (- x left) scale)))
               (py (y) (+ oy (* (- y top) scale))))
          (dolist (segment segments)
            (destructuring-bind (x0 y0 x1 y1 dashed) segment
              (if dashed
                  (draw-dashed-line canvas width height (px x0) (py y0) (px x1) (py y1))
                  (progn
                    (draw-line canvas width height (px x0) (py y0) (px x1) (py y1))
                    ;; a second pass half a pixel over thickens the line
                    (when (plusp second-pass)
                      (draw-line canvas width height
                                 (+ (px x0) 0.5d0) (+ (py y0) 0.5d0) (+ (px x1) 0.5d0) (+ (py y1) 0.5d0)
                                 :weight second-pass)))))))))
    canvas))

(defun coverage-rgba (canvas width height &key (color '(0 0 0)))
  "The coverage CANVAS as an RGBA8 layer of COLOR (r g b bytes) with the
coverage as alpha."
  (let ((out (make-array (* 4 width height) :element-type '(unsigned-byte 8) :initial-element 0))
        (r (first color)) (g (second color)) (b (third color)))
    (dotimes (i (* width height) out)
      (let ((a (round (* 255 (aref canvas i)))))
        (when (plusp a)
          (let ((o (* 4 i)))
            (setf (aref out o) r (aref out (+ o 1)) g (aref out (+ o 2)) b (aref out (+ o 3)) a)))))))

;;; Gendl's own rasterizer (gendl-raster, in gwl-graphics since
;;; 2026-11-03) draws the same lines and also fills the arrowheads and
;;; draws the text -- view labels, dimensions -- in the Hershey simplex
;;; stroke font.  An image that has it does the whole job; an image
;;; without it (the published images before then) gets the lines above.

(defun gendl-raster-coverage (svg width height margin weight sheet?)
  "The coverage canvas gendl-raster draws for SVG at WIDTH x HEIGHT, or
nil when the image has no gendl-raster."
  (let ((renderer (and (find-package "GENDL-RASTER")
                       (find-symbol "RENDER-SVG-COVERAGE" "GENDL-RASTER"))))
    (and renderer (fboundp renderer)
         (funcall renderer svg :size (list width height)
                               :fit (if sheet? nil (float margin 1d0))
                               :line-weight (float weight 1d0)))))

(defun svg-rgba (svg width height &key (color '(0 0 0)) (margin 0.05d0) (weight 1.5d0) sheet?)
  "The drawing SVG rasterized as a WIDTH x HEIGHT RGBA8 layer: ink of
COLOR, alpha from coverage; the drawing's extent fitted with MARGIN,
or with SHEET? the whole sheet.  Through gendl-raster (text and filled
arrowheads too) when the image has it."
  (coverage-rgba (or (gendl-raster-coverage svg width height margin weight sheet?)
                     (multiple-value-bind (segments sw sh) (svg-segments svg)
                       (segments-coverage segments width height
                                          :margin (float margin 1d0) :weight (float weight 1d0)
                                          :sheet (and sheet? (list sw sh)))))
                 width height :color color))

;;; The generator: (:drawing :source NAME ...) as a layer's source.

(defun gen-drawing (w h &key source (projection :trimetric) (hidden-lines :draw)
                                 (color '(0 0 0)) (margin 0.05) (weight 1.5) sheet?)
  "The registered Gendl model SOURCE drawn as a W x H layer."
  (svg-rgba (drawing-svg (drawing-source source) :projection projection :hidden-lines hidden-lines)
            w h :color color :margin margin :weight weight :sheet? sheet?))

(defun drawing-layer-spec (source &key (projection :trimetric) (hidden-lines :draw)
                                       (color '(0 0 0)) (margin 0.05) (weight 1.5) name)
  "A raster layer spec drawn from the registered model SOURCE."
  (list :kind :raster
        :name (or name (format nil "~a (~(~a~))" source projection))
        :source (list :drawing :source source :projection projection :hidden-lines hidden-lines
                      :color color :margin margin :weight weight)))

(defun place-drawing! (doc source &rest options &key above &allow-other-keys)
  "A new layer of DOC drawn from the registered model SOURCE (options
as drawing-layer-spec takes them), above layer ABOVE or on top; the new
layer's index."
  (let ((spec (apply #'drawing-layer-spec source
                     (loop for (k v) on options by #'cddr unless (eq k :above) collect k and collect v))))
    (the-object doc (add-layer! spec above))))

;;; A model to draw out of the box: a bracket of the four primitives.

(define-object drawing-sample (base-object)
  :documentation
  (:description "A small assembly of the geometric primitives -- a base
box, a boss cylinder on it, a cone on the boss and a sphere beside --
so a drawing has straight edges, circles and curves to show.")

  :computed-slots
  ((width 40) (length 24) (height 10))

  :objects
  ((base :type 'box
         :width 40 :length 24 :height 6
         :center (make-point 0 0 3))
   (boss :type 'cylinder
         :radius 6 :length 10
         :center (make-point -8 0 11)
         :orientation (alignment :front (the (face-normal-vector :top))))
   (tip :type 'cone
        :radius-1 6 :radius-2 1.5 :length 8
        :center (make-point -8 0 20)
        :orientation (alignment :front (the (face-normal-vector :top))))
   (ball :type 'sphere
         :radius 5
         :center (make-point 12 0 11))))

(register-drawing-source "sample" (lambda () (make-object 'drawing-sample)))
(register-drawing-source "box" (lambda () (make-object 'box :width 10 :length 6 :height 4)))
