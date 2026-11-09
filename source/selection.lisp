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

(defun combine-masks (old mask mode)
  "MASK as the new selection (MODE :new), added to OLD (:add, the
larger coverage), taken from it (:subtract, OLD where MASK is not) or
kept where both cover (:intersect).  OLD may be nil: everything."
  (cond ((or (eql mode :new) (null old)) (if (eql mode :subtract) (make-array (length mask) :element-type '(unsigned-byte 8) :initial-element 0) mask))
        ((eql mode :add) (map '(simple-array (unsigned-byte 8) (*)) #'max old mask))
        ((eql mode :subtract) (map '(simple-array (unsigned-byte 8) (*)) (lambda (o m) (min o (- 255 m))) old mask))
        ((eql mode :intersect) (map '(simple-array (unsigned-byte 8) (*)) #'min old mask))
        (t (error "Unknown selection mode ~s." mode))))

(defun set-selection-mask! (doc mask mode)
  (let ((old (and (the-object doc selection) (getf (the-object doc selection) :mask))))
    (the-object doc (set-slot! :selection
                               (make-selection (combine-masks old mask mode)
                                               (the-object doc doc-width) (the-object doc doc-height))))))

(defun select-rect! (doc x y w h &optional (mode :new))
  (set-selection-mask! doc (rasterize-rect (the-object doc doc-width) (the-object doc doc-height) x y w h) mode))

(defun select-polygon! (doc points &optional (mode :new))
  (set-selection-mask! doc (rasterize-polygon (the-object doc doc-width) (the-object doc doc-height) points) mode))

;;; The Magic Wand: the pixels within TOLERANCE of the one at X, Y, by
;;; the largest channel difference, alpha included; the connected patch
;;; of them when CONTIGUOUS, every one otherwise.
(defun wand-mask (pixels w h x y tolerance contiguous)
  (let* ((mask (make-gray w h))
         (o (* 4 (+ x (* y w))))
         (r0 (aref pixels o)) (g0 (aref pixels (+ o 1))) (b0 (aref pixels (+ o 2))) (a0 (aref pixels (+ o 3))))
    (declare (type (simple-array (unsigned-byte 8) (*)) pixels mask) (type fixnum w h tolerance))
    (flet ((matches (i)
             (let ((p (* 4 i)))
               (and (<= (abs (- (aref pixels p) r0)) tolerance)
                    (<= (abs (- (aref pixels (+ p 1)) g0)) tolerance)
                    (<= (abs (- (aref pixels (+ p 2)) b0)) tolerance)
                    (<= (abs (- (aref pixels (+ p 3)) a0)) tolerance)))))
      (if (not contiguous)
          (dotimes (i (* w h))
            (when (matches i) (setf (aref mask i) 255)))
          (let ((stack (make-array 1024 :element-type 'fixnum :adjustable t :fill-pointer 0)))
            (vector-push-extend (+ x (* y w)) stack)
            (setf (aref mask (+ x (* y w))) 255)
            (loop while (plusp (fill-pointer stack))
                  do (let* ((i (vector-pop stack))
                            (px (mod i w)) (py (floor i w)))
                       (flet ((visit (j)
                                (when (and (zerop (aref mask j)) (matches j))
                                  (setf (aref mask j) 255)
                                  (vector-push-extend j stack))))
                         (when (> px 0) (visit (1- i)))
                         (when (< px (1- w)) (visit (1+ i)))
                         (when (> py 0) (visit (- i w)))
                         (when (< py (1- h)) (visit (+ i w)))))))))
    mask))

(defun select-wand! (doc x y &key (tolerance 32) (contiguous t) (mode :new) layer)
  "The Magic Wand at X, Y on LAYER's pixels (a raster layer) or on the
composited picture (nil, or any other layer)."
  (let* ((w (the-object doc doc-width))
         (h (the-object doc doc-height))
         (x (max 0 (min (1- w) (round x))))
         (y (max 0 (min (1- h) (round y))))
         (pixels (if (and layer (eql (the-object layer kind) :raster))
                     (the-object layer pixels)
                     (the-object doc (render-cpu)))))
    (set-selection-mask! doc (wand-mask pixels w h x y (max 0 (min 255 (round tolerance))) contiguous) mode)))

(defparameter *feather-exact-sigma* 12d0
  "Up to this radius a feather is the exact blur.  Past it the mask is
blurred at a reduced size and scaled back: a mask that soft has nothing
left at full size for the blur to keep, and the exact kernel over a
photo is minutes of work under the server's lock.")

(defun feather-mask-exact (mask w h sigma &optional (rect (list 0 0 w h)))
  "MASK (W x H bytes) blurred by SIGMA: fresh bytes.  RECT (x y w h)
holds every nonzero byte of it, and only that rectangle and the
kernel's reach around it are worked on: past that the blur has nothing
but zeros to add, so the bytes are the same as a blur of the whole mask,
for the memory and the time of the selection and not of the picture."
  (destructuring-bind (rx ry rw rh) rect
    (let* ((extent (nth-value 1 (gaussian-kernel (float sigma 1d0))))
           (x0 (max 0 (- rx extent)))
           (y0 (max 0 (- ry extent)))
           (x1 (min w (+ rx rw extent)))
           (y1 (min h (+ ry rh extent)))
           (pw (- x1 x0))
           (ph (- y1 y0))
           (plane (make-plane (* pw ph)))
           (scratch (make-plane (* pw ph)))
           (out (make-gray w h)))
      (dotimes (y ph)
        (dotimes (x pw)
          (setf (aref plane (+ x (* y pw)))
                (/ (float (aref mask (+ x0 x (* (+ y0 y) w))) 1f0) 255f0))))
      (blur-plane! plane scratch pw ph sigma)
      (dotimes (y ph out)
        (dotimes (x pw)
          (setf (aref out (+ x0 x (* (+ y0 y) w)))
                (unit->byte (float (aref plane (+ x (* y pw))) 1d0))))))))

(defun feather-mask-reduced (mask w h sigma)
  "MASK blurred by SIGMA at a reduced size: averaged down by K, blurred,
and scaled back bilinearly, the blur narrowed by what the averaging
already does (a box of K has a variance of K squared over twelve)."
  (let* ((k (max 2 (floor sigma 6)))
         (sw (ceiling w k))
         (sh (ceiling h k))
         (small (make-plane (* sw sh)))
         (scratch (make-plane (* sw sh)))
         (reduced-sigma (/ (sqrt (max 1d0 (- (* sigma sigma) (/ (* k k) 12d0)))) k))
         (out (make-gray w h)))
    (dotimes (sy sh)
      (dotimes (sx sw)
        (let ((sum 0) (n 0))
          (loop for y from (* sy k) below (min h (* (1+ sy) k))
                do (loop for x from (* sx k) below (min w (* (1+ sx) k))
                         do (incf sum (aref mask (+ x (* y w))))
                            (incf n)))
          (setf (aref small (+ sx (* sy sw))) (float (/ sum (* 255d0 n)) 1f0)))))
    (blur-plane! small scratch sw sh reduced-sigma)
    (flet ((axis (p limit)
             ;; the reduced cell under P and the way to the next one
             (let* ((s (max 0d0 (min (float (1- limit) 1d0) (- (/ (+ p 0.5d0) k) 0.5d0))))
                    (i (min (max 0 (- limit 2)) (floor s))))
               (values i (- s i)))))
      (dotimes (y h out)
        (multiple-value-bind (y0 fy) (axis y sh)
          (let ((y1 (min (1- sh) (1+ y0))))
            (dotimes (x w)
              (multiple-value-bind (x0 fx) (axis x sw)
                (let* ((x1 (min (1- sw) (1+ x0)))
                       (top (+ (* (- 1d0 fx) (aref small (+ x0 (* y0 sw)))) (* fx (aref small (+ x1 (* y0 sw))))))
                       (bottom (+ (* (- 1d0 fx) (aref small (+ x0 (* y1 sw)))) (* fx (aref small (+ x1 (* y1 sw)))))))
                  (setf (aref out (+ x (* y w)))
                        (unit->byte (+ (* (- 1d0 fy) top) (* fy bottom)))))))))))))

(defun feather-selection! (doc radius)
  "The selection's edge softened by a Gaussian blur of RADIUS pixels;
nothing without a selection."
  (let ((selection (the-object doc selection))
        (w (the-object doc doc-width))
        (h (the-object doc doc-height))
        (sigma (max 0.1d0 (float radius 1d0))))
    (when selection
      (let ((mask (getf selection :mask)))
        (the-object doc (set-slot! :selection
                                   (make-selection (if (> sigma *feather-exact-sigma*)
                                                       (feather-mask-reduced mask w h sigma)
                                                       (feather-mask-exact mask w h sigma (getf selection :rect)))
                                                   w h)))))))

;;; Select > Modify > Contract and Expand: the selection's edge moved in
;;; or out.  A feather spreads both ways, so a cut-out on the outline
;;; brings a rim of what was behind the subject; moved in first, it does
;;; not.  The distance from every pixel to the nearest pixel across the
;;; edge comes from an exact Euclidean transform, whatever the amount.

(defun parabola-envelope! (f d v z n)
  "D, for each of the N places, the lowest of F[p] + (q - p) squared over
every p: the lower envelope of the parabolas rooted at F.  V and Z are
workspace for N and N + 1 entries."
  (declare (type (simple-array double-float (*)) f d z)
           (type (simple-array fixnum (*)) v)
           (type fixnum n)
           (optimize (speed 3)))
  (let ((k 0))
    (declare (type fixnum k))
    (setf (aref v 0) 0
          (aref z 0) most-negative-double-float
          (aref z 1) most-positive-double-float)
    (loop for q of-type fixnum from 1 below n
          do (let ((height (+ (aref f q) (* (float q 1d0) (float q 1d0)))))
               (declare (type double-float height))
               (loop
                 (let* ((p (aref v k))
                        (crossing (/ (- height (+ (aref f p) (* (float p 1d0) (float p 1d0))))
                                     (* 2d0 (float (- q p) 1d0)))))
                   (declare (type fixnum p) (type double-float crossing))
                   (cond ((<= crossing (aref z k)) (decf k))
                         (t (incf k)
                            (setf (aref v k) q
                                  (aref z k) crossing
                                  (aref z (1+ k)) most-positive-double-float)
                            (return)))))))
    (setf k 0)
    (dotimes (q n d)
      (let ((place (float q 1d0)))
        (loop while (< (aref z (1+ k)) place) do (incf k))
        (let* ((p (aref v k))
               (run (- place (float p 1d0))))
          (setf (aref d q) (+ (* run run) (aref f p))))))))

(defun squared-distances! (grid w h)
  "GRID, W x H doubles, 0 at a site and huge elsewhere, made the squared
distance from each cell to the nearest site: along the columns, then the
rows (Felzenszwalb and Huttenlocher)."
  (declare (type (simple-array double-float (*)) grid) (type fixnum w h))
  (flet ((lines (count length stride step)
           (declare (type fixnum count length stride step))
           (parallel-ranges
            count
            (lambda (start end)
              (let ((f (make-array length :element-type 'double-float))
                    (d (make-array length :element-type 'double-float))
                    (v (make-array length :element-type 'fixnum))
                    (z (make-array (1+ length) :element-type 'double-float)))
                (loop for line from start below end
                      do (let ((base (* line step)))
                           (dotimes (i length)
                             (setf (aref f i) (aref grid (+ base (* i stride)))))
                           (parabola-envelope! f d v z length)
                           (dotimes (i length)
                             (setf (aref grid (+ base (* i stride))) (aref d i))))))))))
    (lines w h w 1)
    (lines h w 1 w)
    grid))

(defun selected-byte? (byte) (>= byte 128))

(defun distances-to (mask w region site?)
  "The squared distance across REGION (x y w h) of a MASK W wide from
each pixel to the nearest one whose byte satisfies SITE?."
  (destructuring-bind (x0 y0 pw ph) region
    (let ((grid (make-array (* pw ph) :element-type 'double-float)))
      (dotimes (y ph)
        (dotimes (x pw)
          (setf (aref grid (+ x (* y pw)))
                (if (funcall site? (aref mask (+ x0 x (* (+ y0 y) w)))) 0d0 1d20))))
      (squared-distances! grid pw ph))))

(defun region-around (rect margin w h)
  "RECT grown by MARGIN on every side, inside W x H."
  (destructuring-bind (x y rw rh) rect
    (let ((x0 (max 0 (- x margin))) (y0 (max 0 (- y margin)))
          (x1 (min w (+ x rw margin))) (y1 (min h (+ y rh margin))))
      (list x0 y0 (- x1 x0) (- y1 y0)))))

(defun contract-selection! (doc pixels)
  "The selection's edge moved PIXELS in; nothing without a selection.
The picture's edge is no edge of the selection: what reaches the frame
stays on it, as a subject cut by the frame must.  A soft edge comes out
hard: feather after."
  (let ((selection (the-object doc selection))
        (w (the-object doc doc-width))
        (h (the-object doc doc-height))
        (amount (max 0d0 (float pixels 1d0))))
    (when selection
      (let* ((mask (getf selection :mask))
             (region (region-around (getf selection :rect) 1 w h))
             (grid (distances-to mask w region (lambda (byte) (not (selected-byte? byte)))))
             (out (copy-seq mask)))
        (destructuring-bind (x0 y0 pw ph) region
          (dotimes (y ph)
            (dotimes (x pw)
              (let* ((i (+ x0 x (* (+ y0 y) w)))
                     (reach (- (sqrt (aref grid (+ x (* y pw)))) amount)))
                (setf (aref out i)
                      (floor (* (aref mask i) (max 0d0 (min 1d0 reach)))))))))
        (the-object doc (set-slot! :selection (make-selection out w h)))))))

(defun expand-selection! (doc pixels)
  "The selection's edge moved PIXELS out, inside the picture; nothing
without a selection."
  (let ((selection (the-object doc selection))
        (w (the-object doc doc-width))
        (h (the-object doc doc-height))
        (amount (max 0d0 (float pixels 1d0))))
    (when selection
      (let* ((mask (getf selection :mask))
             (region (region-around (getf selection :rect) (1+ (ceiling amount)) w h))
             (grid (distances-to mask w region #'selected-byte?))
             (out (copy-seq mask)))
        (destructuring-bind (x0 y0 pw ph) region
          (dotimes (y ph)
            (dotimes (x pw)
              (let* ((i (+ x0 x (* (+ y0 y) w)))
                     (reach (- (+ amount 1d0) (sqrt (aref grid (+ x (* y pw)))))))
                (unless (selected-byte? (aref mask i))
                  (setf (aref out i)
                        (max (aref mask i) (floor (* 255 (max 0d0 (min 1d0 reach)))))))))))
        (the-object doc (set-slot! :selection (make-selection out w h)))))))

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

;;; Copy, Cut and Paste.  The clipboard is the session's (document.lisp):
;;; what one tester copies another never sees.

(defun copy-selection! (layer)
  "Copy: raster LAYER's pixels inside the selection's rectangle, their
alpha times the selection, onto the session's clipboard; the whole layer without
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
        (setf (clipboard) (list :pixels pixels :w w :h h :x x :y y))
        rect))))

(defun cut-selection! (layer)
  "Cut: Copy, then Clear."
  (copy-selection! layer)
  (fill-selection! layer '(0 0 0) 1 t))

(defun paste-clipboard! (doc &optional above)
  "Paste: a new raster layer above ABOVE (or on top) holding the
clipboard's pixels where they came from, or centered when they do not
fit; the new layer's index.  Nothing without a clipboard."
  (let ((cb (clipboard)))
    (unless cb (error "The clipboard is empty."))
    (let* ((width (the-object doc doc-width))
           (height (the-object doc doc-height))
           (cw (getf cb :w)) (ch (getf cb :h))
           (fits (and (<= (+ (getf cb :x) cw) width) (<= (+ (getf cb :y) ch) height)))
           (x (if fits (getf cb :x) (floor (- width cw) 2)))
           (y (if fits (getf cb :y) (floor (- height ch) 2)))
           (pixels (make-rgba width height)))
      (dotimes (row ch)
        (let ((py (+ y row)))
          (when (and (<= 0 py) (< py height))
            (let ((x0 (max 0 x)) (x1 (min width (+ x cw))))
              (when (< x0 x1)
                (replace pixels (getf cb :pixels)
                         :start1 (* 4 (+ x0 (* width py)))
                         :start2 (* 4 (+ (- x0 x) (* cw row)))
                         :end2 (* 4 (+ (- x1 x) (* cw row)))))))))
      (the-object doc (add-layer! (plist-merge (list :pixels (doc-pixels doc pixels)) (the-object doc (new-layer-spec :raster)))
                                  above)))))

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
        (destructuring-bind (r g b) (stroke-color-linear (list :color (doc-color doc color)))
          (composite-stroke! pixels width coverage x y w h r g b
                             (max 0d0 (min 1d0 (float opacity 1d0))) erase?))
        (the-object layer (set-slot! :paint-version (1+ (the-object layer paint-version))))
        rect))))

;;; Cut Out.  A pixel the selection covers only partly is a mix of the
;;; subject and of what was behind it, and Copy takes the mix as it is:
;;; on a new background the old one shows as a rim.  Cut Out estimates
;;; the subject's own color there by blur fusion (Forte and Pitie, 2021):
;;; local means of subject and background, weighted by the selection and
;;; by its complement, corrected toward the pixel, in a wide pass and
;;; then a narrow one.  Then, within a few pixels of the edge, the color
;;; is drawn from deeper inside, which also takes away the photograph's
;;; own sharpening rim.  All in linear light, one channel at a time, over
;;; the selection's rectangle and a margin.

(defparameter *fusion-radii* '(45 3)
  "Blur fusion's box radii: the wide pass, then the narrow.")

(defun box-sums (src w h r)
  "The sum of SRC (W x H single floats) over the square of radius R
around each pixel, clipped to the rectangle: rows, then columns."
  (declare (type (simple-array single-float (*)) src) (type fixnum w h r))
  (let ((rows (make-plane (* w h)))
        (out (make-plane (* w h))))
    (flet ((pass (from to count length stride step)
             (declare (type (simple-array single-float (*)) from to)
                      (type fixnum count length stride step))
             (parallel-ranges
              count
              (lambda (start end)
                (loop for line of-type fixnum from start below end
                      do (let ((base (* line step)) (sum 0d0))
                           (declare (type fixnum base) (type double-float sum))
                           (dotimes (i (min r length))
                             (incf sum (aref from (+ base (* i stride)))))
                           (dotimes (i length)
                             (let ((add (+ i r)) (drop (- i r 1)))
                               (when (< add length) (incf sum (aref from (+ base (* add stride)))))
                               (when (>= drop 0) (decf sum (aref from (+ base (* drop stride)))))
                               (setf (aref to (+ base (* i stride))) (float sum 1f0))))))))))
      (pass src rows h w 1 w)
      (pass rows out w h w 1))
    out))

(defun fusion-pass (image alpha alpha-sums f b w h r)
  "One pass of blur fusion over one channel, W x H: the subject's color
F and the background's B re-estimated as their means over the square of
radius R, weighted by ALPHA and by its complement, each moved toward
IMAGE by its share of what the pair fails to explain.  ALPHA-SUMS is
ALPHA's box sums at R.  Fresh planes for F and B."
  (declare (type (simple-array single-float (*)) image alpha alpha-sums f b)
           (type fixnum w h r))
  (let* ((n (* w h)) (af (make-plane n)) (ab (make-plane n)))
    (declare (type (simple-array single-float (*)) af ab))
    (dotimes (k n)
      (let ((a (aref alpha k)))
        (setf (aref af k) (* a (aref f k))
              (aref ab k) (* (- 1f0 a) (aref b k)))))
    (let ((sf (box-sums af w h r)) (sb (box-sums ab w h r)))
      (declare (type (simple-array single-float (*)) sf sb))
      (parallel-ranges
       h
       (lambda (start end)
         (loop for y of-type fixnum from start below end
               do (let ((rows (float (- (min (1- h) (+ y r)) (max 0 (- y r)) -1) 1d0)))
                    (dotimes (x w)
                      (let* ((k (+ x (* y w)))
                             (count (* rows (float (- (min (1- w) (+ x r)) (max 0 (- x r)) -1) 1d0)))
                             (a (float (aref alpha k) 1d0))
                             (in (float (aref alpha-sums k) 1d0))
                             (mf (/ (aref sf k) (+ in 1d-5)))
                             (mb (/ (aref sb k) (+ (- count in) 1d-5)))
                             (e (- (aref image k) (* a mf) (* (- 1d0 a) mb))))
                        (setf (aref af k) (float (max 0d0 (min 1d0 (+ mf (* a e)))) 1f0)
                              (aref ab k) (float (max 0d0 (min 1d0 (+ mb (* (- 1d0 a) e)))) 1f0))))))))
      (values af ab))))

(defun defringe! (colors mask width region depth)
  "COLORS, three planes over REGION of a MASK WIDTH wide, drawn toward a
blur of the colors lying DEPTH and more inside the selection's edge:
wholly at the edge, not at all DEPTH in.  The weight each pixel kept of
its own color, a plane."
  (destructuring-bind (x0 y0 pw ph) region
    (declare (ignore x0 y0))
    (let* ((n (* pw ph))
           (grid (distances-to mask width region (lambda (byte) (< byte 243))))
           (kept (make-plane n))
           (packed (make-plane (* 4 n))))
      (declare (type (simple-array double-float (*)) grid)
               (type (simple-array single-float (*)) kept packed))
      (dotimes (k n)
        (let ((u (min 1d0 (/ (sqrt (aref grid k)) depth))))
          (setf (aref kept k) (float u 1f0))
          (when (>= u 1d0)
            (loop for channel from 0 for plane in colors
                  do (setf (aref packed (+ channel (* 4 k))) (aref plane k)))
            (setf (aref packed (+ 3 (* 4 k))) 1f0))))
      (let ((deeper (gaussian-blur-premul packed pw ph depth)))
        (declare (type (simple-array single-float (*)) deeper))
        (dotimes (k n)
          (let ((u (aref kept k)) (weight (aref deeper (+ 3 (* 4 k)))))
            (when (and (< u 1f0) (> weight 1f-3))
              (loop for channel from 0 for plane in colors
                    do (let ((plane plane))
                         (declare (type (simple-array single-float (*)) plane))
                         (setf (aref plane k)
                               (+ (* u (aref plane k))
                                  (* (- 1f0 u) (/ (aref deeper (+ channel (* 4 k))) weight))))))))))
      kept)))

(defun cut-out-selection! (layer &key name (defringe 3))
  "Cut Out (the API's, no menu's): raster LAYER's pixels inside the
selection as a new raster layer on top, their alpha times the
selection's; where the selection covers a pixel only partly, or within
DEFRINGE pixels of its edge, the subject's own color in place of the mix
with what was behind it.  Elsewhere the pixels are LAYER's own,
unchanged.  The new layer's index."
  (unless (eql (the-object layer kind) :raster)
    (error "~a is not a raster layer." (the-object layer layer-name)))
  (unless (and (realp defringe) (<= 0 defringe 20))
    (error "Defringe is a number of pixels from 0 to 20, not ~s." defringe))
  (let* ((doc (the-object layer parent))
         (width (the-object doc doc-width))
         (height (the-object doc doc-height))
         (selection (or (the-object doc selection)
                        (error "Nothing is selected: select the subject before cutting it out.")))
         (mask (getf selection :mask))
         (source (the-object layer pixels))
         (decode *srgb-decode*)
         (depth (float defringe 1d0))
         (region (region-around (getf selection :rect) (first *fusion-radii*) width height)))
    (declare (type (simple-array (unsigned-byte 8) (*)) mask source)
             (type (simple-array double-float (256)) decode))
    (destructuring-bind (x0 y0 pw ph) region
      (let* ((n (* pw ph))
             (alpha (make-plane n))
             (colors (list (make-plane n) (make-plane n) (make-plane n))))
        (macrolet ((over-region ((k i) &body body)
                     `(dotimes (y ph)
                        (dotimes (x pw)
                          (let ((,k (+ x (* y pw))) (,i (+ x0 x (* width (+ y0 y)))))
                            ,@body)))))
          (over-region (k i) (setf (aref alpha k) (/ (aref mask i) 255f0)))
          (destructuring-bind (wide narrow) *fusion-radii*
            (let ((wide-sums (box-sums alpha pw ph wide))
                  (narrow-sums (box-sums alpha pw ph narrow)))
              (loop for channel from 0 for out in colors
                    do (let ((image (make-plane n)))
                         (over-region (k i)
                           (setf (aref image k)
                                 (float (aref decode (aref source (+ channel (* 4 i)))) 1f0)))
                         (multiple-value-bind (f b) (fusion-pass image alpha wide-sums image image pw ph wide)
                           (replace out (fusion-pass image alpha narrow-sums f b pw ph narrow)))))))
          (let ((kept (when (plusp depth) (defringe! colors mask width region depth)))
                (pixels (make-rgba width height)))
            (over-region (k i)
              (let ((s (aref mask i)) (o (* 4 i)))
                (cond ((zerop s))
                      ((and (= s 255) (or (null kept) (>= (aref kept k) 1f0)))
                       (replace pixels source :start1 o :start2 o :end2 (+ o 4)))
                      (t
                       (loop for channel from 0 for plane in colors
                             do (setf (aref pixels (+ o channel))
                                      (unit->byte (linear->srgb (float (aref plane k) 1d0)))))
                       (setf (aref pixels (+ o 3))
                             (unit->byte (* (/ s 255d0) (/ (aref source (+ o 3)) 255d0))))))))
            (let ((spec (the-object doc (new-layer-spec :raster))))
              (when name (setf (getf spec :name) name))
              (the-object doc (add-layer! (plist-merge (list :pixels (doc-pixels doc pixels)) spec))))))))))

;;; Refine Edge (the API's, no menu's): a selection drawn roughly, as a
;;; polygon by eye, moved onto the subject's own outline.  Within RADIUS
;;; pixels either side of its edge every pixel is judged by its color,
;;; against the colors near it that lie surely inside (deeper in than
;;; RADIUS) and surely outside (farther out): its nearest samples in
;;; color vote, so a side wins by having more of that color nearby, not
;;; by having it at all.  A polygon that took in a strip of the
;;; background puts some of it among the sure inside's samples, and a
;;; nearest-color test then ties; a vote does not.  A slight lean toward
;;; the rough edge settles what color cannot.  Then the judgment is
;;; smoothed, pieces cut off from the sure inside are dropped and small
;;; holes in it filled, and the whole is done again around the new edge,
;;; pass after pass, so that the edge can travel farther than RADIUS and
;;; what one pass put outside stops voting for the inside.  The edge
;;; comes out hard: feather after.

(defparameter *refine-samples* 25
  "Sample places across a window's side at most: a wider window takes
every so many pixels.")

(defparameter *refine-votes* 9
  "How many of the nearest samples in color vote on a pixel's side.")

(defparameter *refine-lean* 0.15d0
  "How far the judgment leans toward the rough edge: the share of the
way to certain at the band's border.")

(defun edge-depths (mask width region)
  "Over REGION of MASK (WIDTH wide), each pixel's distance across the
selection's edge, positive inside and negative outside: a plane of
doubles."
  (let ((inside (distances-to mask width region (lambda (byte) (not (selected-byte? byte)))))
        (outside (distances-to mask width region #'selected-byte?)))
    (declare (type (simple-array double-float (*)) inside outside))
    (dotimes (k (length inside) inside)
      (setf (aref inside k) (- (sqrt (aref inside k)) (sqrt (aref outside k)))))))

(defun refine-judgment (colors depth pw ph radius)
  "Each pixel's side over a PW x PH region, a byte plane, 1 inside and 0
outside: the sure pixels' by their DEPTH, the band's by their COLORS
(four bytes a pixel) against the sure ones within twice RADIUS."
  (declare (type (simple-array (unsigned-byte 8) (*)) colors)
           (type (simple-array double-float (*)) depth)
           (type fixnum pw ph)
           (type double-float radius))
  (let* ((side (make-array (* pw ph) :element-type '(unsigned-byte 8)))
         (reach (ceiling (* 2 radius)))
         (step (max 1 (ceiling (1+ (* 2 reach)) *refine-samples*)))
         (lean *refine-lean*))
    (declare (type fixnum reach step) (type double-float lean))
    (parallel-ranges
     ph
     (lambda (start end)
       (let* ((votes *refine-votes*)
              (near (make-array votes :element-type 'double-float))
              (inside (make-array votes :element-type 'bit)))
         (declare (type fixnum votes))
         (flet ((keep! (d in)
                  ;; D among the VOTES nearest so far, in order, with its side
                  (declare (type double-float d) (type bit in))
                  (when (< d (aref near (1- votes)))
                    (let ((j (1- votes)))
                      (declare (type fixnum j))
                      (loop while (and (> j 0) (< d (aref near (1- j))))
                            do (setf (aref near j) (aref near (1- j))
                                     (aref inside j) (aref inside (1- j)))
                               (decf j))
                      (setf (aref near j) d (aref inside j) in)))))
           (loop for y of-type fixnum from start below end
                 do (dotimes (x pw)
                      (let* ((k (+ x (* y pw))) (s (aref depth k)))
                        (declare (type fixnum k) (type double-float s))
                        (cond ((> s radius) (setf (aref side k) 1))
                              ((< s (- radius)) (setf (aref side k) 0))
                              (t
                               (fill near 1d31)
                               (let ((o (* 4 k)) (seen-in nil) (seen-out nil))
                                 (declare (type fixnum o))
                                 (loop for sy of-type fixnum from (max 0 (- y reach)) to (min (1- ph) (+ y reach)) by step
                                       do (loop for sx of-type fixnum from (max 0 (- x reach)) to (min (1- pw) (+ x reach)) by step
                                                do (let* ((j (+ sx (* sy pw))) (there (aref depth j)))
                                                     (declare (type fixnum j) (type double-float there))
                                                     (when (> (abs there) radius)
                                                       (let ((d 0d0) (p (* 4 j)))
                                                         (declare (type double-float d) (type fixnum p))
                                                         (dotimes (c 4)
                                                           (let ((e (float (- (aref colors (+ o c)) (aref colors (+ p c))) 1d0)))
                                                             (incf d (* e e))))
                                                         (if (plusp there) (setf seen-in t) (setf seen-out t))
                                                         (keep! d (if (plusp there) 1 0)))))))
                                 (setf (aref side k)
                                       (if (and seen-in seen-out)
                                           (let* ((n (count-if (lambda (d) (< d 1d30)) near))
                                                  (ins (count 1 inside :end n)))
                                             (if (> (+ (/ ins (float n 1d0)) (* lean (/ s radius))) 0.5d0) 1 0))
                                           (if (plusp s) 1 0)))))))))))))
    side))

(defun smooth-sides (side depth pw ph radius)
  "SIDE with each band pixel (DEPTH within RADIUS) the majority of the
three by three around it: fresh bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) side)
           (type (simple-array double-float (*)) depth)
           (type fixnum pw ph) (type double-float radius))
  (let ((out (copy-seq side)))
    (dotimes (y ph out)
      (dotimes (x pw)
        (let ((k (+ x (* y pw))))
          (when (<= (abs (aref depth k)) radius)
            (let ((count 0) (total 0))
              (declare (type fixnum count total))
              (loop for sy from (max 0 (1- y)) to (min (1- ph) (1+ y))
                    do (loop for sx from (max 0 (1- x)) to (min (1- pw) (1+ x))
                             do (incf total)
                                (incf count (aref side (+ sx (* sy pw))))))
              (setf (aref out k) (if (> (* 2 count) total) 1 0)))))))))

(defun connected-pieces (side pw ph value)
  "The 4-connected pieces of the pixels of SIDE equal to VALUE: a label
plane (-1 for the others) and the count of pieces."
  (declare (type (simple-array (unsigned-byte 8) (*)) side) (type fixnum pw ph))
  (let ((labels (make-array (* pw ph) :element-type 'fixnum :initial-element -1))
        (stack (make-array 1024 :element-type 'fixnum :adjustable t :fill-pointer 0))
        (count 0))
    (declare (type fixnum count))
    (dotimes (seed (* pw ph))
      (when (and (= (aref side seed) value) (minusp (aref labels seed)))
        (setf (aref labels seed) count)
        (vector-push-extend seed stack)
        (loop while (plusp (fill-pointer stack))
              do (let* ((i (vector-pop stack)) (x (mod i pw)) (y (floor i pw)))
                   (flet ((visit (j)
                            (when (and (= (aref side j) value) (minusp (aref labels j)))
                              (setf (aref labels j) count)
                              (vector-push-extend j stack))))
                     (when (> x 0) (visit (1- i)))
                     (when (< x (1- pw)) (visit (1+ i)))
                     (when (> y 0) (visit (- i pw)))
                     (when (< y (1- ph)) (visit (+ i pw))))))
        (incf count)))
    (values labels count)))

(defun tidy-sides! (side depth pw ph radius)
  "SIDE with every inside piece that holds no sure inside pixel put
outside, and every outside piece put inside that holds no sure outside
pixel, does not reach the region's border and is smaller than RADIUS
squared (at least 16 pixels): a speck the colors misjudged."
  (declare (type (simple-array (unsigned-byte 8) (*)) side)
           (type (simple-array double-float (*)) depth)
           (type fixnum pw ph) (type double-float radius))
  (flet ((settle (value keep?)
           (multiple-value-bind (labels count) (connected-pieces side pw ph value)
             (declare (type (simple-array fixnum (*)) labels))
             (let ((sizes (make-array count :element-type 'fixnum :initial-element 0))
                   (anchored (make-array count :initial-element nil)))
               (dotimes (y ph)
                 (dotimes (x pw)
                   (let* ((k (+ x (* y pw))) (piece (aref labels k)))
                     (unless (minusp piece)
                       (incf (aref sizes piece))
                       (when (funcall keep? (aref depth k) x y)
                         (setf (aref anchored piece) t))))))
               (dotimes (k (* pw ph))
                 (let ((piece (aref labels k)))
                   (unless (or (minusp piece) (aref anchored piece))
                     (when (or (= value 1) (< (aref sizes piece) (max 16 (* radius radius))))
                       (setf (aref side k) (- 1 value))))))))))
    (settle 1 (lambda (s x y) (declare (ignore x y)) (> s radius)))
    (settle 0 (lambda (s x y) (or (< s (- radius)) (= x 0) (= y 0) (= x (1- pw)) (= y (1- ph)))))
    side))

(defun refine-selection! (doc &key (radius 8) (passes 5) layer)
  "The selection's edge moved onto the subject's outline by color,
within RADIUS pixels either side of where it is, judged on raster
LAYER's pixels or on the composited picture (nil, or any other layer).
Each of PASSES judges again around the edge the last one left, so the
edge may move as far as RADIUS times PASSES, and what a pass puts
outside no longer speaks for the inside in the next.  The selection
comes out hard."
  (unless (and (realp radius) (<= 1 radius 100))
    (error "A refine radius is a number of pixels from 1 to 100, not ~s." radius))
  (unless (and (integerp passes) (<= 1 passes 10))
    (error "Refine passes are a whole number from 1 to 10, not ~s." passes))
  (let* ((selection (or (the-object doc selection)
                        (error "Nothing is selected: draw the subject's outline roughly first, then refine it.")))
         (w (the-object doc doc-width))
         (h (the-object doc doc-height))
         (r (float radius 1d0))
         (mask (copy-seq (getf selection :mask)))
         (pixels (if (and layer (eql (the-object layer kind) :raster))
                     (the-object layer pixels)
                     (the-object doc (render-cpu))))
         (region (region-around (getf selection :rect) (ceiling (* (+ passes 2) r)) w h)))
    (declare (type (simple-array (unsigned-byte 8) (*)) mask pixels))
    (destructuring-bind (x0 y0 pw ph) region
      (let ((colors (make-array (* 4 pw ph) :element-type '(unsigned-byte 8))))
        (dotimes (y ph)
          (let ((from (* 4 (+ x0 (* (+ y0 y) w)))))
            (replace colors pixels :start1 (* 4 y pw) :start2 from :end2 (+ from (* 4 pw)))))
        (loop repeat passes
              do (let* ((depth (edge-depths mask w region))
                        (side (tidy-sides! (smooth-sides (smooth-sides (refine-judgment colors depth pw ph r)
                                                                       depth pw ph r)
                                                         depth pw ph r)
                                           depth pw ph r))
                        (moved 0))
                   (declare (type fixnum moved))
                   (dotimes (y ph)
                     (dotimes (x pw)
                       (let ((i (+ x0 x (* w (+ y0 y))))
                             (byte (* 255 (aref side (+ x (* y pw))))))
                         (unless (= byte (aref mask i))
                           (incf moved)
                           (setf (aref mask i) byte)))))
                   (when (zerop moved) (loop-finish))))
        (the-object doc (set-slot! :selection (make-selection mask w h)))))))
