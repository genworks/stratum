;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; The Lisp half of the regression suite.  With Stratum loaded:
;;;
;;;   (load "/projects/gw/stratum/tests/run.lisp")
;;;   (stratum::run-tests)            ; => T when every check passed
;;;
;;; The golden checksums in golden.sexp are FNV-1a over the CPU renders
;;; of the sample documents, and must come out the same on every
;;; implementation.  A change that is MEANT to alter the pictures
;;; regenerates them with (stratum::record-golden) and says so in its
;;; commit; any other change that moves them is a regression.
;;;

(in-package :stratum)

(defparameter *tests-dir*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defparameter *golden-file* (merge-pathnames "golden.sexp" *tests-dir*))

;;; "parity+stroke" is the parity document with *test-stroke* painted on
;;; its first layer and erased, wider, from its Discs layer: the brush's
;;; own golden, so the sample pictures' checksums never move with it.
(defparameter *golden-documents* '("parity" "demo" "parity+stroke"))

(defparameter *test-stroke*
  '(:points ((40 50 0.5) (120 90 1.0) (200 100 0.8) (260 60 0.3))
    :radius 12 :hardness 0.7 :spacing 0.25 :flow 0.9 :opacity 0.85
    :color (230 80 70) :mode :paint))

(defvar *checks* nil)

(defun check (name pass &optional detail)
  (push (list (if pass :pass :fail) name detail) *checks*)
  pass)

(defun fnv1a (octets)
  "FNV-1a, 32 bits, over OCTETS."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (let ((h 2166136261))
    (declare (type (unsigned-byte 32) h))
    (loop for b across octets
          do (setf h (logand #xffffffff (* (logxor h b) 16777619))))
    h))

(defun render-checksum (name)
  (let ((doc (make-sample-document (if (string= name "parity+stroke") "parity" name))))
    (when (string= name "parity+stroke")
      (apply-brush-stroke (the-object doc (layer 0)) *test-stroke*)
      (apply-brush-stroke (the-object doc (layer 4)) (list* :mode :erase :radius 30 *test-stroke*)))
    (fnv1a (the-object doc (render-cpu)))))

(defun record-golden ()
  "Write golden.sexp from the current renders.  Only for a change meant
to alter the pictures."
  (let ((golden (loop for name in *golden-documents*
                      collect (cons name (render-checksum name)))))
    (with-open-file (s *golden-file* :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-pretty* t)) (pprint golden s))))
    golden))

(defun counts (doc) (coerce (the-object doc build-counts) 'list))

(defun demand (doc)
  "Ask for both programs and one render, as the page and an export would."
  (the-object doc shader-source)
  (the-object doc (render-cpu))
  (counts doc))

(defun test-rebuilds ()
  (let ((doc (make-sample-document "parity")))
    (check "first demand builds each program once" (equal (demand doc) '(1 1)) (counts doc))
    (the-object doc (layer 1) (set-slot! :opacity 0.3d0))
    (check "an opacity edit rebuilds nothing" (equal (demand doc) '(1 1)) (counts doc))
    (the-object doc (layer 2) (set-slot! :master '((0 0) (0.5 0.7) (1 1))))
    (check "a curve edit rebuilds nothing" (equal (demand doc) '(1 1)) (counts doc))
    (the-object doc (layer 0) (set-slot! :visible? nil))
    (check "a visibility edit rebuilds nothing" (equal (demand doc) '(1 1)) (counts doc))
    (the-object doc (layer 1) (set-slot! :blend-mode :add))
    (check "a blend edit rebuilds each program once" (equal (demand doc) '(2 2)) (counts doc))
    (check "the shader has one block per layer"
           (= (length (the-object doc layer-list))
              (loop with s = (the-object doc shader-source)
                    for start = 0 then (1+ pos)
                    for pos = (search "// layer " s :start2 start)
                    while pos count t)))))

(defun test-layer-types ()
  (let* ((doc (make-sample-document "parity"))
         (kinds (mapcar (lambda (s) (getf s :kind :raster)) (the-object doc layer-specs)))
         (types (mapcar #'type-of (the-object doc layer-list))))
    (check "each spec's kind picks its layer type"
           (every (lambda (k ty) (eql ty (ecase k
                                           (:raster 'raster-layer)
                                           (:curves 'curves-layer)
                                           (:levels 'levels-layer)
                                           (:hue-saturation 'hue-saturation-layer))))
                  kinds types)
           types)
    (the-object doc (set-slot! :layer-specs (reverse (the-object doc layer-specs))))
    (check "new specs re-dispatch the types"
           (equal (mapcar #'type-of (the-object doc layer-list)) (reverse types))
           (mapcar #'type-of (the-object doc layer-list)))))

(defun lut-monotone-p (lut)
  (loop for i from 1 below (length lut) always (>= (aref lut i) (aref lut (1- i)))))

(defun test-curves ()
  (let ((identity (channel-lut *identity-curve* *identity-curve*)))
    (check "the identity curve is the identity table"
           (loop for i below 256 always (< (abs (- (aref identity i) (/ i 255d0))) 1d-12))))
  (let ((steep (channel-lut '((0 0) (0.5 0.9) (0.55 0.95) (1 1)) *identity-curve*)))
    (check "a steep curve stays monotone and in [0,1]"
           (and (lut-monotone-p steep) (<= 0 (reduce #'min steep)) (<= (reduce #'max steep) 1))))
  (let ((s-curve (channel-lut '((0 0) (0.25 0.18) (0.75 0.85) (1 1))
                              '((0 0) (0.5 0.6) (1 1)))))
    (check "master then channel stays monotone" (lut-monotone-p s-curve)))
  (check "normalize-points sorts, clamps and drops repeated x"
         (equal (normalize-points '((1 1) (0.5 2) (0 0) (0.5 0.2)))
                '((0d0 0d0) (0.5d0 1d0) (1d0 1d0)))
         (normalize-points '((1 1) (0.5 2) (0 0) (0.5 0.2))))
  (check "fewer than two points is the identity curve"
         (equal (normalize-points '((0.3 0.3))) *identity-curve*)))

(defun test-adjustments ()
  (let ((identity (levels-lut 0d0 1d0 1d0 0d0 1d0 256)))
    (check "neutral levels are the identity table"
           (loop for i below 256 always (< (abs (- (aref identity i) (/ i 255d0))) 1d-12))))
  (let ((lifted (levels-lut 0.1d0 0.9d0 1.5d0 0d0 1d0 256)))
    (check "levels clip below in-black and above in-white, and stay monotone"
           (and (zerop (aref lifted 0)) (= 1 (aref lifted 255))
                (zerop (aref lifted 25)) (= 1 (aref lifted 230))
                (loop for i from 1 below 256 always (>= (aref lifted i) (aref lifted (1- i)))))))
  (check "neutral hue/saturation leaves colors alone"
         (loop for (r g b) in '((0.2d0 0.5d0 0.8d0) (0.9d0 0.1d0 0.1d0) (0.5d0 0.5d0 0.5d0)
                                (1d0 1d0 0d0) (0d0 0d0 0d0))
               always (multiple-value-bind (nr ng nb) (hue-saturation r g b 0d0 0d0 0d0)
                        (every (lambda (x y) (< (abs (- x y)) 1d-12))
                               (list nr ng nb) (list r g b)))))
  (check "a full turn of hue leaves colors alone"
         (multiple-value-bind (nr ng nb) (hue-saturation 0.2d0 0.5d0 0.8d0 1d0 0d0 0d0)
           (every (lambda (x y) (< (abs (- x y)) 1d-12)) (list nr ng nb) '(0.2d0 0.5d0 0.8d0))))
  (check "saturation -1 makes gray"
         (multiple-value-bind (nr ng nb) (hue-saturation 0.2d0 0.5d0 0.8d0 0d0 -1d0 0d0)
           (and (= nr ng nb) (< (abs (- nr 0.5d0)) 1d-12))))
  (let ((doc (make-sample-document "parity")))
    (demand doc)
    (let ((levels (find :levels (the-object doc layer-list) :key (lambda (l) (the-object l kind))))
          (hue-sat (find :hue-saturation (the-object doc layer-list)
                         :key (lambda (l) (the-object l kind)))))
      (the-object levels (set-slot! :gamma 1.6d0))
      (the-object hue-sat (set-slot! :hue -30d0))
      (check "levels and hue/saturation edits rebuild nothing" (equal (demand doc) '(1 1))
             (counts doc)))))

(defun test-golden ()
  (let ((golden (and (probe-file *golden-file*)
                     (with-open-file (s *golden-file*)
                       (with-standard-io-syntax (read s))))))
    (if (null golden)
        (check "golden.sexp exists" nil "run (stratum::record-golden) once, deliberately")
        (dolist (name *golden-documents*)
          (let ((want (cdr (assoc name golden :test #'string=)))
                (got (render-checksum name)))
            (check (format nil "CPU render of ~a matches its golden checksum" name)
                   (eql want got) (list :want want :got got)))))))

(defun test-rect ()
  (let* ((w 7) (h 5)
         (pixels (let ((p (make-rgba w h)))
                   (dotimes (i (length p) p) (setf (aref p i) (mod i 251)))))
         (rect (rgba-rect pixels w h 2 1 3 2)))
    (check "rgba-rect copies the rectangle, rows from the top"
           (equalp rect (concatenate '(vector (unsigned-byte 8))
                                     (subseq pixels (* 4 (+ 2 (* 7 1))) (* 4 (+ 5 (* 7 1))))
                                     (subseq pixels (* 4 (+ 2 (* 7 2))) (* 4 (+ 5 (* 7 2)))))))
    (check "rgba-rect refuses a rectangle outside the image"
           (handler-case (progn (rgba-rect pixels w h 5 0 3 1) nil)
             (error () t)))
    (check "rgba-thumb takes the source pixel under each thumbnail pixel's center"
           (let ((thumb (rgba-thumb pixels w h 2 1)))
             (equalp thumb (concatenate '(vector (unsigned-byte 8))
                                        (subseq pixels (* 4 (+ 1 (* 7 2))) (* 4 (+ 2 (* 7 2))))
                                        (subseq pixels (* 4 (+ 5 (* 7 2))) (* 4 (+ 6 (* 7 2)))))))
           (rgba-thumb pixels w h 2 1))))

(defun stroke-vectors-of (points)
  (multiple-value-list (stroke-vectors points)))

(defun test-brush ()
  ;; Placement: a straight 100-pixel stroke at full pressure, radius 10,
  ;; spacing a quarter: a dab every 2.5 pixels from the start, 41 of them.
  (destructuring-bind (xs ys ps) (stroke-vectors-of '((0 0 1) (100 0 1)))
    (let* ((count (place-dabs xs ys ps 2 10d0 0.25d0 (make-array 0 :element-type 'double-float)))
           (out (make-array (* 3 count) :element-type 'double-float)))
      (place-dabs xs ys ps 2 10d0 0.25d0 out)
      (check "dabs fall every spacing * radius along a straight stroke, the first at its start"
             (and (= count 41)
                  (loop for k below count
                        always (and (< (abs (- (aref out (* 3 k)) (* k 2.5d0))) 1d-9)
                                    (= (aref out (+ 1 (* 3 k))) 0d0))))
             count)))
  ;; One dab: a hard dab of radius 2 on the center of a 5 x 5 buffer.
  (let ((buf (make-array 25 :element-type 'double-float :initial-element 0d0)))
    (paint-dab buf 5 5 0 0 2.5d0 2.5d0 2d0 1d0 1d0)
    (check "a hard dab covers its center fully, the corners not at all, its edge in part, symmetrically"
           (and (= (aref buf 12) 1d0) (= (aref buf 0) 0d0) (= (aref buf 24) 0d0)
                (= 1d0 (aref buf 7) (aref buf 17) (aref buf 11) (aref buf 13))
                (= (aref buf 6) (aref buf 8) (aref buf 16) (aref buf 18))
                (< 0d0 (aref buf 6) 1d0))
           (coerce buf 'list)))
  (let ((buf (make-array 25 :element-type 'double-float :initial-element 0d0)))
    (paint-dab buf 5 5 0 0 2.5d0 2.5d0 2d0 1d0 0.5d0)
    (paint-dab buf 5 5 0 0 2.5d0 2.5d0 2d0 1d0 0.5d0)
    (check "flow builds coverage toward full and never past it"
           (and (= (aref buf 12) 0.75d0) (<= (reduce #'max buf) 1d0))
           (aref buf 12)))
  ;; The offset form the server uses (a buffer covering one rectangle)
  ;; paints the same numbers as the browser's whole-document buffer.
  (let ((whole (make-array 400 :element-type 'double-float :initial-element 0d0))
        (part (make-array 64 :element-type 'double-float :initial-element 0d0)))
    (paint-dab whole 20 20 0 0 9.3d0 7.1d0 3d0 0.6d0 1d0)
    (paint-dab part 8 8 5 4 9.3d0 7.1d0 3d0 0.6d0 1d0)
    (check "a dab painted into an offset rectangle equals those pixels of the whole"
           (loop for row below 8
                 always (loop for col below 8
                              always (= (aref part (+ col (* 8 row)))
                                        (aref whole (+ col 5 (* 20 (+ row 4)))))))))
  ;; A stroke on the parity document, and its undo.
  (let* ((doc (make-sample-document "parity"))
         (w (the-object doc doc-width))
         (layer (the-object doc (layer 0)))
         (before (copy-seq (the-object layer pixels)))
         (source (the-object layer source-pixels))
         (render-before (the-object doc (render-cpu))))
    (demand doc)
    (let* ((rect (apply-brush-stroke layer *test-stroke*))
           (after (copy-seq (the-object layer pixels))))
      (destructuring-bind (x y rw rh) rect
        (check "a stroke changes pixels inside its rectangle only"
               (and (not (equalp before after))
                    (loop for i below (length before)
                          for px = (mod (floor i 4) w)
                          for py = (floor (floor i 4) w)
                          always (or (and (<= x px (+ x rw -1)) (<= y py (+ y rh -1)))
                                     (= (aref before i) (aref after i)))))
               rect))
      (check "a stroke rebuilds nothing" (equal (demand doc) '(1 1)) (counts doc))
      (check "the pixel cache's copy is never painted" (equalp source before))
      (check "the CPU render sees the stroke"
             (not (equalp render-before (the-object doc (render-cpu)))))
      (check "a stroke on an adjustment layer is refused"
             (handler-case (progn (apply-brush-stroke (the-object doc (layer 2)) *test-stroke*) nil)
               (error () t)))
      (check "the same stroke on a fresh document paints the same bytes"
             (let* ((other (make-sample-document "parity"))
                    (other-layer (the-object other (layer 0))))
               (apply-brush-stroke other-layer *test-stroke*)
               (equalp (the-object other-layer pixels) after)))
      (let ((o (* 4 (+ 120 (* w 90)))))
        (apply-brush-stroke layer (list* :mode :erase *test-stroke*))
        (check "an erasing stroke lowers alpha and keeps the color"
               (and (< (aref (the-object layer pixels) (+ o 3)) (aref after (+ o 3)))
                    (= (aref (the-object layer pixels) o) (aref after o)))
               (list :alpha (aref after (+ o 3)) (aref (the-object layer pixels) (+ o 3)))))
      (check "undo reports the rectangle it put back"
             (equal (undo-stroke! doc) (list* 0 :layer rect)))
      (check "undo gives the layer back exactly"
             (and (equal (undo-stroke! doc) (list* 0 :layer rect))
                  (equalp (the-object layer pixels) before)
                  (equalp (the-object doc (render-cpu)) render-before)))
      (check "nothing is left to undo" (null (undo-stroke! doc)))
      (check "redo brings the erase and the stroke back exactly, in order"
             (let ((stroked after))
               (and (equal (redo-stroke! doc) (list* 0 :layer rect))
                    (equalp (the-object layer pixels) stroked)
                    (redo-stroke! doc)
                    (< (aref (the-object layer pixels) (+ 3 (* 4 (+ 120 (* w 90)))))
                       (aref stroked (+ 3 (* 4 (+ 120 (* w 90))))))
                    (null (redo-stroke! doc)))))
      (check "a new change after an undo empties the redo list"
             (progn (undo-stroke! doc)
                    (apply-brush-stroke layer *test-stroke*)
                    (and (null (the-object doc redo-entries))
                         (null (redo-stroke! doc)))))
      (check "undo and redo round trip through the whole list"
             (progn (loop while (undo-stroke! doc))
                    (loop while (redo-stroke! doc))
                    (and (null (the-object doc redo-entries))
                         (= 2 (length (the-object doc undo-entries)))))))))

(defun test-masks ()
  (let* ((doc (make-sample-document "parity"))
         (w (the-object doc doc-width))
         (discs (the-object doc (layer 4)))
         (sky (the-object doc (layer 0)))
         (render-before (the-object doc (render-cpu)))
         (mask-before (copy-seq (the-object discs mask-pixels))))
    (demand doc)
    (check "a stroke on a mask is refused where there is none"
           (handler-case (progn (apply-brush-stroke sky *test-stroke* :target :mask) nil)
             (error () t)))
    (let ((rect (apply-brush-stroke discs (list* :color '(0 0 0) *test-stroke*) :target :mask))
          (o (+ 120 (* w 90))))
      (check "a black stroke on a mask lowers it inside its rectangle only, and rebuilds nothing"
             (and rect
                  (< (aref (the-object discs mask-pixels) o) (aref mask-before o))
                  (equal (demand doc) '(1 1))
                  (loop for i below (length mask-before)
                        for px = (mod i w) for py = (floor i w)
                        always (or (and (<= (first rect) px (+ (first rect) (third rect) -1))
                                        (<= (second rect) py (+ (second rect) (fourth rect) -1)))
                                   (= (aref mask-before i) (aref (the-object discs mask-pixels) i)))))
             rect)
      (check "the CPU render sees the mask stroke"
             (not (equalp render-before (the-object doc (render-cpu)))))
      (check "undo gives the mask back exactly"
             (and (equal (undo-stroke! doc) (list* 4 :mask rect))
                  (equalp (the-object discs mask-pixels) mask-before)
                  (equalp (the-object doc (render-cpu)) render-before))))
    (check "Hide All on a layer without a mask makes a black one, and the programs follow"
           (progn (the-object sky (set-mask! '(:flat :value 0)))
                  (and (the-object sky has-mask?)
                       (every #'zerop (the-object sky mask-pixels))
                       (equal (demand doc) '(2 2)))))
    (check "Reveal All replaces it with a white one"
           (progn (the-object sky (set-mask! '(:flat :value 255)))
                  (every (lambda (v) (= v 255)) (the-object sky mask-pixels))))
    (check "a painted mask survives a change of the stack"
           (progn (apply-brush-stroke sky (list* :color '(0 0 0) *test-stroke*) :target :mask)
                  (let ((painted (copy-seq (the-object sky mask-pixels))))
                    (the-object doc (add-layer! (the-object doc (new-layer-spec :curves))))
                    (equalp (the-object doc (layer 0) mask-pixels) painted))))
    (check "Delete takes the mask away and renders as before"
           (progn (the-object doc (layer 0) (set-mask! nil))
                  (the-object doc (delete-layer! (1- (length (the-object doc layer-list)))))
                  (and (not (the-object doc (layer 0) has-mask?))
                       (equalp (the-object doc (render-cpu)) render-before))))))

(defun test-files ()
  (let ((image (let ((p (make-rgba 4 3)))
                 (dotimes (i (length p) p) (setf (aref p i) (+ 1 (mod i 251)))))))
    (check "fit-rgba pads a smaller image, centered, with transparency"
           (let ((out (fit-rgba image 4 3 8 5)))
             (and (= (length out) (* 4 8 5))
                  (every #'zerop (subseq out 0 (* 4 8)))
                  (equalp (subseq out (* 4 (+ 2 (* 8 1))) (* 4 (+ 6 (* 8 1)))) (subseq image 0 16))
                  (equalp (subseq out (* 4 (+ 2 (* 8 3))) (* 4 (+ 6 (* 8 3)))) (subseq image 32 48)))))
    (check "fit-rgba crops a larger image, centered"
           (let ((out (fit-rgba image 4 3 2 1)))
             (equalp out (subseq image (* 4 (+ 1 (* 4 1))) (* 4 (+ 3 (* 4 1)))))))
    (let* ((opaque (let ((p (copy-seq image)))
                     (loop for i from 3 below (length p) by 4 do (setf (aref p i) 255))
                     p))
           (doc (make-file-document "test-file" 4 3 opaque)))
      (check "an opened image is one Background layer, rendered as it came"
             (and (= 1 (length (the-object doc layer-list)))
                  (string= (the-object doc (layer 0) layer-name) "Background")
                  (equalp (the-object doc (render-cpu)) opaque))
             (list (the-object doc (render-cpu)) opaque)))))

(defun test-selection ()
  (check "a rectangle selection covers its pixels and reports its bounds"
         (let ((doc (make-sample-document "parity")))
           (select-rect! doc 10 20 30 40)
           (let* ((sel (the-object doc selection)) (mask (getf sel :mask)))
             (and (equal (getf sel :rect) '(10 20 30 40))
                  (= 255 (aref mask (+ 10 (* 320 20))) (aref mask (+ 39 (* 320 59))))
                  (= 0 (aref mask (+ 9 (* 320 20))) (aref mask (+ 40 (* 320 59))) (aref mask (+ 10 (* 320 60))))
                  (= (count 255 mask) (* 30 40))))))
  (check "a square lasso equals the same rectangle"
         (equalp (rasterize-polygon 50 50 '((10 10) (30 10) (30 25) (10 25)))
                 (rasterize-rect 50 50 10 10 20 15)))
  (check "a triangle lasso fills by pixel centers, even-odd"
         (let ((m (rasterize-polygon 20 20 '((0 0) (20 0) (0 20)))))
           (and (= 255 (aref m 0)) (= 255 (aref m (+ 5 (* 20 5)))) (= 0 (aref m (+ 19 (* 20 19))))
                (= 0 (aref m (+ 15 (* 20 15)))))))
  (let ((doc (make-sample-document "parity")))
    (check "an empty rectangle selects nothing; inverse of nothing is all"
           (progn (select-rect! doc 400 400 10 10)
                  (and (null (the-object doc selection))
                       (progn (select-inverse! doc)
                              (equal (getf (the-object doc selection) :rect) '(0 0 320 200))))))
    (check "inverse of a rectangle is everything else, and deselect clears it"
           (progn (select-rect! doc 0 0 100 200)
                  (select-inverse! doc)
                  (and (equal (getf (the-object doc selection) :rect) '(100 0 220 200))
                       (progn (select-none! doc) (null (the-object doc selection))))))
    (let* ((sky (the-object doc (layer 0)))
           (w 320)
           (before (copy-seq (the-object sky pixels))))
      (demand doc)
      (select-rect! doc 100 60 60 40)
      (check "a stroke under a selection changes pixels inside it only"
             (let ((rect (apply-brush-stroke sky *test-stroke*))
                   (after (the-object sky pixels)))
               (and rect
                    (not (equalp before after))
                    (loop for i below (length before)
                          for px = (mod (floor i 4) w) for py = (floor (floor i 4) w)
                          always (or (and (<= 100 px 159) (<= 60 py 99))
                                     (= (aref before i) (aref after i)))))))
      (check "clipping is the same arithmetic as the browser's"
             (let ((cov (make-array 4 :element-type 'double-float :initial-contents '(1d0 0.5d0 0.25d0 1d0))))
               (clip-coverage! cov 100 60 2 2 (the-object doc selection) w)
               (every (lambda (c s) (= c (* c (/ (float s 1d0) 255d0))))
                      cov (list 255 255 255 255))))
      (check "Fill paints the selection only, Clear empties it, undo gives both back"
             (let ((white (progn (fill-selection! sky '(255 255 255) 1) (copy-seq (the-object sky pixels)))))
               (and (= 255 (aref white (* 4 (+ 120 (* w 80)))) (aref white (+ 1 (* 4 (+ 120 (* w 80))))))
                    (= (aref white (* 4 (+ 50 (* w 80)))) (aref before (* 4 (+ 50 (* w 80)))))
                    (progn (fill-selection! sky '(0 0 0) 1 t)
                           (= 0 (aref (the-object sky pixels) (+ 3 (* 4 (+ 120 (* w 80)))))))
                    (progn (undo-stroke! doc) (undo-stroke! doc) (undo-stroke! doc)
                           (equalp (the-object sky pixels) before)))))
      (check "a selection rebuilds nothing" (equal (demand doc) '(1 1)) (counts doc)))))

(defun test-floating ()
  (let* ((image (let ((p (make-rgba 4 3)))
                  (dotimes (i 12 p)
                    (let ((o (* 4 i)))
                      (setf (aref p o) (* 20 i) (aref p (+ o 1)) 100 (aref p (+ o 2)) 50 (aref p (+ o 3)) 255)))))
         (doc (make-file-document "float" 4 3 image))
         (before (copy-seq (the-object doc (layer 0) pixels))))
    (check "a move without a selection shifts the whole layer and empties what it left"
           (progn (move-pixels! (the-object doc (layer 0)) 1 0)
                  (let ((p (the-object doc (layer 0) pixels)))
                    (and (= 0 (aref p 3))
                         (= (aref p (+ 3 (* 4 1))) 255)
                         (= (aref p (* 4 (+ 3 (* 4 1)))) (aref before (* 4 (+ 2 (* 4 1)))))
                         (= (aref p (* 4 (+ 1 (* 4 2)))) (aref before (* 4 (+ 0 (* 4 2))))))))
           (the-object doc (layer 0) pixels))
    (check "undo gives the layer back after a move"
           (and (undo-stroke! doc) (equalp (the-object doc (layer 0) pixels) before)))
    (check "a move under a selection moves the selected pixels only, and the selection with them"
           (progn (select-rect! doc 0 0 1 3)
                  (move-pixels! (the-object doc (layer 0)) 2 0)
                  (let ((p (the-object doc (layer 0) pixels)))
                    (and (= 0 (aref p 3))
                         (= (aref p (* 4 1)) (aref before (* 4 1)))
                         (= (aref p (* 4 (+ 2 (* 4 1)))) (aref before (* 4 (+ 0 (* 4 1)))))
                         (= (aref p (+ 3 (* 4 2))) 255)
                         (equal (getf (the-object doc selection) :rect) '(2 0 1 3)))))
           (list (the-object doc (layer 0) pixels) (getf (the-object doc selection) :rect)))
    (undo-stroke! doc)
    (check "Copy takes the selection's bytes; Paste puts them where they were, on a new layer above"
           (progn (select-rect! doc 1 1 2 2)
                  (copy-selection! (the-object doc (layer 0)))
                  (and (= 2 (getf *clipboard* :w)) (= 2 (getf *clipboard* :h))
                       (= 1 (paste-clipboard! doc 0))
                       (let ((p (the-object doc (layer 1) pixels)))
                         (and (equalp (rgba-rect p 4 3 1 1 2 2) (rgba-rect before 4 3 1 1 2 2))
                              (= 0 (aref p 3)))))))
    (check "Cut clears the selection on the layer, and undo gives it back"
           (progn (cut-selection! (the-object doc (layer 0)))
                  (and (= 0 (aref (the-object doc (layer 0) pixels) (+ 3 (* 4 (+ 1 (* 4 1))))))
                       (progn (undo-stroke! doc)
                              (equalp (the-object doc (layer 0) pixels) before)))))))

(defun test-transform ()
  (let* ((image (let ((p (make-rgba 4 3)))
                  (dotimes (i 12 p)
                    (let ((o (* 4 i)))
                      (setf (aref p o) (* 20 i) (aref p (+ o 1)) 100 (aref p (+ o 2)) 50 (aref p (+ o 3)) 255)))))
         (doc (make-file-document "xform" 4 3 image))
         (before (copy-seq (the-object doc (layer 0) pixels))))
    (check "the identity transform leaves the layer as it is"
           (progn (transform-pixels! (the-object doc (layer 0)) '(1 0 0 0 1 0))
                  (equalp (the-object doc (layer 0) pixels) before)))
    (check "an integer translation transforms as a move does"
           (progn (undo-stroke! doc)
                  (transform-pixels! (the-object doc (layer 0)) '(1 0 1 0 1 0))
                  (let ((transformed (copy-seq (the-object doc (layer 0) pixels))))
                    (undo-stroke! doc)
                    (move-pixels! (the-object doc (layer 0)) 1 0)
                    (equalp transformed (the-object doc (layer 0) pixels)))))
    (check "a singular transform is refused"
           (handler-case (progn (transform-pixels! (the-object doc (layer 0)) '(0 0 0 0 0 0)) nil)
             (error () t)))
    (check "a transform under a selection takes the selection with it"
           (progn (undo-stroke! doc)
                  (select-rect! doc 0 0 2 3)
                  (transform-pixels! (the-object doc (layer 0)) '(1 0 2 0 1 0))
                  (equal (getf (the-object doc selection) :rect) '(2 0 2 3)))
           (getf (the-object doc selection) :rect))
    ;; A 2x scale about the origin: every interior pixel samples four
    ;; source texels inside the picture, so it stays opaque, and holds
    ;; a mix of its sources; the edge row and column sample half a pixel
    ;; past the picture's edge and feather, as bilinear sampling does.
    (check "a scale about the origin doubles the picture's reach"
           (progn (undo-stroke! doc)
                  (select-none! doc)
                  (transform-pixels! (the-object doc (layer 0)) '(2 0 0 0 2 0))
                  (let ((p (the-object doc (layer 0) pixels)))
                    (and (loop for y from 1 below 3
                               always (loop for x from 1 below 4
                                            always (= 255 (aref p (+ 3 (* 4 (+ x (* 4 y))))))))
                         (<= 0 (aref p (* 4 (+ 2 (* 4 1)))) 100)
                         (< (aref p (+ 3 (* 4 0))) 255))))
           (the-object doc (layer 0) pixels))))

(defun test-file-format ()
  (let ((doc (make-sample-document "parity")))
    (the-object doc (layer 1) (set-slot! :opacity 0.3d0))
    (the-object doc (layer 2) (set-slot! :master '((0 0) (0.4 0.5) (1 1))))
    (apply-brush-stroke (the-object doc (layer 0)) *test-stroke*)
    (the-object doc (layer 6) (set-mask! '(:flat :value 128)))
    (apply-brush-stroke (the-object doc (layer 6)) *test-stroke* :target :mask)
    (let* ((render (the-object doc (render-cpu)))
           (octets (document-octets doc))
           (back (read-document-octets octets "back")))
      (check "a saved document loads back and renders the same: edits, strokes and masks included"
             (and (equalp (the-object back (render-cpu)) render)
                  (equal (layer-names back) (layer-names doc))
                  (= (the-object back (layer 1) opacity) 0.3d0)
                  (the-object back (layer 6) has-mask?)))
      (check "the file is smaller than the pixels it holds"
             (< (length octets) (* 4 320 200 (length (the-object doc raster-layers))))
             (length octets))
      (check "a file that is not a Stratum file is refused"
             (handler-case (progn (read-document-octets (string-octets "PNG, not us") "x") nil)
               (error () t)))
      (check "a header that tries to evaluate is refused"
             (handler-case (progn (read-document-octets
                                   (string-octets (format nil "STRATUM1~%20~%#.(list :stratum 1) "))
                                   "x")
                                  nil)
               (error () t))))))

(defun test-filter ()
  (let ((dot (let ((p (make-rgba 9 9)))
               (let ((o (* 4 (+ 4 (* 9 4)))))
                 (setf (aref p o) 255 (aref p (+ o 1)) 255 (aref p (+ o 2)) 255 (aref p (+ o 3)) 255))
               p)))
    (let ((b (gaussian-blur-rgba dot 9 9 1d0)))
      (check "a blurred dot spreads symmetrically with its center the brightest"
             (flet ((a (x y) (aref b (+ 3 (* 4 (+ x (* 9 y)))))))
               (and (> (a 4 4) (a 3 4)) (= (a 3 4) (a 5 4) (a 4 3) (a 4 5))
                    (> (a 3 4) 0) (= 0 (a 0 0))))
             (coerce b 'list))))
  (let ((flat (let ((p (make-rgba 7 5)))
                (dotimes (i 35 p)
                  (let ((o (* 4 i)))
                    (setf (aref p o) 90 (aref p (+ o 1)) 140 (aref p (+ o 2)) 200 (aref p (+ o 3)) 255))))))
    (check "a uniform layer blurs to itself, edges clamped"
           (equalp (gaussian-blur-rgba flat 7 5 3d0) flat)))
  (let* ((doc (make-sample-document "parity"))
         (discs (the-object doc (layer 4)))
         (w 320)
         (before (copy-seq (the-object discs pixels))))
    (demand doc)
    (check "a blur changes the layer and rebuilds nothing"
           (progn (filter-layer! discs :gaussian-blur :radius 2)
                  (and (not (equalp before (the-object discs pixels)))
                       (equal (demand doc) '(1 1)))))
    (check "undo gives the layer back after a blur"
           (and (undo-stroke! doc) (equalp before (the-object discs pixels))))
    (check "under a selection a blur changes the selected pixels only"
           (progn (select-rect! doc 100 60 60 40)
                  (filter-layer! discs :gaussian-blur :radius 2)
                  (let ((after (the-object discs pixels)))
                    (and (not (equalp before after))
                         (loop for i below (length before)
                               for px = (mod (floor i 4) w) for py = (floor (floor i 4) w)
                               always (or (and (<= 100 px 159) (<= 60 py 99))
                                          (= (aref before i) (aref after i))))))))))

(defun test-resize ()
  (let ((image (let ((p (make-rgba 4 3)))
                 (dotimes (i 12 p)
                   (let ((o (* 4 i)))
                     (setf (aref p o) (* 20 i) (aref p (+ o 1)) 100 (aref p (+ o 2)) 50 (aref p (+ o 3)) 255))))))
    (check "place-rgba pads with transparency and crops with a negative offset"
           (let ((padded (place-rgba image 4 3 6 5 1 1))
                 (cropped (place-rgba image 4 3 2 2 -1 -1)))
             (and (= 0 (aref padded 3))
                  (equalp (subseq padded (* 4 (+ 1 (* 6 1))) (* 4 (+ 5 (* 6 1)))) (subseq image 0 16))
                  (equalp cropped (rgba-rect image 4 3 1 1 2 2)))))
    (check "resampling to the same size changes nothing"
           (equalp (resample-rgba image 4 3 4 3) image))
    (check "resampling doubles and halves with the ends in place"
           (let ((up (resample-rgba image 4 3 8 6))
                 (down (resample-rgba image 4 3 2 1)))
             (and (= (length up) (* 4 8 6)) (= (length down) (* 4 2 1))
                  (= (aref up 0) (aref image 0))
                  (= (aref up (* 4 (+ 7 (* 8 5)))) (aref image (* 4 11)))
                  (every (lambda (i) (= 255 (aref up (+ 3 (* 4 i))))) (loop for i below 48 collect i)))))
    (check "a mask resamples on its own scale"
           (let ((m (resample-gray (coerce #(0 255) '(vector (unsigned-byte 8))) 2 1 4 1)))
             (and (= (aref m 0) 0) (= (aref m 3) 255) (< (aref m 0) (aref m 1) (aref m 2) (aref m 3))))))
  (let* ((doc (make-sample-document "parity"))
         (render (progn (the-object doc (layer 1) (set-slot! :opacity 0.3d0))
                        (apply-brush-stroke (the-object doc (layer 0)) *test-stroke*)
                        (the-object doc (render-cpu)))))
    (let ((cropped (crop-document doc 50 40 120 90)))
      (check "a cropped document renders as that rectangle of the original, edits and all"
             (and (= 120 (the-object cropped doc-width)) (= 90 (the-object cropped doc-height))
                  (equalp (the-object cropped (render-cpu)) (rgba-rect render 320 200 50 40 120 90))
                  (= (the-object cropped (layer 1) opacity) 0.3d0))))
    (let ((sized (resize-document doc 320 200)))
      (check "an image size of the same size renders the same" (equalp (the-object sized (render-cpu)) render)))
    (let ((bigger (resize-document doc 640 400)))
      (check "an image size of twice the size is twice the size"
             (and (= 640 (the-object bigger doc-width)) (= 400 (the-object bigger doc-height))
                  (= (length (the-object bigger (render-cpu))) (* 4 640 400)))))
    (let ((canvas (canvas-size-document doc 340 220 0.5d0 0.5d0)))
      (check "a larger canvas keeps the picture centered on a white border"
             (let ((r (the-object canvas (render-cpu))))
               (and (= 340 (the-object canvas doc-width))
                    (equalp (rgba-rect r 340 220 10 10 320 200) render)
                    (= 255 (aref r 0) (aref r 1) (aref r 2))))))
    (let ((canvas (canvas-size-document doc 200 100 0d0 0d0)))
      (check "a smaller canvas anchored top-left is the top-left corner"
             (equalp (the-object canvas (render-cpu)) (rgba-rect render 320 200 0 0 200 100))))))

(defun test-adjust ()
  (let* ((doc (make-sample-document "parity"))
         (sky (the-object doc (layer 0)))
         (before (copy-seq (the-object sky pixels))))
    (demand doc)
    (check "identity curves bake nothing, and rebuild nothing"
           (and (equal (adjust-layer! sky :curves) (list 0 0 320 200))
                (equalp (the-object sky pixels) before)
                (equal (demand doc) '(1 1))))
    (check "levels with a gamma change the pixels and leave alpha alone"
           (progn (adjust-layer! sky :levels :settings '(:gamma 1.6d0))
                  (let ((after (the-object sky pixels)))
                    (and (not (equalp after before))
                         (loop for i from 3 below (length before) by 4
                               always (= (aref after i) (aref before i)))))))
    (check "undo gives the layer back exactly, through both adjustments"
           (and (undo-stroke! doc) (undo-stroke! doc)
                (equalp (the-object sky pixels) before)))
    (check "saturation -1 bakes gray"
           (progn (adjust-layer! sky :hue-saturation :settings '(:saturation -1d0))
                  (let ((p (the-object sky pixels)))
                    (loop for o from 0 below (length p) by 4
                          always (= (aref p o) (aref p (+ o 1)) (aref p (+ o 2)))))))
    (check "an adjustment layer cannot be baked into"
           (handler-case (progn (adjust-layer! (the-object doc (layer 2)) :levels) nil)
             (error () t)))
    (check "the undo list keeps within its byte budget"
           (let ((*undo-bytes* (* 3 (length before))))
             (dotimes (i 6) (adjust-layer! sky :levels :settings '(:gamma 1.1d0)))
             (<= (length (the-object doc undo-entries)) 3))
           (length (the-object doc undo-entries)))))

(defun layer-names (doc)
  (mapcar (lambda (l) (the-object l layer-name)) (the-object doc layer-list)))

(defun test-layers ()
  (let* ((doc (make-sample-document "parity"))
         (sky (the-object doc (layer 0))))
    (the-object doc (layer 1) (set-slot! :opacity 0.3d0))
    (apply-brush-stroke sky *test-stroke*)
    (let ((painted (copy-seq (the-object sky pixels)))
          (names (layer-names doc)))
      (check "a new adjustment layer goes in above the given layer, named in sequence"
             (and (= 1 (the-object doc (add-layer! (the-object doc (new-layer-spec :levels)) 0)))
                  (eql (the-object doc (layer 1) kind) :levels)
                  (string= (the-object doc (layer 1) layer-name) "Levels 2")
                  (= (length (the-object doc layer-list)) (1+ (length names))))
             (layer-names doc))
      (check "painted pixels and edits survive a change of the stack"
             (and (equalp (the-object doc (layer 0) pixels) painted)
                  (= (the-object doc (layer 2) opacity) 0.3d0)))
      (check "a change of the stack forgets the undo list"
             (null (the-object doc undo-entries)))
      (check "deleting the layer gives the stack back"
             (and (= 1 (the-object doc (delete-layer! 1)))
                  (equal (layer-names doc) names))
             (layer-names doc))
      (check "moving a layer up swaps it with the one above, pixels and all"
             (and (= 1 (the-object doc (move-layer! 0 1)))
                  (string= (the-object doc (layer 1) layer-name) "Sky")
                  (equalp (the-object doc (layer 1) pixels) painted)
                  (= 0 (the-object doc (move-layer! 1 -1)))
                  (equal (layer-names doc) names))
             (layer-names doc))
      (check "a duplicate sits above the original with its own copy of the pixels"
             (and (= 1 (the-object doc (duplicate-layer! 0)))
                  (string= (the-object doc (layer 1) layer-name) "Sky copy")
                  (equalp (the-object doc (layer 1) pixels) painted)
                  (not (eq (the-object doc (layer 1) pixels) (the-object doc (layer 0) pixels))))
             (layer-names doc))
      (check "a new layer on top is transparent"
             (let ((j (the-object doc (add-layer! (the-object doc (new-layer-spec :raster))))))
               (and (= j (1- (length (the-object doc layer-list))))
                    (string= (the-object doc (layer j) layer-name)
                             (format nil "Layer ~d" (1+ j)))
                    (every #'zerop (the-object doc (layer j) pixels))))
             (layer-names doc))
      (check "the programs follow the stack"
             (let ((shader (the-object doc shader-source)))
               (and (= (length (the-object doc layer-list))
                       (loop for start = 0 then (1+ pos)
                             for pos = (search "// layer " shader :start2 start)
                             while pos count t))
                    (= (length (the-object doc (render-cpu)))
                       (* 4 (the-object doc doc-width) (the-object doc doc-height)))))))))

(defun test-threads ()
  (let* ((doc (make-sample-document "parity"))
         (threaded (let ((*render-threads* 8)) (the-object doc (render-cpu))))
         (single (let ((*render-threads* 1)) (the-object doc (render-cpu)))))
    (check "a threaded render equals a single-threaded one" (equalp threaded single))))

(defun run-tests ()
  "Run every check; print one line each; T when all passed."
  (setf *checks* nil)
  (dolist (test '(test-rebuilds test-layer-types test-curves test-adjustments test-rect
                  test-brush test-masks test-adjust test-selection test-floating test-transform test-resize
                  test-file-format test-filter
                  test-layers test-files
                  test-threads test-golden))
    (handler-case (funcall test)
      (error (e) (check (format nil "~(~a~) ran without error" test) nil (princ-to-string e)))))
  (let ((checks (reverse *checks*)))
    (dolist (c checks)
      (format t "~&~:[FAIL~;pass~]  ~a~@[  ~s~]~%"
              (eql (first c) :pass) (second c) (and (eql (first c) :fail) (third c))))
    (let ((failed (count :fail checks :key #'first)))
      (format t "~&~d checks, ~d failed (~a ~a)~%" (length checks) failed
              (lisp-implementation-type) (lisp-implementation-version))
      (zerop failed))))
