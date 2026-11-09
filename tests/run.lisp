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

(defun test-clone ()
  ;; An 8 x 4 picture, its left half opaque red, its right half clear.
  (let ((pic (let ((p (make-rgba 8 4)))
               (dotimes (y 4 p)
                 (dotimes (x 4)
                   (let ((o (* 4 (+ x (* 8 y)))))
                     (setf (aref p o) 255 (aref p (+ o 3)) 255)))))))
    (check "clone-source-rect copies the rectangle and is clear beyond the picture"
           (let ((s (clone-source-rect pic 8 4 2 1 4 4)))
             (and (= 255 (aref s 0) (aref s 3))                   ; (2,1): red
                  (= 255 (aref s (* 4 1)))                         ; (3,1): red
                  (= 0 (aref s (+ 3 (* 4 2))))                     ; (4,1): clear
                  (= 255 (aref s (+ 3 (* 4 (+ 1 (* 4 2))))))       ; row 2 = y 3, col 1 = x 3: red
                  (= 0 (aref s (+ 3 (* 4 (* 4 3)))))))             ; row 3 = y 4: beyond
           (coerce (clone-source-rect pic 8 4 2 1 4 4) 'list))
    (check "a source rectangle starting before the picture is clear there"
           (let ((s (clone-source-rect pic 8 4 -2 -1 4 4)))
             (and (= 0 (aref s 3)) (= 0 (aref s (+ 3 (* 4 (+ 2 (* 4 0))))))
                  (= 255 (aref s (+ 3 (* 4 (+ 2 (* 4 1))))))))))
  (let* ((doc (make-sample-document "parity"))
         (sky (the-object doc (layer 0)))
         (w 320)
         (before (copy-seq (the-object sky pixels)))
         (brush '(:radius 6 :hardness 1 :spacing 0.25 :flow 1 :opacity 1)))
    (demand doc)
    (flet ((px (pixels x y) (let ((o (* 4 (+ x (* w y))))) (list (aref pixels o) (aref pixels (+ o 1)) (aref pixels (+ o 2)) (aref pixels (+ o 3)))))
           (near (a b) (every (lambda (p q) (<= (abs (- p q)) 1)) a b)))
      (check "a clone stroke paints the layer's own pixels from the offset, inside its rectangle only, and rebuilds nothing"
             (let ((rect (apply-brush-stroke sky (list* :points '((150 100 1)) :clone '(-100 -40) brush)))
                   (after (the-object sky pixels)))
               (and rect
                    (near (px after 150 100) (px before 50 60))
                    (not (equalp before after))
                    (loop for i below (length before)
                          for x = (mod (floor i 4) w) for y = (floor (floor i 4) w)
                          always (or (and (<= 143 x 157) (<= 93 y 107)) (= (aref before i) (aref after i))))
                    (equal (demand doc) '(1 1))))
             (list (px (the-object sky pixels) 150 100) (px before 50 60)))
      (check "a stroke over its own source reads the pixels as they were"
             (let ((now (copy-seq (the-object sky pixels))))
               (apply-brush-stroke sky (list* :points '((200 50 1)) :clone '(1 0) brush))
               (let ((after (the-object sky pixels)))
                 (and (near (px after 200 50) (px now 201 50))
                      (near (px after 197 50) (px now 198 50))))))
      (check "a clone from beyond the layer paints nothing"
             (let ((now (copy-seq (the-object sky pixels))))
               (and (apply-brush-stroke sky (list* :points '((5 5 1)) :clone '(-100 -100) brush))
                    (equalp now (the-object sky pixels)))))
      (check "undo gives the layer back after the clone strokes"
             (progn (undo-stroke! doc) (undo-stroke! doc) (undo-stroke! doc)
                    (equalp before (the-object sky pixels)))))))

(defun test-heal ()
  (let* ((doc (make-sample-document "parity"))
         (discs (the-object doc (layer 4)))
         (pixels (the-object discs pixels)))
    (check "blur-rect-straight gives the whole layer's blur at the rectangle, bit for bit"
           (let* ((whole (gaussian-blur-premul (decode-premul pixels 320 200) 320 200 2.5d0))
                  (part (blur-rect-straight pixels 320 200 100 60 30 20 2.5d0)))
             (loop for row below 20
                   always (loop for col below 30
                                always (let* ((wo (* 4 (+ 100 col (* 320 (+ 60 row)))))
                                              (po (* 4 (+ col (* 30 row))))
                                              (a (aref whole (+ wo 3))))
                                         (and (= a (aref part (+ po 3)))
                                              (or (= a 0f0)
                                                  (and (= (/ (aref whole wo) a) (aref part po))
                                                       (= (/ (aref whole (+ wo 2)) a) (aref part (+ po 2)))))))))))
    (check "a rectangle beyond the layer blurs to zero there"
           (let ((part (blur-rect-straight pixels 320 200 310 190 20 20 2d0)))
             (and (= 0f0 (aref part (+ 3 (* 4 (+ 15 (* 20 15))))))
                  (> (aref part (+ 3 (* 4 (+ 5 (* 20 5))))) 0f0)))))
  (flet ((flat (w h v) (let ((p (make-rgba w h)))
                         (dotimes (i (* w h) p)
                           (let ((o (* 4 i))) (setf (aref p o) v (aref p (+ o 1)) v (aref p (+ o 2)) v (aref p (+ o 3)) 255)))))
         (gray (p w x y) (aref p (* 4 (+ x (* w y)))))
         (cov (w h) (make-array (* w h) :element-type 'double-float :initial-element 1d0)))
    ;; Two tones side by side: healing from the dark half onto the light
    ;; half keeps the light tone, where a clone pastes the dark one.
    (let ((two (flat 40 20 180)))
      (dotimes (y 20) (dotimes (x 20) (let ((o (* 4 (+ x (* 40 y))))) (setf (aref two o) 60 (aref two (+ o 1)) 60 (aref two (+ o 2)) 60))))
      (let ((healed (copy-seq two)) (cloned (copy-seq two)))
        (heal-stroke! healed 40 20 (cov 5 5) 28 8 5 5 -20 0 4 1d0)
        (composite-clone-stroke! cloned 40 (cov 5 5) 28 8 5 5 (clone-source-rect two 40 20 8 8 5 5) 1d0)
        (check "healing from a dark flat onto a light flat keeps the light tone; a clone pastes the dark"
               (and (<= 179 (gray healed 40 30 10) 181) (= 60 (gray cloned 40 30 10)))
               (list (gray healed 40 30 10) (gray cloned 40 30 10)))))
    ;; Texture carries: a single bright pixel in a flat field, healed
    ;; onto flat field elsewhere, brightens its destination.
    (let ((field (flat 40 20 120)))
      (let ((o (* 4 (+ 10 (* 40 10))))) (setf (aref field o) 255 (aref field (+ o 1)) 255 (aref field (+ o 2)) 255))
      (let ((healed (copy-seq field)))
        (heal-stroke! healed 40 20 (cov 5 5) 28 8 5 5 -20 0 4 1d0)
        (check "healing carries the source's texture onto the destination"
               (and (> (gray healed 40 30 10) 150) (= 120 (gray healed 40 20 10)))
               (gray healed 40 30 10)))))
  (let* ((doc (make-sample-document "parity"))
         (sky (the-object doc (layer 0)))
         (w 320)
         (before (copy-seq (the-object sky pixels))))
    (demand doc)
    (flet ((px (pixels x y) (let ((o (* 4 (+ x (* w y))))) (list (aref pixels o) (aref pixels (+ o 1)) (aref pixels (+ o 2)) (aref pixels (+ o 3)))))
           (within (a b n) (every (lambda (p q) (<= (abs (- p q)) n)) a b)))
      (check "a healing stroke on the Sky ramp keeps the destination's tone inside its rectangle only, and rebuilds nothing"
             (let ((rect (apply-brush-stroke sky (list :points '((150 100 1)) :clone '(-100 -40) :heal t
                                                        :radius 6 :hardness 1 :spacing 0.25 :flow 1 :opacity 1)))
                   (after (the-object sky pixels)))
               (and rect
                    (within (px after 150 100) (px before 150 100) 3)
                    (not (within (px before 50 60) (px before 150 100) 8))
                    (loop for i below (length before)
                          for x = (mod (floor i 4) w) for y = (floor (floor i 4) w)
                          always (or (and (<= 143 x 157) (<= 93 y 107)) (= (aref before i) (aref after i))))
                    (equal (demand doc) '(1 1))))
             (list (px (the-object sky pixels) 150 100) (px before 150 100) (px before 50 60)))
      (check "undo gives the layer back after a healing stroke"
             (and (undo-stroke! doc) (equalp before (the-object sky pixels)))))))

(defun test-history ()
  (let* ((doc (make-sample-document "parity"))
         (sky (the-object doc (layer 0)))
         (w 320)
         (brush '(:radius 6 :hardness 1 :spacing 0.25 :flow 1 :opacity 1)))
    (ensure-history-specs! doc)
    (demand doc)
    (flet ((px (pixels x y) (let ((o (* 4 (+ x (* w y))))) (list (aref pixels o) (aref pixels (+ o 1)) (aref pixels (+ o 2)) (aref pixels (+ o 3)))))
           (near (a b) (every (lambda (p q) (<= (abs (- p q)) 1)) a b)))
      (let ((opened (copy-seq (composite-specs doc (the-object doc layer-specs))))
            (before (copy-seq (the-object sky pixels))))
        (check "the history composite is the document as opened, over transparency, rendered once and kept"
               (let ((h (history-pixels doc)))
                 (and (equalp h opened) (eq h (history-pixels doc)) (= 0 (the-object doc history-version)))))
        (check "a history stroke paints the opened composite onto the layer, inside its rectangle, and rebuilds nothing"
               (progn (apply-brush-stroke sky (list* :points '((150 100 1)) :color '(255 0 0) brush))
                      (let ((rect (apply-brush-stroke sky (list* :points '((150 100 1)) :history t brush)))
                            (after (the-object sky pixels)))
                        (and rect
                             (near (px after 150 100) (px opened 150 100))
                             (not (near (px before 150 100) (px opened 150 100)))
                             (equal (px after 50 50) (px before 50 50))
                             (equal (demand doc) '(1 1)))))
               (list (px (the-object sky pixels) 150 100) (px opened 150 100) (px before 150 100)))
        (check "undo twice gives the layer back; the source still stands after strokes"
               (progn (undo-stroke! doc) (undo-stroke! doc)
                      (and (equalp before (the-object sky pixels))
                           (equalp opened (history-pixels doc)))))
        (check "Set Source takes the current state, copied, and bumps the version"
               (progn (select-rect! doc 100 60 60 40)
                      (fill-selection! sky '(255 0 0) 1)
                      (select-none! doc)
                      (let ((version (set-history! doc))
                            (red (copy-seq (composite-specs doc (materialized-specs doc)))))
                        (undo-stroke! doc)
                        (and (= version 1)
                             (equalp (history-pixels doc) red)
                             (equalp before (the-object sky pixels))
                             (progn (apply-brush-stroke sky (list* :points '((130 80 1)) :history t brush))
                                    (near (px (the-object sky pixels) 130 80) (px red 130 80)))
                             (progn (undo-stroke! doc) (equalp before (the-object sky pixels)))))))
        (check "in Grayscale the history brush paints gray"
               (progn (set-color-mode! doc :grayscale)
                      (apply-brush-stroke sky (list* :points '((150 100 1)) :history t brush))
                      (let ((p (px (the-object sky pixels) 150 100)))
                        (prog1 (and (= (first p) (second p) (third p))
                                    (not (= (first (px opened 150 100)) (second (px opened 150 100)))))
                          (undo-stroke! doc) (undo-stroke! doc)))))
        (check "a cropped document starts its history at the cropped state"
               (let ((cropped (crop-document doc 10 10 50 50)))
                 (and (null (the-object cropped history-specs))
                      (progn (ensure-history-specs! cropped)
                             (equalp (history-pixels cropped)
                                     (composite-specs cropped (the-object cropped layer-specs)))))))))))

(defun test-mode ()
  (check "a color turns to its luminance gray, alpha kept"
         (let ((p (make-rgba 2 1)))
           (setf (aref p 0) 255 (aref p 1) 0 (aref p 2) 0 (aref p 3) 100
                 (aref p 4) 0 (aref p 5) 255 (aref p 6) 0 (aref p 7) 255)
           (gray-rgba! p)
           (and (= (aref p 0) (aref p 1) (aref p 2) 127) (= 100 (aref p 3))
                (= (aref p 4) (aref p 5) (aref p 6) 220) (= 255 (aref p 7))
                (= 127 (color-gray '(255 0 0))) (= 220 (color-gray '(0 255 0)))))
         (list (color-gray '(255 0 0)) (color-gray '(0 255 0))))
  (let* ((doc (make-sample-document "parity"))
         (rasters (remove-if-not (lambda (l) (eql (the-object l kind) :raster)) (the-object doc layer-list)))
         (before (mapcar (lambda (l) (copy-seq (the-object l pixels))) rasters))
         (w 320))
    (demand doc)
    (flet ((all-gray (pixels) (loop for o from 0 below (length pixels) by 4
                                    always (= (aref pixels o) (aref pixels (+ o 1)) (aref pixels (+ o 2)))))
           (px (pixels x y) (let ((o (* 4 (+ x (* w y))))) (list (aref pixels o) (aref pixels (+ o 1)) (aref pixels (+ o 2))))))
      (check "Grayscale turns every raster layer gray in one undo entry, and rebuilds nothing"
             (let ((entry (set-color-mode! doc :grayscale)))
               (and (eql (the-object doc color-mode) :grayscale)
                    (equal (subseq entry 0 3) '(:mode :rgb :grayscale))
                    (= (length (cdddr entry)) (length rasters))
                    (every (lambda (l) (all-gray (the-object l pixels))) rasters)
                    (notany #'all-gray before)
                    (= 1 (length (the-object doc undo-entries)))
                    (equal (demand doc) '(1 1)))))
      (check "in Grayscale a red stroke, a red fill and a red-to-blue gradient paint gray"
             (let* ((sky (first rasters))
                    (gray-sky (copy-seq (the-object sky pixels))))
               (apply-brush-stroke sky (list :points '((150 100 1)) :color '(255 0 0) :radius 6 :hardness 1 :flow 1 :opacity 1))
               (select-rect! doc 10 10 20 20)
               (fill-selection! sky '(255 0 0) 1)
               (select-none! doc)
               (gradient-layer! sky 0 0 320 0 :start '(255 0 0 255) :end '(0 0 255 255))
               (let ((painted (and (all-gray (the-object sky pixels))
                                   (not (equal (px (the-object sky pixels) 150 100) (px gray-sky 150 100)))))
                     (entries (length (cdddr (fourth (the-object doc undo-entries))))))
                 (undo-stroke! doc) (undo-stroke! doc) (undo-stroke! doc)
                 (and painted (= entries (length rasters)) (equalp gray-sky (the-object sky pixels))))))
      (check "undo of the mode change brings every layer and the mode back; redo turns it gray again"
             (let ((entry (undo-stroke! doc)))
               (and (equal (subseq entry 0 3) '(:mode :rgb :grayscale))
                    (eql (the-object doc color-mode) :rgb)
                    (every (lambda (l b) (equalp (the-object l pixels) b)) rasters before)
                    (progn (redo-stroke! doc)
                           (and (eql (the-object doc color-mode) :grayscale)
                                (every (lambda (l) (all-gray (the-object l pixels))) rasters))))))
      (check "RGB Color changes the mode alone, and undoes"
             (let ((gray-now (mapcar (lambda (l) (copy-seq (the-object l pixels))) rasters)))
               (and (null (set-color-mode! doc :grayscale))
                    (equal (set-color-mode! doc :rgb) '(:mode :grayscale :rgb))
                    (eql (the-object doc color-mode) :rgb)
                    (every (lambda (l b) (equalp (the-object l pixels) b)) rasters gray-now)
                    (progn (undo-stroke! doc) (eql (the-object doc color-mode) :grayscale)))))
      (check "a paste into a Grayscale document is gray; the file and a crop keep the mode"
             (let ((color-doc (make-sample-document "parity")))
               (select-rect! color-doc 100 60 40 30)
               (copy-selection! (the-object color-doc (layer 0)))
               (let ((index (paste-clipboard! doc)))
                 (and (all-gray (the-object doc (layer index) pixels))
                      (not (all-gray (getf (clipboard) :pixels)))
                      (eql (the-object (read-document-octets (document-octets doc) "back") color-mode) :grayscale)
                      (eql (the-object (crop-document doc 10 10 50 50) color-mode) :grayscale)
                      (eql (the-object (read-document-octets (document-octets color-doc) "rgb") color-mode) :rgb))))))))

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

;;; A PNG built by hand for the reader's tests: PIXEL is a function of
;;; (x y) giving the channel samples at DEPTH; FILTERS a list of filter
;;; types used in turn, row by row; INTERLACE writes Adam7 passes.
(defun build-png (width height depth color-type pixel &key palette trns (filters '(0)) interlace)
  (let* ((channels (ecase color-type (0 1) (2 3) (3 1) (4 2) (6 4)))
         (bpp (max 1 (floor (* channels depth) 8)))
         (raw (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (row-count 0))
    (labels ((emit (v) (vector-push-extend v raw))
             (pack-row (xs y xstep pw)
               (let* ((stride (ceiling (* pw channels depth) 8))
                      (row (make-array stride :element-type '(unsigned-byte 8) :initial-element 0)))
                 (dotimes (px pw row)
                   (let ((samples (funcall pixel (+ xs (* px xstep)) y)))
                     (loop for s in samples for k from 0
                           for index = (+ (* px channels) k)
                           do (case depth
                                (8 (setf (aref row index) s))
                                (16 (setf (aref row (* 2 index)) (ash s -8) (aref row (1+ (* 2 index))) (logand s 255)))
                                (t (let ((bit (* index depth)))
                                     (setf (ldb (byte depth (- 8 depth (mod bit 8))) (aref row (floor bit 8))) s)))))))))
             (filter-row (row prior filter)
               (let ((out (copy-seq row)))
                 (dotimes (i (length row) out)
                   (let ((a (if (>= i bpp) (aref row (- i bpp)) 0))
                         (b (aref prior i))
                         (c (if (>= i bpp) (aref prior (- i bpp)) 0)))
                     (setf (aref out i)
                           (logand #xff (- (aref row i)
                                           (ecase filter
                                             (0 0) (1 a) (2 b) (3 (floor (+ a b) 2))
                                             (4 (let* ((p (- (+ a b) c)) (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
                                                  (cond ((and (<= pa pb) (<= pa pc)) a) ((<= pb pc) b) (t c))))))))))))
             (pass (xs ys xstep ystep)
               (let ((pw (if (< xs width) (ceiling (- width xs) xstep) 0))
                     (ph (if (< ys height) (ceiling (- height ys) ystep) 0)))
                 (when (and (plusp pw) (plusp ph))
                   (let ((prior (make-array (ceiling (* pw channels depth) 8) :element-type '(unsigned-byte 8) :initial-element 0)))
                     (dotimes (py ph)
                       (let* ((y (+ ys (* py ystep)))
                              (row (pack-row xs y xstep pw))
                              (filter (nth (mod row-count (length filters)) filters)))
                         (incf row-count)
                         (emit filter)
                         (map nil #'emit (filter-row row prior filter))
                         (setf prior row))))))))
      (if interlace
          (dolist (p *adam7-passes*) (apply #'pass p))
          (pass 0 0 1 1)))
    (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (flet ((chunk (type data)
               (let* ((data (coerce data '(simple-array (unsigned-byte 8) (*))))
                      (body (concatenate '(simple-array (unsigned-byte 8) (*)) (map 'vector #'char-code type) data))
                      (crc (crc32 body 0 (length body))))
                 (loop for s in '(24 16 8 0) do (vector-push-extend (ldb (byte 8 s) (length data)) out))
                 (map nil (lambda (b) (vector-push-extend b out)) body)
                 (loop for s in '(24 16 8 0) do (vector-push-extend (ldb (byte 8 s) crc) out)))))
        (map nil (lambda (b) (vector-push-extend b out)) *png-signature*)
        (chunk "IHDR" (list (ldb (byte 8 24) width) (ldb (byte 8 16) width) (ldb (byte 8 8) width) (ldb (byte 8 0) width)
                            (ldb (byte 8 24) height) (ldb (byte 8 16) height) (ldb (byte 8 8) height) (ldb (byte 8 0) height)
                            depth color-type 0 0 (if interlace 1 0)))
        (when palette (chunk "PLTE" palette))
        (when trns (chunk "tRNS" trns))
        (chunk "IDAT" (deflate-octets (coerce raw '(simple-array (unsigned-byte 8) (*)))))
        (chunk "IEND" #()))
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

(defun test-png ()
  (flet ((px (rgba w x y) (let ((o (* 4 (+ x (* w y))))) (list (aref rgba o) (aref rgba (+ o 1)) (aref rgba (+ o 2)) (aref rgba (+ o 3))))))
    (let* ((doc (make-sample-document "parity"))
           (rgba (the-object doc (render-cpu))))
      (check "a PNG written by zpng reads back byte for byte"
             (multiple-value-bind (back w h) (read-png (png-octets rgba 320 200))
               (and (= w 320) (= h 200) (equalp back rgba)))))
    (let ((want (lambda (x y) (list (mod (* 37 (+ x (* 7 y))) 256) (mod (* 91 (+ x y)) 256) (mod (* 13 x y) 256) (if (= 0 (mod (+ x y) 3)) 40 255)))))
      (check "8-bit RGBA through every filter type reads back"
             (multiple-value-bind (back w h) (read-png (build-png 7 5 8 6 want :filters '(0 1 2 3 4)))
               (and (= w 7) (= h 5)
                    (loop for y below 5 always (loop for x below 7 always (equal (px back 7 x y) (funcall want x y)))))))
      (check "an Adam7 interlaced RGBA reads back through all seven passes"
             (multiple-value-bind (back w h) (read-png (build-png 9 9 8 6 want :filters '(4 3 2 1 0) :interlace t))
               (and (= w 9) (= h 9)
                    (loop for y below 9 always (loop for x below 9 always (equal (px back 9 x y) (funcall want x y)))))))
      (check "an interlaced image narrower than a pass's start skips that pass"
             (multiple-value-bind (back w h) (read-png (build-png 3 2 8 6 want :interlace t))
               (and (= w 3) (= h 2)
                    (loop for y below 2 always (loop for x below 3 always (equal (px back 3 x y) (funcall want x y))))))))
    (check "16-bit RGB rounds each sample to a byte, opaque"
           (let ((back (read-png (build-png 3 1 16 2 (lambda (x y) (declare (ignore y)) (nth x '((65535 0 32768) (257 254 128) (1 65534 65535))))))))
             (and (equal (px back 3 0 0) '(255 0 128 255))
                  (equal (px back 3 1 0) '(1 1 0 255))
                  (equal (px back 3 2 0) '(0 255 255 255)))))
    (check "1-bit gray reads bits most significant first, over a width that is not a byte"
           (let* ((bits '(1 0 1 1 0 0 1 0 1 1))
                  (back (read-png (build-png 10 2 1 0 (lambda (x y) (list (if (= y 0) (nth x bits) (- 1 (nth x bits)))))))))
             (and (loop for x below 10 always (equal (px back 10 x 0) (let ((v (* 255 (nth x bits)))) (list v v v 255))))
                  (equal (px back 10 0 1) '(0 0 0 255)))))
    (check "4-bit gray scales to 0..255"
           (let ((back (read-png (build-png 3 1 4 0 (lambda (x y) (declare (ignore y)) (list (nth x '(0 7 15))))))))
             (and (equal (px back 3 0 0) '(0 0 0 255)) (equal (px back 3 1 0) '(119 119 119 255)) (equal (px back 3 2 0) '(255 255 255 255)))))
    (check "a 2-bit palette with partial transparency reads colors and alphas"
           (let ((back (read-png (build-png 5 1 2 3 (lambda (x y) (declare (ignore y)) (list (nth x '(0 1 2 3 1))))
                                            :palette '(255 0 0 0 255 0 0 0 255 9 8 7) :trns '(0 128)))))
             (and (equal (px back 5 0 0) '(255 0 0 0)) (equal (px back 5 1 0) '(0 255 0 128))
                  (equal (px back 5 2 0) '(0 0 255 255)) (equal (px back 5 3 0) '(9 8 7 255)) (equal (px back 5 4 0) '(0 255 0 128)))))
    (check "8-bit gray with alpha, and a gray key color"
           (let ((ga (read-png (build-png 2 2 8 4 (lambda (x y) (list (+ 10 x (* 2 y)) (* 60 (+ 1 x y)))))))
                 (keyed (read-png (build-png 3 1 8 0 (lambda (x y) (declare (ignore y)) (list (nth x '(5 77 200)))) :trns '(0 77)))))
             (and (equal (px ga 2 1 1) '(13 13 13 180)) (equal (px ga 2 0 0) '(10 10 10 60))
                  (equal (px keyed 3 0 0) '(5 5 5 255)) (equal (px keyed 3 1 0) '(77 77 77 0)) (equal (px keyed 3 2 0) '(200 200 200 255)))))
    (check "a chunk with a bad CRC, and a file that is not a PNG, are refused"
           (let ((png (build-png 2 2 8 6 (lambda (x y) (list x y 0 255)))))
             (setf (aref png 20) (logxor (aref png 20) 1))
             (and (handler-case (progn (read-png png) nil) (error () t))
                  (handler-case (progn (read-png (string-octets "hello")) nil) (error () t))
                  (not (png-octets-p (string-octets "hello"))))))
    (check "a PNG file opens as a document under its own name"
           (let* ((path (merge-pathnames "stratum-test-open.png" (uiop:temporary-directory)))
                  (png (build-png 6 4 8 2 (lambda (x y) (list (* 40 x) (* 60 y) 7)))))
             (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
               (write-sequence png out))
             (unwind-protect
                  (let ((doc (open-png-file path)))
                    (and (gethash (the-object doc doc-name) *documents*)
                         (= 6 (the-object doc doc-width)) (= 4 (the-object doc doc-height))
                         (equal (px (the-object doc (layer 0) pixels) 6 5 3) '(200 180 7 255))
                         (progn (remhash (the-object doc doc-name) *documents*) t)))
               (delete-file path))))))

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
             (list (the-object doc (render-cpu)) opaque)))
    (check "a new document is white, or transparent, or a color, one Background"
           (let ((white (new-document "n1" 5 4))
                 (clear (new-document "n2" 5 4 :transparent))
                 (red (new-document "n3" 5 4 '(200 10 10))))
             (and (every (lambda (v) (= v 255)) (the-object white (layer 0) pixels))
                  (equal (layer-names white) '("Background"))
                  (loop for i from 3 below 80 by 4 always (zerop (aref (the-object clear (layer 0) pixels) i)))
                  (= 200 (aref (the-object red (layer 0) pixels) 0))
                  (= 10 (aref (the-object red (layer 0) pixels) 1))
                  (equalp (the-object white (render-cpu)) (the-object white (layer 0) pixels)))))))

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

(defun test-wand ()
  ;; A 6 x 4 picture: a red left half, a blue right half, and one red
  ;; pixel inside the blue half.
  (let ((pic (let ((p (make-rgba 6 4)))
               (dotimes (y 4 p)
                 (dotimes (x 6)
                   (let ((o (* 4 (+ x (* 6 y))))
                         (red (or (< x 3) (and (= x 4) (= y 2)))))
                     (setf (aref p o) (if red 220 20) (aref p (+ o 1)) 30
                           (aref p (+ o 2)) (if red 20 220) (aref p (+ o 3)) 255)))))))
    (check "a contiguous wand takes the connected patch only"
           (let ((m (wand-mask pic 6 4 0 0 32 t)))
             (and (= 12 (count 255 m)) (= 0 (aref m (+ 4 (* 6 2)))))))
    (check "a non-contiguous wand takes every matching pixel"
           (= 13 (count 255 (wand-mask pic 6 4 0 0 32 nil))))
    (check "the tolerance decides what matches"
           (and (= 13 (count 255 (wand-mask pic 6 4 4 2 0 nil)))
                (= 11 (count 255 (wand-mask pic 6 4 5 0 0 nil)))
                (= 24 (count 255 (wand-mask pic 6 4 0 0 255 nil)))))
    (let ((doc (make-file-document "wand" 6 4 pic)))
      (check "the wand through the document selects on the layer, and Shift adds"
             (progn (select-wand! doc 0 0 :tolerance 32 :layer (the-object doc (layer 0)))
                    (and (equal (getf (the-object doc selection) :rect) '(0 0 3 4))
                         (progn (select-wand! doc 5 0 :tolerance 32 :mode :add :layer (the-object doc (layer 0)))
                                (= 23 (count 255 (getf (the-object doc selection) :mask)))))))
      (check "Alt takes from the selection"
             (progn (select-rect! doc 0 0 3 4 :subtract)
                    (= 11 (count 255 (getf (the-object doc selection) :mask)))))
      (check "Feather softens the edge and widens the bounds"
             (progn (select-rect! doc 2 1 2 2)
                    (feather-selection! doc 1)
                    (let* ((sel (the-object doc selection)) (m (getf sel :mask)))
                      (and (equal (getf sel :rect) '(0 0 6 4))
                           (< 0 (aref m (+ 1 (* 6 1))) 255)
                           (> (aref m (+ 2 (* 6 1))) (aref m (+ 1 (* 6 1)))))))))))

(defun nearest-site-squared (bytes w h x y site?)
  "The squared distance from X, Y to the nearest byte of BYTES (W x H)
that satisfies SITE?, by asking every pixel."
  (let ((best nil))
    (dotimes (sy h best)
      (dotimes (sx w)
        (when (funcall site? (aref bytes (+ sx (* sy w))))
          (let ((d (+ (* (- x sx) (- x sx)) (* (- y sy) (- y sy)))))
            (when (or (null best) (< d best)) (setf best d))))))))

(defun test-modify-selection ()
  "Contract and Expand: the distances exact, the edges where they should
be, and the picture's edge no edge of the selection."
  (let* ((w 40) (h 30)
         (doc (make-file-document "modify" w h (make-rgba w h)))
         (rect (lambda () (getf (the-object doc selection) :rect)))
         (at (lambda (x y) (aref (getf (the-object doc selection) :mask) (+ x (* y w))))))
    (let* ((gw 23) (gh 17) (bytes (test-noise (* gw gh) 7))
           (site? (lambda (byte) (< byte 40)))
           (grid (distances-to bytes gw (list 0 0 gw gh) site?)))
      (check "the distance transform is exact, against every pair"
             (loop for y below gh
                   always (loop for x below gw
                                always (= (aref grid (+ x (* y gw)))
                                          (nearest-site-squared bytes gw gh x y site?))))))
    (select-rect! doc 10 8 20 14)
    (contract-selection! doc 3)
    (check "Contract moves a straight edge in by exactly that"
           (and (equal (funcall rect) '(13 11 14 8)) (= 255 (funcall at 13 11)))
           (funcall rect))
    (select-rect! doc 10 8 20 14)
    (contract-selection! doc 2.5)
    (check "half a pixel of it is half a pixel's coverage"
           (and (equal (funcall rect) '(12 10 16 10))
                (< 120 (funcall at 12 15) 135)
                (= 255 (funcall at 13 15)))
           (list (funcall rect) (funcall at 12 15)))
    (select-rect! doc 10 8 20 14)
    (expand-selection! doc 3)
    (check "Expand moves it out, and rounds the corners"
           (and (equal (funcall rect) '(7 5 26 20))
                (= 255 (funcall at 7 12))
                (zerop (funcall at 7 5))
                (= 255 (funcall at 15 15)))
           (funcall rect))
    (select-rect! doc 0 0 20 30)
    (contract-selection! doc 3)
    (check "what reaches the picture's edge stays on it"
           (equal (funcall rect) '(0 0 17 30))
           (funcall rect))
    (select-rect! doc 0 0 w h)
    (contract-selection! doc 5)
    (check "everything selected stays selected"
           (equal (funcall rect) (list 0 0 w h)))
    (select-rect! doc 10 10 4 4)
    (contract-selection! doc 3)
    (check "contracted past its middle a selection is gone"
           (null (the-object doc selection)))
    (contract-selection! doc 3)
    (expand-selection! doc 3)
    (check "with no selection there is nothing to do"
           (null (the-object doc selection)))
    (select-rect! doc 10 8 20 14)
    (expand-selection! doc 100)
    (check "Expand stops at the picture's edge"
           (equal (funcall rect) (list 0 0 w h))
           (funcall rect))
    (let ((stratum-api:*document* doc))
      (stratum-api:select-rect 10 8 20 14)
      (let ((results (stratum:run-forms "(contract-selection 3) (expand-selection 1)")))
        (check "the prompt may use them"
               (and (every (lambda (r) (getf r :ok)) results)
                    (equal (funcall rect) '(12 10 16 10)))
               (list results (funcall rect)))))))

(defun cut-out-card (w h &key textured rim)
  "A disk on a background, its edge pixels mixed in linear light by
their exact coverage, as a camera mixes them; the coverage as the
selection.  The subject a flat orange, or with TEXTURED a gradient on
noise; with RIM, a ring of white just inside the edge, as sharpening
leaves.  The document and each pixel's true subject color, three
doubles a pixel."
  (let* ((pixels (make-rgba w h))
         (mask (make-array (* w h) :element-type '(unsigned-byte 8)))
         (truth (make-array (* 3 w h) :element-type 'double-float))
         (noise (test-noise (* 3 w h) 11))
         (cx (/ w 2d0)) (cy (/ h 2d0)) (radius (* 0.3d0 (min w h))))
    (flet ((linear (byte) (srgb->linear (/ byte 255d0))))
      (dotimes (y h)
        (dotimes (x w)
          (let ((inside 0) (i (+ x (* y w))))
            (dotimes (sy 8)
              (dotimes (sx 8)
                (let ((dx (- (+ x (/ (+ sx 0.5d0) 8)) cx)) (dy (- (+ y (/ (+ sy 0.5d0) 8)) cy)))
                  (when (< (+ (* dx dx) (* dy dy)) (* radius radius)) (incf inside)))))
            (let* ((a (/ inside 64d0))
                   (depth (- radius (sqrt (+ (expt (- x -0.5d0 cx) 2) (expt (- y -0.5d0 cy) 2)))))
                   (own (if textured
                            (list (+ 0.2d0 (* 0.6d0 (/ x w))) (+ 0.1d0 (* 0.3d0 (/ y h))) 0.05d0)
                            (mapcar #'linear '(220 140 40))))
                   (subject (if (and rim (< 0.5d0 depth 2d0)) (list 1d0 1d0 1d0) own))
                   (background (if textured
                                   (loop for c below 3 collect (* 0.8d0 (/ (aref noise (+ c (* 3 i))) 255d0)))
                                   (mapcar #'linear '(30 60 200)))))
              (setf (aref mask i) (round (* 255 a)))
              (loop for c from 0 for s in subject for b in background for o in own
                    do (setf (aref truth (+ c (* 3 i))) o
                             (aref pixels (+ c (* 4 i)))
                             (unit->byte (linear->srgb (+ (* a s) (* (- 1 a) b))))))
              (setf (aref pixels (+ 3 (* 4 i))) 255))))))
    (let ((doc (make-file-document "card" w h pixels)))
      (the-object doc (set-slot! :selection (make-selection mask w h)))
      (values doc truth))))

(defun cut-out-error (doc truth &key (from 1) (to 255))
  "How far, in linear light, the top layer's colors lie from the subject's
own over the pixels the selection covers by FROM to TO of 255: the root
mean square, weighted by coverage."
  (let* ((mask (getf (the-object doc selection) :mask))
         (pixels (the-object (car (last (the-object doc layer-list))) pixels))
         (sum 0d0) (weight 0d0))
    (dotimes (i (length mask) (sqrt (/ sum (max weight 1d-9))))
      (let ((m (aref mask i)))
        (when (<= from m to)
          (let ((a (/ m 255d0)))
            (dotimes (c 3)
              (incf sum (* a (expt (- (aref *srgb-decode* (aref pixels (+ c (* 4 i))))
                                      (aref truth (+ c (* 3 i))))
                                   2))))
            (incf weight a)))))))

(defun refine-card (w h &key (wobble 7) (points 24) (seed 5) (noise 20)
                          (above '(170 200 215)) (below '(230 120 40))
                          (left '(180 150 150)) (right '(150 170 155)))
  "A subject for Refine Edge: a blob whose radius swings with the angle,
ABOVE's color in its top half and BELOW's in the bottom, on a background
LEFT's on the left and RIGHT's on the right, all on NOISE levels of
noise, the edge pixels mixed in linear light by their coverage.  It is
selected by a polygon of POINTS points, each off the outline by up to
WOBBLE pixels, as one drawn by eye.  The document and the true mask, 255
where the subject covers half a pixel or more."
  (let* ((pixels (make-rgba w h))
         (truth (make-array (* w h) :element-type '(unsigned-byte 8)))
         (jitter (test-noise (* 3 w h) seed))
         (cx (/ w 2d0)) (cy (/ h 2d0)) (scale (/ (min w h) 180d0)))
    (flet ((reach (angle) (* scale (+ 60 (* 15 (sin (* 3 angle))) (* 8 (cos (* 5 angle))))))
           (linear (level) (srgb->linear (max 0d0 (min 1d0 (/ level 255d0))))))
      (dotimes (y h)
        (dotimes (x w)
          (let ((inside 0) (i (+ x (* y w))))
            (dotimes (sy 4)
              (dotimes (sx 4)
                (let ((dx (- (+ x (/ (+ sx 0.5d0) 4)) cx)) (dy (- (+ y (/ (+ sy 0.5d0) 4)) cy)))
                  (when (< (sqrt (+ (* dx dx) (* dy dy))) (reach (atan dy dx))) (incf inside)))))
            (let ((a (/ inside 16d0)))
              (setf (aref truth i) (if (>= a 0.5d0) 255 0))
              (loop for c from 0
                    for s in (if (< y cy) above below)
                    for b in (if (< x cx) left right)
                    do (let ((n (* noise (- (/ (aref jitter (+ c (* 3 i))) 255d0) 0.5d0))))
                         (setf (aref pixels (+ c (* 4 i)))
                               (unit->byte (linear->srgb (+ (* a (linear (+ s n))) (* (- 1 a) (linear (+ b n)))))))))
              (setf (aref pixels (+ 3 (* 4 i))) 255)))))
      (let ((doc (make-file-document "refine" w h pixels))
            (offsets (test-noise points (1+ seed))))
        (select-polygon! doc (loop for k below points
                                   collect (let* ((angle (* 2 +pi+ (/ k points)))
                                                  (r (+ (reach angle) (* wobble (- (/ (aref offsets k) 127.5d0) 1)))))
                                             (list (+ cx (* r (cos angle))) (+ cy (* r (sin angle)))))))
        (values doc truth)))))

(defun selection-misses (doc truth)
  "How many pixels the selection, taken at half, puts on the other side
from TRUTH."
  (let ((mask (getf (the-object doc selection) :mask)))
    (loop for i below (length truth)
          count (not (eql (>= (aref mask i) 128) (>= (aref truth i) 128))))))

(defun test-cut-out ()
  "Cut Out: the edge's colors the subject's own, against cards whose
truth is known; the interior LAYER's own bytes; the same on one thread
and many."
  (let ((w 96) (h 72))
    (flet ((copied (doc)
             (copy-selection! (first (the-object doc layer-list)))
             (paste-clipboard! doc)
             doc)
           (cut (doc &rest keys)
             (apply #'cut-out-selection! (first (the-object doc layer-list)) keys)
             doc))
      (multiple-value-bind (doc truth) (cut-out-card w h)
        (let ((copy-error (cut-out-error (copied doc) truth :to 254)))
          (multiple-value-bind (doc truth) (cut-out-card w h)
            (let ((cut-error (cut-out-error (cut doc :defringe 0) truth :to 254)))
              (check "on a flat subject the edge comes out the subject's color, not the mix"
                     (and (> copy-error 0.2d0) (< cut-error 0.01d0))
                     (list copy-error cut-error))))))
      (multiple-value-bind (doc truth) (cut-out-card w h :textured t)
        (let ((copy-error (cut-out-error (copied doc) truth :to 254)))
          (multiple-value-bind (doc truth) (cut-out-card w h :textured t)
            (let ((fused (cut-out-error (cut doc :defringe 0) truth :to 254)))
              (multiple-value-bind (doc truth) (cut-out-card w h :textured t)
                (let ((defringed (cut-out-error (cut doc :defringe 3) truth :to 254)))
                  (check "on a gradient over noise, under half Copy's error, a sixth defringed"
                         (and (< fused (/ copy-error 2.5d0)) (< defringed (/ copy-error 6)))
                         (list copy-error fused defringed))))))))
      (multiple-value-bind (doc truth) (cut-out-card w h :rim t)
        (let ((plain (cut-out-error (cut doc :defringe 0) truth :from 255)))
          (multiple-value-bind (doc truth) (cut-out-card w h :rim t)
            (let ((defringed (cut-out-error (cut doc :defringe 3) truth :from 255)))
              (check "defringe takes away a bright rim inside the edge"
                     (< defringed (/ plain 2))
                     (list plain defringed))))))
      (let* ((doc (cut (cut-out-card w h) :defringe 3))
             (source (the-object (first (the-object doc layer-list)) pixels))
             (result (the-object (second (the-object doc layer-list)) pixels))
             (mask (getf (the-object doc selection) :mask))
             (middle (* 4 (+ (floor w 2) (* (floor h 2) w)))))
        (check "deep inside, the pixels are the layer's own; outside, clear"
               (and (equalp (subseq result middle (+ middle 4)) (subseq source middle (+ middle 4)))
                    (zerop (aref result 3)))
               (subseq result middle (+ middle 4)))
        (check "the cut-out's alpha is the selection's"
               (loop for i below (* w h) always (= (aref result (+ 3 (* 4 i))) (aref mask i)))))
      (let ((pixels (test-noise (* 4 w h) 5)))
        (flet ((rect-doc ()
                 (let ((doc (make-file-document "rect" w h (copy-seq pixels))))
                   (select-rect! doc 20 10 40 30)
                   doc)))
          (check "a hard-edged selection with no defringe is Copy and Paste exactly"
                 (equalp (the-object (second (the-object (copied (rect-doc)) layer-list)) pixels)
                         (the-object (second (the-object (cut (rect-doc) :defringe 0) layer-list)) pixels)))))
      (let ((many (the-object (second (the-object (cut (cut-out-card w h :textured t)) layer-list)) pixels))
            (one (let ((*render-threads* 1))
                   (the-object (second (the-object (cut (cut-out-card w h :textured t)) layer-list)) pixels))))
        (check "one thread and many cut out the same bytes" (equalp many one)))
      (let ((doc (make-file-document "none" w h (make-rgba w h))))
        (check "with nothing selected, an error in words"
               (search "select the subject"
                       (handler-case (progn (cut doc) "no error")
                         (error (e) (princ-to-string e))))))
      (let ((doc (cut-out-card w h)))
        (let ((stratum-api:*document* doc))
          (let ((results (stratum:run-forms "(cut-out 0 :name \"Disk\" :defringe 2) (cut-out 0 :defringe 50)")))
            (check "the prompt may cut out, and a wild defringe is refused in words"
                   (and (getf (first results) :ok)
                        (equal (getf (getf (first results) :value) :name) "Disk")
                        (= 1 (getf (getf (first results) :value) :index))
                        (not (getf (second results) :ok))
                        (search "0 to 20" (getf (second results) :error)))
                   results)))))))

(defun test-refine-selection ()
  "Refine Edge: a polygon drawn by eye moved onto the outline by color;
nothing beyond the band touched; the same on one thread and many."
  (let ((w 240) (h 180))
    (flet ((refined (radius &rest card)
             (let ((passes (getf card :passes 5))
                   (card (loop for (key value) on card by #'cddr
                               unless (eq key :passes) append (list key value))))
               (multiple-value-bind (doc truth) (apply #'refine-card w h card)
                 (let ((before (selection-misses doc truth))
                       (mask (copy-seq (getf (the-object doc selection) :mask))))
                   (refine-selection! doc :radius radius :passes passes)
                   (values before (selection-misses doc truth) doc mask))))))
      (multiple-value-bind (before after) (refined 8)
        (check "a polygon off by up to 7 pixels comes onto the outline: a fifteenth of the misses"
               (and (> before 1000) (< after (/ before 15)))
               (list before after)))
      (multiple-value-bind (before after) (refined 8 :right '(175 190 200))
        (check "with a background near the subject's color, a seventh of the misses"
               (< after (/ before 7))
               (list before after)))
      (multiple-value-bind (before one) (refined 8 :wobble 20 :passes 1)
        (let ((five (nth-value 1 (refined 8 :wobble 20))))
          (check "a polygon off by 20 pixels, past one pass's reach, comes on over five"
                 (and (> one (* 5 five)) (< five (/ before 30)))
                 (list before one five))))
      (multiple-value-bind (before after doc mask) (refined 8)
        (declare (ignore before after))
        (let ((now (getf (the-object doc selection) :mask)))
          (check "deep inside and far outside, the selection stays"
                 (and (= 255 (aref now (+ 120 (* 90 w))) (aref mask (+ 120 (* 90 w))))
                      (= 0 (aref now 0) (aref mask 0))
                      (every (lambda (byte) (or (= byte 0) (= byte 255))) now)))))
      (let ((many (nth-value 2 (refined 8)))
            (one (let ((*render-threads* 1)) (nth-value 2 (refined 8)))))
        (check "one thread and many refine the same"
               (equalp (getf (the-object many selection) :mask) (getf (the-object one selection) :mask))))
      (let ((doc (make-file-document "none" w h (make-rgba w h))))
        (check "with nothing selected, an error in words"
               (search "outline roughly"
                       (handler-case (progn (refine-selection! doc) "no error")
                         (error (e) (princ-to-string e))))))
      (let ((doc (refine-card w h)))
        (let ((stratum-api:*document* doc))
          (let ((results (stratum:run-forms "(refine-selection :radius 6) (refine-selection :radius 500)")))
            (check "the prompt may refine, and a wild radius is refused in words"
                   (and (getf (first results) :ok)
                        (getf (getf (first results) :value) :selection)
                        (not (getf (second results) :ok))
                        (search "1 to 100" (getf (second results) :error)))
                   results)))))))

(defun test-select-subject ()
  "Select Subject: the mask's specks and holes, the decoder's square
stretched to the picture, refusals in words; and, on a server with the
model, a subject found from a rough box."
  (let ((mask (make-gray 40 30)))
    (flet ((block! (x0 y0 x1 y1 value)
             (loop for y from y0 below y1
                   do (loop for x from x0 below x1 do (setf (aref mask (+ x (* y 40))) value)))))
      (block! 0 5 30 25 255)              ; the subject, against the left edge
      (block! 10 10 13 13 0)              ; a small hole in it
      (block! 0 14 2 16 0)                ; a notch at the picture's edge
      (block! 35 2 37 4 255)              ; a speck away from it
      (without-specks! mask 40 30 0.02d0)
      (check "specks go, small holes fill, a notch at the picture's edge stays"
             (and (= 255 (aref mask (+ 11 (* 11 40))))
                  (zerop (aref mask (+ 35 (* 2 40))))
                  (zerop (aref mask (* 15 40)))
                  (= 255 (aref mask (+ 20 (* 20 40))))))))
  (let ((logits (make-array (* 256 256) :element-type 'single-float :initial-element -1f0)))
    (dotimes (y 256)
      (dotimes (x 128) (setf (aref logits (+ x (* y 256))) 1f0)))
    (let ((mask (stretched-logits logits 100 50)))
      (check "the decoder's square is stretched over the picture"
             (and (= 255 (aref mask (+ 48 (* 10 100))))
                  (zerop (aref mask (+ 52 (* 10 100))))
                  (= 50 (count 255 mask :start 0 :end 100))))))
  (let ((doc (refine-card 240 180)))
    (let ((*subject-model* nil))
      (check "with no model, an error in words, and the planner is told"
             (and (search "no subject model"
                          (handler-case (progn (select-subject! doc 10 10 100 100) "no error")
                            (error (e) (princ-to-string e))))
                  (search "has no subject model" (subject-state)))))
    (let ((*subject-model* '(:not-a-session)))
      (check "a box off the picture and points that are no points are refused in words"
             (and (search "inside the picture"
                          (handler-case (progn (select-subject! doc 500 10 20 20) "no error")
                            (error (e) (princ-to-string e))))
                  (search "Points are"
                          (handler-case (progn (select-subject! doc 10 10 50 50 :points '(1 2)) "no error")
                            (error (e) (princ-to-string e))))))))
  (flet ((columns (x0 x1)
           (let ((m (make-gray 40 10)))
             (dotimes (y 10 m)
               (loop for x from x0 below x1 do (setf (aref m (+ x (* y 40))) 255))))))
    (let ((near (joined-masks (columns 0 10) (columns 12 20) 40 10 2))
          (far (joined-masks (columns 0 10) (columns 20 30) 40 10 2)))
      (check "two subjects a seam apart are joined; a real gap between them stays"
             (and (every (lambda (x) (= 255 (aref near (+ x (* 5 40))))) '(10 11))
                  (zerop (aref near (+ 25 (* 5 40))))
                  (every (lambda (x) (zerop (aref far (+ x (* 5 40))))) '(10 13 16 19))))))
  (flet ((mask (n) (let ((m (make-gray 10 10))) (fill m 255 :end n) m)))
    (let ((part (mask 30)) (whole (mask 80)) (wild (mask 100)) (box '(0 0 9 7)))
      (check "of the candidates scored near the best, the largest; one far below is passed over"
             (eq whole (widest-near-best (list (cons 0.82f0 part) (cons 0.80f0 whole) (cons 0.60f0 wild))
                                         box 10 10)))
      (check "a candidate spilling far out of the box is passed over too"
             (eq whole (widest-near-best (list (cons 0.82f0 part) (cons 0.80f0 whole) (cons 0.81f0 wild))
                                         '(0 0 9 6) 10 10)))))
  (when (subject-model?)
    (multiple-value-bind (doc truth) (refine-card 240 180)
      (select-subject! doc 40 10 165 165)
      (let ((misses (selection-misses doc truth)))
        (check "with the model, a rough box gives the subject: under 2% of the picture off"
               (< misses (* 0.02 240 180))
               misses))
      (the-object doc (add-layer! (plist-merge (list :pixels (let ((p (make-rgba 240 180)))
                                                               (dotimes (i (* 240 180) p)
                                                                 (setf (aref p (* 4 i)) 90 (aref p (+ 1 (* 4 i))) 140
                                                                       (aref p (+ 2 (* 4 i))) 200 (aref p (+ 3 (* 4 i))) 255))))
                                               (the-object doc (new-layer-spec :raster)))))
      (select-subject! doc 40 10 165 165 :layer (first (the-object doc layer-list)))
      (let ((misses (selection-misses doc truth)))
        (check "under a layer that covers it, :layer finds the subject in its own pixels"
               (< misses (* 0.02 240 180))
               misses)))))

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
                  (and (= 2 (getf (clipboard) :w)) (= 2 (getf (clipboard) :h))
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

(defun test-rotate ()
  ;; A 3 x 2 picture whose pixels are numbered 1..6, row by row.
  (let ((pic (let ((p (make-rgba 3 2)))
               (dotimes (i 6 p)
                 (let ((o (* 4 i))) (setf (aref p o) (1+ i) (aref p (+ o 1)) (1+ i) (aref p (+ o 2)) (1+ i) (aref p (+ o 3)) 255))))))
    (flet ((numbers (pixels) (loop for i from 0 below (length pixels) by 4 collect (aref pixels i))))
      (check "the quarter turns, the half turn and the flips move the pixels whole"
             (and (equal (numbers (turn-rgba pic 3 2 :cw)) '(4 1 5 2 6 3))
                  (equal (numbers (turn-rgba pic 3 2 :ccw)) '(3 6 2 5 1 4))
                  (equal (numbers (turn-rgba pic 3 2 :180)) '(6 5 4 3 2 1))
                  (equal (numbers (turn-rgba pic 3 2 :flip-h)) '(3 2 1 6 5 4))
                  (equal (numbers (turn-rgba pic 3 2 :flip-v)) '(4 5 6 1 2 3)))
             (list (numbers (turn-rgba pic 3 2 :cw)) (numbers (turn-rgba pic 3 2 :ccw))))
      (check "a quarter turn each way, and four the same way, give the picture back"
             (and (equalp pic (turn-rgba (turn-rgba pic 3 2 :cw) 2 3 :ccw))
                  (equalp pic (turn-rgba (turn-rgba (turn-rgba (turn-rgba pic 3 2 :cw) 2 3 :cw) 3 2 :cw) 2 3 :cw))))
      (check "a mask turns on one channel"
             (equalp (turn-rgba (coerce #(1 2 3 4 5 6) '(simple-array (unsigned-byte 8) (*))) 3 2 :cw 1)
                     (coerce #(4 1 5 2 6 3) '(simple-array (unsigned-byte 8) (*)))))))
  (let ((pic (let ((p (make-rgba 10 10)))
               (dotimes (y 10 p)
                 (dotimes (x 10)
                   (let ((o (* 4 (+ x (* 10 y)))))
                     (setf (aref p o) (* 25 x) (aref p (+ o 1)) (* 25 y) (aref p (+ o 2)) 128 (aref p (+ o 3)) 255)))))))
    (check "an arbitrary 90 degrees through resampling equals the whole-pixel turn within a level"
           (multiple-value-bind (nw nh) (rotated-size 10 10 90)
             (and (= nw 10) (= nh 10)
                  (<= (max-byte-diff (rotate-rgba pic 10 10 90) (turn-rgba pic 10 10 :cw)) 1))))
    (check "45 degrees grows the canvas, keeps the center and clears the corners"
           (multiple-value-bind (nw nh) (rotated-size 10 10 45)
             (let ((r (rotate-rgba pic 10 10 45)))
               (and (= nw 15) (= nh 15)
                    (= 0 (aref r 3)) (= 0 (aref r (+ 3 (* 4 (+ 14 (* 15 14))))))
                    (= 255 (aref r (+ 3 (* 4 (+ 7 (* 15 7))))))
                    (<= (abs (- (aref r (* 4 (+ 7 (* 15 7)))) 112)) 14)))))
    (check "the half turn and a flip are each their own inverse through the resampler too"
           (<= (max-byte-diff (rotate-rgba (rotate-rgba pic 10 10 180) 10 10 180) pic) 1)))
  (let* ((doc (make-sample-document "parity"))
         (sky (the-object doc (layer 0))))
    (the-object sky (set-mask! '(:flat :value 255)))
    (apply-brush-stroke sky (list :points '((10 10 1)) :color '(0 0 0) :radius 3 :hardness 1 :flow 1 :opacity 1) :target :mask)
    (set-color-mode! doc :grayscale)
    (let* ((before (copy-seq (the-object sky pixels)))
           (mask-before (copy-seq (the-object sky mask-pixels)))
           (turned (rotate-document doc :cw))
           (turned-sky (the-object turned (layer 0))))
      (check "a turned document has the sides swapped, every layer and mask turned, and keeps its mode"
             (and (= 200 (the-object turned doc-width)) (= 320 (the-object turned doc-height))
                  (equalp (the-object turned-sky pixels) (turn-rgba before 320 200 :cw))
                  (equalp (the-object turned-sky mask-pixels) (turn-rgba mask-before 320 200 :cw 1))
                  (eql (the-object turned color-mode) :grayscale)
                  (equal (layer-names turned) (layer-names doc))))
      (check "an arbitrary multiple of 90 turns whole pixels; 360 changes nothing"
             (and (equalp (the-object (rotate-document-arbitrary doc -90) (layer 0) pixels) (turn-rgba before 320 200 :ccw))
                  (equalp (the-object (rotate-document-arbitrary doc 360) (layer 0) pixels) before))))))

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

(defun test-drawing ()
  (let ((d "M 0 0 L 10 0 C 10 5 20 5 20 0 Z M 1 1 2 2"))
    (check "a path's M, L, C and Z read as polylines, the curve flattened through its midpoint"
           (let ((lines (svg-path-polylines d)))
             (and (= (length lines) 2)
                  (equal (first (first lines)) '(0d0 . 0d0))
                  (equal (second (first lines)) '(10d0 . 0d0))
                  (= (length (first lines)) (+ 3 *curve-segments*))
                  (let ((mid (nth (1+ (floor *curve-segments* 2)) (first lines))))
                    (and (< (abs (- (car mid) 15)) 1d-9) (< (abs (- (cdr mid) 3.75)) 1d-9)))
                  (equal (first (last (first lines))) '(0d0 . 0d0))
                  (equal (second lines) '((1d0 . 1d0) (2d0 . 2d0)))))
           (svg-path-polylines d)))
  (multiple-value-bind (segments w h)
      (svg-segments "<svg viewBox=\"0 0 100 50\"><g transform=\"translate(10, 5)\"><path d=\"M 0 0 10 0\" stroke-dasharray=\"4 2\"/><g><path d=\"M 0 0 L 0 10\"/></g></g><path d=\"M 1 1 2 2\"/></svg>")
    (check "segments are translated by their groups, the sheet size read from the viewBox, dashes noted"
           (and (= w 100d0) (= h 50d0)
                (equal segments '((10d0 5d0 20d0 5d0 t) (10d0 5d0 10d0 15d0 nil) (1d0 1d0 2d0 2d0 nil))))
           segments))
  (let ((c (make-coverage 16 8)))
    (draw-line c 16 8 2d0 3d0 12d0 3d0)
    (check "a horizontal line covers its row fully between the endpoints, half at them, and nothing beside"
           (and (loop for x from 3 to 11 always (= 1d0 (aref c (+ x (* 16 3)))))
                (= 0.5d0 (aref c (+ 2 (* 16 3)))) (= 0.5d0 (aref c (+ 12 (* 16 3))))
                (loop for x below 16 always (and (zerop (aref c (+ x (* 16 2)))) (zerop (aref c (+ x (* 16 4)))))))
           (coerce c 'list)))
  (let* ((rgba (gen-drawing 64 64 :source "box" :projection :top))
         (rows (loop for y below 64 collect (loop for x below 64 count (plusp (aref rgba (+ 3 (* 4 (+ x (* 64 y)))))))))
         (inked (remove 0 rows)))
    (check "a box seen from the top draws as a rectangle inside the margin: long rows at the ends, thin ones between, black ink"
           (and (every #'zerop (subseq rows 0 3)) (every #'zerop (subseq rows 61))
                (>= (apply #'max rows) 50)
                (<= (nth (floor (length inked) 2) inked) 8)
                (loop for i below (* 64 64)
                      always (let ((o (* 4 i)))
                               (or (zerop (aref rgba (+ o 3)))
                                   (and (zerop (aref rgba o)) (zerop (aref rgba (+ o 1))) (zerop (aref rgba (+ o 2))))))))
           rows))
  (let* ((spec '(:drawing :source "sample" :projection :trimetric))
         (a (generate-pixels spec 320 200))
         (b (generate-pixels spec 320 200))
         (c (generate-pixels spec 160 100))
         (ink (lambda (p) (loop for i from 3 below (length p) by 4 count (plusp (aref p i))))))
    (check "the sample assembly draws with curves and straight edges, cached per size, afresh at another size"
           (and (> (funcall ink a) 3000) (eq a b) (= (length c) (* 4 160 100)) (> (funcall ink c) 800)
                (< (funcall ink c) (funcall ink a)))
           (list (funcall ink a) (funcall ink c))))
  ;; Hidden-line removal on the sample (a sphere, a cylinder and a cone
  ;; beside the box) takes well under a second; the first version of this
  ;; check called gen-drawing inside its pixel loop, ten thousand drawings,
  ;; and read the minutes that took as a stall in geom-base.  Draw once.
  (let ((start (get-internal-real-time))
        (p (gen-drawing 100 100 :source "sample" :hidden-lines :dashed)))
    (check "dashed hidden lines draw too, lighter than the visible edges, and the sample's curved solids draw in seconds, not minutes"
           (and (plusp (loop for i from 3 below (length p) by 4 count (plusp (aref p i))))
                (plusp (loop for i from 3 below (length p) by 4 count (< 0 (aref p i) 160)))
                (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))
           (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)))
  (check "the layer draws through gendl-raster when the image has it (text and filled arrowheads), and through the lines here otherwise"
         (let ((canvas (gendl-raster-coverage (drawing-svg (drawing-source "box") :projection :top) 64 64 0.05 1.5 nil)))
           (if (find-package "GENDL-RASTER")
               (and canvas (= (length canvas) (* 64 64)) (plusp (count-if #'plusp canvas)))
               (null canvas))))
  (let* ((doc (make-sample-document "parity"))
         (before (the-object doc (render-cpu)))
         (count (length (the-object doc layer-list)))
         (i (place-drawing! doc "sample" :projection :front :color '(200 0 0))))
    (check "place-drawing! adds a raster layer named for the model and the view, on top, and it inks the picture"
           (and (= i count) (= (length (the-object doc layer-list)) (1+ count))
                (equal (the-object doc (layer i) layer-name) "sample (front)")
                (eql (the-object doc (layer i) kind) :raster)
                (not (equalp (the-object doc (render-cpu)) before)))
           (layer-names doc)))
  (let ((doc (open-drawing (make-object 'box :width 3 :length 2 :height 1) :name "drawing-test" :width 200 :height 120)))
    (check "open-drawing registers a document of a white Background and the model's drawing"
           (and (gethash "drawing-test" *documents*)
                (equal (layer-names doc) '("Background" "drawing-test (trimetric)"))
                (let ((p (the-object doc (render-cpu))))
                  (and (loop for i from 3 below (length p) by 4 always (= 255 (aref p i)))
                       (plusp (loop for i below (length p) by 4 count (< (aref p i) 128)))))))
    (remhash "drawing-test" *documents*)
    (remhash "drawing-test" *drawing-sources*)))

(defun test-ora ()
  ;; The zip layer.
  (let* ((flat (make-array 5000 :element-type '(unsigned-byte 8) :initial-element 7))
         (text (string-octets "<a>b &amp; c</a>"))
         (none (make-array 0 :element-type '(unsigned-byte 8)))
         (archive (zip-octets (list (list "mimetype" text :method :stored)
                                    (list "data/flat.bin" flat :method :deflate)
                                    (list "empty" none :method :deflate))))
         (back (zip-entries archive)))
    (check "a zip archive round-trips its entries, stored and deflated, in order"
           (and (zip-octets-p archive)
                (equal (mapcar #'car back) '("mimetype" "data/flat.bin" "empty"))
                (equalp (cdr (first back)) text)
                (equalp (cdr (second back)) flat)
                (zerop (length (cdr (third back))))
                (< (length archive) 5000))
           (mapcar #'car back))
    (check "a file that is not a zip archive is refused"
           (handler-case (progn (zip-entries text) nil) (error () t))))
  ;; A foreign file: stack.xml as another editor writes it (a
  ;; declaration, a comment, a nested stack with its own opacity,
  ;; entities, defaults left out), every entry deflated.
  (let* ((w 4) (h 3)
         (bg (let ((p (make-rgba w h)))
               (dotimes (i (* w h) p) (setf (aref p (* 4 i)) (* 20 i) (aref p (+ 3 (* 4 i))) 255))))
         (small (let ((p (make-rgba 2 2)))
                  (dotimes (i 4 p) (setf (aref p (* 4 i)) 255 (aref p (+ 3 (* 4 i))) 255))))
         (stack (format nil "<?xml version='1.0' encoding='UTF-8'?>~%<image version=\"0.0.3\" w=\"4\" h=\"3\">~%  <!-- a comment -->~%  <stack>~%    <layer src=\"data/top.png\" name=\"Top &amp; hidden\" x=\"0\" y=\"0\" opacity=\"1.0\" visibility=\"hidden\" composite-op=\"svg:overlay\"/>~%    <stack name=\"Group\" opacity=\"0.5\">~%      <layer src=\"data/small.png\" name=\"Small &#x41;\" x=\"1\" y=\"1\" opacity=\"0.8\" composite-op=\"svg:multiply\"/>~%    </stack>~%    <layer src=\"data/bg.png\" name=\"Background\"/>~%  </stack>~%</image>~%"))
         (archive (zip-octets (list (list "mimetype" (string-octets "image/openraster"))
                                    (list "stack.xml" (string-octets stack) :method :deflate)
                                    (list "data/top.png" (png-octets bg w h) :method :deflate)
                                    (list "data/small.png" (png-octets small 2 2) :method :deflate)
                                    (list "data/bg.png" (png-octets bg w h) :method :deflate)
                                    (list "mergedimage.png" (png-octets bg w h) :method :deflate))))
         (image (read-ora archive))
         (layers (ora-image-layers image)))
    (check "a foreign OpenRaster file reads: the nested stack flattened with its opacity multiplied in, visibility inherited, entities decoded, defaults filled"
           (and (= (ora-image-width image) 4) (= (ora-image-height image) 3)
                (equal (ora-image-version image) "0.0.3")
                (equal (mapcar #'ora-layer-name layers) '("Top & hidden" "Small A" "Background"))
                (not (ora-layer-visible? (first layers)))
                (= (ora-layer-opacity (second layers)) 0.4d0)
                (= (ora-layer-x (second layers)) 1) (= (ora-layer-y (second layers)) 1)
                (equal (ora-layer-composite-op (third layers)) "svg:src-over")
                (= (ora-layer-opacity (third layers)) 1d0)
                (null (ora-image-thumbnail image)))
           (mapcar #'ora-layer-name layers))
    (multiple-value-bind (doc unsupported) (ora-image-document image "foreign")
      (check "it becomes a document, bottom first, the small layer placed at its offset, blend modes mapped and the one Stratum lacks reported"
             (and (equal (layer-names doc) '("Background" "Small A" "Top & hidden"))
                  (equal (mapcar (lambda (l) (the-object l blend-mode)) (the-object doc raster-layers))
                         '(:normal :multiply :normal))
                  (equal (mapcar (lambda (l) (the-object l visible?)) (the-object doc layer-list)) '(t t nil))
                  (equal unsupported '(("Top & hidden" "svg:overlay")))
                  (let ((p (the-object doc (layer 1) pixels)))
                    (equal (loop for y below 3
                                 collect (loop for x below 4 collect (aref p (+ 3 (* 4 (+ x (* 4 y)))))))
                           '((0 0 0 0) (0 255 255 0) (0 255 255 0)))))
             (list (layer-names doc) unsupported)))
    (check "an archive without stack.xml, and one with another mimetype, are refused"
           (and (handler-case (progn (read-ora (zip-octets (list (list "mimetype" (string-octets "image/openraster"))))) nil)
                  (error () t))
                (handler-case (progn (read-ora (zip-octets (list (list "mimetype" (string-octets "image/png"))
                                                                 (list "stack.xml" (string-octets stack)))))
                                     nil)
                  (error () t)))))
  ;; Stratum's own documents out and back.
  (let ((doc (make-sample-document "parity")))
    (the-object doc (layer 1) (set-slot! :opacity 0.3d0))
    (the-object doc (layer 2) (set-slot! :master '((0 0) (0.4 0.5) (1 1))))
    (apply-brush-stroke (the-object doc (layer 0)) *test-stroke*)
    (the-object doc (layer 6) (set-mask! '(:flat :value 128)))
    (apply-brush-stroke (the-object doc (layer 6)) *test-stroke* :target :mask)
    (let* ((render (the-object doc (render-cpu)))
           (octets (document-ora-octets doc))
           (image (read-ora octets))
           (layers (ora-image-layers image)))
      (check "the file is a zip with the stored mimetype first, every standard part present"
             (and (zip-octets-p octets)
                  (zerop (le16 octets 8))
                  (equal (octets-string (subseq octets 30 38)) "mimetype")
                  (equal (octets-string (subseq octets 38 54)) "image/openraster")
                  (ora-image-merged image) (ora-image-thumbnail image)
                  (= (length layers) (length (the-object doc layer-list))))
             (length octets))
      (check "a deflated entry chipz cannot finish without its look-ahead byte reads back whole"
             (let* ((text (string-octets (ora-stack-xml image)))
                    (back (zip-entries (zip-octets (list (list "stack.xml" text :method :deflate))))))
               (equalp (cdr (first back)) text)))
      (check "the merged image is the render exactly; the thumbnail is 256 wide with the aspect kept"
             (and (equalp (read-png (ora-image-merged image)) render)
                  (multiple-value-bind (p tw th) (read-png (ora-image-thumbnail image))
                    (declare (ignore p))
                    (and (= tw 256) (= th 160)))))
      (check "layers come out top first with their opacity and blend; adjustment layers hidden and empty, their spec and mask attached"
             (and (equal (mapcar #'ora-layer-name layers) (reverse (layer-names doc)))
                  (= (ora-layer-opacity (nth 7 layers)) 0.3d0)
                  (equal (mapcar #'ora-layer-composite-op layers)
                         '("svg:src-over" "svg:src-over" "svg:plus" "svg:multiply" "svg:src-over"
                           "svg:src-over" "svg:src-over" "svg:screen" "svg:src-over"))
                  (not (ora-layer-visible? (first layers)))
                  (assoc "stratum:spec" (ora-layer-attributes (first layers)) :test #'string=)
                  (assoc "stratum:mask" (ora-layer-attributes (first layers)) :test #'string=)
                  (null (ora-layer-attributes (third layers)))
                  (equal (ora-image-namespaces image) (list *stratum-xmlns*)))
             (mapcar #'ora-layer-attributes layers))
      (check "a masked raster layer's PNG carries the mask baked into its alpha"
             (let ((pixels (the-object doc (layer 6) pixels))
                   (mask (the-object doc (layer 6) mask-pixels))
                   (png (ora-layer-rgba (nth 2 layers))))
               (loop for i below (length mask)
                     always (= (aref png (+ 3 (* 4 i)))
                               (round (* (aref pixels (+ 3 (* 4 i))) (aref mask i)) 255)))))
      (multiple-value-bind (back unsupported) (ora-image-document image "back")
        (let* ((render2 (the-object back (render-cpu)))
               (maxdiff (loop for i below (length render) maximize (abs (- (aref render i) (aref render2 i))))))
          (check "loaded back, it is the same stack: names, kinds, opacity, the adjustments and their masks whole, the raster mask baked"
                 (and (null unsupported)
                      (equal (layer-names back) (layer-names doc))
                      (equal (mapcar (lambda (l) (the-object l kind)) (the-object back layer-list))
                             (mapcar (lambda (l) (the-object l kind)) (the-object doc layer-list)))
                      (= (the-object back (layer 1) opacity) 0.3d0)
                      (equal (the-object back (layer 2) master) '((0 0) (0.4 0.5) (1 1)))
                      (the-object back (layer 8) has-mask?)
                      (equalp (the-object back (layer 8) mask-pixels) (the-object doc (layer 8) mask-pixels))
                      (not (the-object back (layer 6) has-mask?)))
                 (layer-names back))
          (check "loaded back, it renders within one level (the baked mask rounds once)"
                 (<= maxdiff 1) maxdiff))))))

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
           (progn (filter-layer! discs :gaussian-blur :settings '(:radius 2))
                  (and (not (equalp before (the-object discs pixels)))
                       (equal (demand doc) '(1 1)))))
    (check "undo gives the layer back after a blur"
           (and (undo-stroke! doc) (equalp before (the-object discs pixels))))
    (check "under a selection a blur changes the selected pixels only"
           (progn (select-rect! doc 100 60 60 40)
                  (filter-layer! discs :gaussian-blur :settings '(:radius 2))
                  (let ((after (the-object discs pixels)))
                    (and (not (equalp before after))
                         (loop for i below (length before)
                               for px = (mod (floor i 4) w) for py = (floor (floor i 4) w)
                               always (or (and (<= 100 px 159) (<= 60 py 99))
                                          (= (aref before i) (aref after i)))))))))
  (let ((step (let ((p (make-rgba 8 1)))
                (dotimes (x 8 p)
                  (let ((o (* 4 x)) (v (if (< x 4) 60 180)))
                    (setf (aref p o) v (aref p (+ o 1)) v (aref p (+ o 2)) v (aref p (+ o 3)) 255))))))
    (check "Unsharp Mask at no amount changes nothing"
           (equalp (unsharp-mask-rgba step 8 1 1 0 0) step))
    (check "Unsharp Mask pushes the two sides of an edge apart and leaves the flat ends"
           (let ((s (unsharp-mask-rgba step 8 1 1 1 0)))
             (and (< (aref s (* 4 3)) 60) (> (aref s (* 4 4)) 180)
                  (= (aref s 0) 60) (= (aref s (* 4 7)) 180))))
    (check "a threshold above the edge's difference leaves it alone"
           (equalp (unsharp-mask-rgba step 8 1 1 1 200) step))
    (check "noise-hash is in [0, 1) and differs by pixel, channel and seed"
           (let ((a (noise-hash 3 4 0 1)) (b (noise-hash 4 4 0 1)) (c (noise-hash 3 4 1 1)) (d (noise-hash 3 4 0 2)))
             (and (<= 0 a) (< a 1) (/= a b) (/= a c) (/= a d))))
    (check "Add Noise at no amount changes nothing, and draws the same numbers for a seed"
           (and (equalp (add-noise-rgba step 8 1 0 nil 1) step)
                (equalp (add-noise-rgba step 8 1 0.2 nil 5) (add-noise-rgba step 8 1 0.2 nil 5))
                (not (equalp (add-noise-rgba step 8 1 0.2 nil 5) (add-noise-rgba step 8 1 0.2 nil 6)))))
    (check "monochromatic noise moves the three channels together"
           (let ((n (add-noise-rgba step 8 1 0.2 t 3)))
             (loop for x below 8
                   for o = (* 4 x)
                   always (= (aref n o) (aref n (+ o 1)) (aref n (+ o 2))))))))

(defun old-noise (pixels w h amount monochrome seed)
  "Add Noise as it was: arbitrary integers, one pixel at a time."
  (flet ((hash (x y c)
           (let* ((m #xffffffff)
                  (v (logand (+ (* x 374761393) (* y 668265263) (* c 1274126177) (* seed 2246822519)) m)))
             (setf v (logand (* (logxor v (ash v -13)) 1274126177) m))
             (setf v (logxor v (ash v -16)))
             (/ (float (logand v #xffffff) 1d0) 16777216d0))))
    (let ((out (copy-seq pixels)) (scale (* 255d0 (float amount 1d0))))
      (dotimes (y h out)
        (dotimes (x w)
          (let ((o (* 4 (+ x (* y w)))))
            (dotimes (c 3)
              (let ((v (+ (aref pixels (+ o c)) (* scale (- (* 2d0 (hash x y (if monochrome 0 c))) 1d0)))))
                (setf (aref out (+ o c)) (unit->byte (/ v 255d0)))))))))))

(defun test-selection-rects ()
  "A filter or an adjustment inside a selection works on its rectangle
and the kernel's reach: the same bytes as the whole layer filtered and
blended by the selection, one rectangle on the undo list, and that
rectangle answered."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "parity") stratum-api:*document*))
         (w (the-object doc doc-width))
         (h (the-object doc doc-height))
         (layer (first (the-object doc raster-layers)))
         (index (the-object layer index))
         (pixels (the-object layer (ensure-own-pixels!))))
    (check "Add Noise draws the numbers it always drew"
           (and (equalp (add-noise-rgba pixels w h 0.2 nil 5) (old-noise pixels w h 0.2 nil 5))
                (equalp (add-noise-rgba pixels w h 0.1 t -3) (old-noise pixels w h 0.1 t -3))))
    (dolist (place '((100 50 60 40) (0 0 40 30)))
      (destructuring-bind (x y rw rh) place
        (stratum-api:select-rect x y rw rh)
        (stratum-api:feather 2)
        (let ((selection (the-object doc selection)))
          (dolist (run (list (list "Gaussian Blur"
                                   (lambda () (filter-layer! layer :gaussian-blur :settings '(:radius 4)))
                                   (lambda (p) (gaussian-blur-rgba p w h 4d0)))
                             (list "Unsharp Mask"
                                   (lambda () (filter-layer! layer :unsharp-mask
                                                             :settings '(:radius 3 :amount 150 :threshold 4)))
                                   (lambda (p) (unsharp-mask-rgba p w h 3d0 1.5d0 4)))
                             (list "Add Noise"
                                   (lambda () (filter-layer! layer :add-noise :settings '(:amount 20 :seed 7)))
                                   (lambda (p) (add-noise-rgba p w h 0.2d0 nil 7)))
                             (list "Levels"
                                   (lambda () (adjust-layer! layer :levels :settings '(:gamma 1.4)))
                                   (lambda (p)
                                     (let ((copy (copy-seq p))
                                           (table (byte-table (levels-lut 0d0 1d0 1.4d0 0d0 1d0 256))))
                                       (apply-tables! copy table table table))))
                             (list "Hue/Saturation"
                                   (lambda () (adjust-layer! layer :hue-saturation :settings '(:hue 30)))
                                   (lambda (p)
                                     (apply-hue-saturation! (copy-seq p) (/ 30d0 360) 0d0 0d0)))))
            (destructuring-bind (name act whole) run
              (let* ((before (copy-seq pixels))
                     (want (blend-by-selection! (copy-seq before) (funcall whole before) w h selection))
                     (rect (funcall act))
                     (entry (first (the-object doc undo-entries))))
                (check (format nil "~a in a selection at ~a, ~a: the bytes of the whole layer's" name x y)
                       (equalp pixels want))
                (check (format nil "~a answers the selection's rectangle, and that is all undo holds" name)
                       (and (equal rect (getf selection :rect))
                            (= (entry-bytes entry) (* 4 (third rect) (fourth rect))))
                       (list rect (entry-bytes entry)))
                (stratum-api:undo)
                (check (format nil "~a undoes" name) (equalp pixels before)))))))
      (stratum-api:select-none))
    ;; a prompt's changes, for the page
    (let* ((stamp (the-object layer pixel-stamp))
           (answer (prompt-with doc (format nil "(select-rect 100 50 60 40) (levels ~d :gamma 1.3) (add-layer :raster :name \"New\") (select-none)" index)))
           (changed (getf answer :changed)))
      (check "a prompt says which rectangle of which content it changed"
             (and (= (length changed) 1)
                  (eql (getf (first changed) :from) stamp)
                  (eql (getf (first changed) :stamp)
                       (the-object (first (the-object doc raster-layers)) pixel-stamp))
                  (equal (list (getf (first changed) :x) (getf (first changed) :y)
                               (getf (first changed) :w) (getf (first changed) :h))
                         '(100 50 60 40)))
             changed)
      (check "and the page hears it"
             (= 1 (length (gethash "changed" (yason:parse (prompt-json answer))))))
      (stratum-api:undo))))

(defun test-generate-direct ()
  "The Generate dialog: forms made from its settings, run with no model
asked, one undo like a prompt; and the picture as a generator's input."
  (flet ((settings (&rest kv)
           (let ((table (make-hash-table :test 'equal)))
             (loop for (k v) on kv by #'cddr do (setf (gethash k table) v))
             table)))
    (check "the dialog's settings become one form"
           (and (equal (generation-forms (settings "source" "new" "name" "Sky") "a \"blue\" sky")
                       "(generate-layer \"a \\\"blue\\\" sky\" :name \"Sky\" :seed -1)")
                (search ":keep :changed" (generation-forms (settings "source" "selection") "grass"))
                (search ":keep :all" (generation-forms (settings "source" "selection" "keepAll" t) "grass"))
                (search ":strength 0.30 :reference nil"
                        (generation-forms (settings "source" "picture" "strength" 0.3d0) "grass"))
                (search ":reference t" (generation-forms (settings "source" "picture" "reference" t) "grass"))))
    (check "an unknown source is refused"
           (search "not something"
                   (handler-case (generation-forms (settings "source" "(quit)") "grass")
                     (error (e) (princ-to-string e)))))
    (let* ((stratum-api:*document* nil)
           (*documents* (make-hash-table :test 'equal))
           (doc (progn (stratum-api:use-document "parity") stratum-api:*document*))
           (before (copy-seq (the-object doc (render-cpu))))
           (layers (length (stratum-api:layers))))
      (call-with-stub-generator
       (lambda (generator seen)
         (call-with-stub-model
          '()
          (lambda (agent requests)
            (let ((*generator* generator) (*agent* agent))
              (let ((answer (prompt-document doc "a meadow"
                                             :forms (generation-forms (settings "source" "new" "name" "Meadow") "a meadow"))))
                (check "a generation runs with no model asked"
                       (and (getf answer :ok) (zerop (length (funcall requests)))
                            (= (length (stratum-api:layers)) (1+ layers))
                            (search "no planner" (getf answer :reply))
                            (search "generate-layer" (first (getf answer :forms))))
                       answer)
                (check "and the page can read the answer"
                       (eq (gethash "ok" (yason:parse (prompt-json answer))) t)))
              (stratum-api:undo)
              (check "it is one undo like a prompt"
                     (and (= (length (stratum-api:layers)) layers)
                          (equalp before (the-object doc (render-cpu)))))
              (stratum-api:generate-picture "in oils" :name "Oils")
              (let ((body (first (car (last (funcall seen))))))
                (check "the picture goes as a reference"
                       (and (= 1 (length (gethash "ref_images" body)))
                            (null (gethash "init_image" body)) (null (gethash "mask_image" body)))))
              (stratum-api:delete-layer "Oils")
              (stratum-api:generate-picture "in oils" :name "Oils" :reference nil :strength 0.3)
              (let ((body (first (car (last (funcall seen))))))
                (check "or as a starting point, with its strength and no mask"
                       (and (gethash "init_image" body) (null (gethash "mask_image" body))
                            (null (gethash "ref_images" body))
                            (< 0.29 (gethash "strength" body) 0.31))
                       (gethash "strength" body)))
              (check "the layer is the picture's size, on top"
                     (= (length (stratum-api:layers)) (1+ layers)))
              (stratum-api:delete-layer "Oils")
              ;; the page is given the picture, not left to fetch the layer
              (let* ((answer (prompt-document doc "a meadow"
                                              :forms (generation-forms (settings "source" "new") "a meadow")))
                     (stretched (getf answer :stretched))
                     (layer (car (last (the-object doc raster-layers)))))
                (check "a generation answers its picture, at the generator's size"
                       (and (= 1 (length stretched))
                            (eql (getf (first stretched) :stamp) (the-object layer pixel-stamp))
                            (<= (max (getf (first stretched) :w) (getf (first stretched) :h)) 256)
                            (png-octets-p (getf (first stretched) :png)))
                       stretched)
                (check "and the page hears it"
                       (= 1 (length (gethash "stretched" (yason:parse (prompt-json answer))))))
                (stratum-api:undo))
              ;; a layer painted on after it is no picture any more
              (let ((answer (prompt-document doc "a meadow"
                                             :forms (format nil "~a (select-rect 10 10 20 20) (fill-selection \"Generated\" (1 2 3)) (select-none)"
                                                            (generation-forms (settings "source" "new") "a meadow")))))
                (check "a layer painted on after is not sent as the picture"
                       (and (getf answer :ok) (null (getf answer :stretched)))
                       answer)
                (stratum-api:undo))))))))))

(defun test-generate-references ()
  "A model that cannot be shown a picture: what the generator is sent,
and what the person hears.  The replies have sd-server's own shape."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "parity") stratum-api:*document*)))
    (flet ((capabilities (name references)
             (format nil "{\"model\":{\"name\":~s,\"path\":\"\",\"stem\":\"\"},~
                         \"generation_features\":{\"ref_images\":~:[false~;true~],\"img2img\":~:[false~;true~]}}"
                     name references (plusp (length name))))
           (with-stub (capabilities fn)
             (call-with-stub-generator
              (lambda (generator seen)
                (let ((*generator* generator))
                  (funcall fn generator (lambda () (first (car (last (funcall seen))))))))
              :capabilities capabilities)))
      (with-stub (capabilities "sdxl.safetensors" nil)
        (lambda (generator last-body)
          (check "a model with no reference path says so" (eq (generator-references generator) :no))
          (check "and the planner is told" (search "cannot be shown a picture" (generator-state)))
          (let ((info (stratum-api:generate-picture "in oils" :name "Oils" :strength 0.4))
                (body (funcall last-body)))
            (check "such a model starts from the picture, not from the words alone"
                   (and (gethash "init_image" body) (null (gethash "ref_images" body))
                        (< 0.39 (gethash "strength" body) 0.41)))
            (check "and the answer says which happened"
                   (search "started from the picture" (getf info :note)) info))
          (stratum-api:delete-layer "Oils")
          (let ((answer (prompt-document doc "in oils"
                                         :forms "(generate-picture \"in oils\" :name \"Oils\")")))
            (check "the person who asked hears it too"
                   (and (getf answer :ok) (search "started from the picture" (getf answer :reply)))
                   answer))
          (stratum-api:undo)
          (let ((info (stratum-api:generate-picture "in oils" :name "Oils" :reference nil)))
            (check "a starting point asked for needs no word" (null (getf info :note)) info))
          (stratum-api:delete-layer "Oils")))
      (with-stub (capabilities "klein.gguf" t)
        (lambda (generator last-body)
          (check "a model with a reference path says so" (eq (generator-references generator) :yes))
          (let ((info (stratum-api:generate-picture "in oils" :name "Oils"))
                (body (funcall last-body)))
            (check "and is shown the picture, with nothing to add"
                   (and (gethash "ref_images" body) (null (gethash "init_image" body))
                        (null (getf info :note)))
                   info))
          (stratum-api:delete-layer "Oils")))
      (dolist (case (list (capabilities "" nil) nil))
        (with-stub case
          (lambda (generator last-body)
            (check "nothing loaded, or no answer, is not a model with no reference path"
                   (null (generator-references generator)))
            (let ((info (stratum-api:generate-picture "in oils" :name "Oils"))
                  (body (funcall last-body)))
              (check "it goes as asked, and says it could not tell"
                     (and (gethash "ref_images" body) (search "did not say" (getf info :note)))
                     info))
            (stratum-api:delete-layer "Oils")))))))

(defun test-generate-turn ()
  "A generator shared with something else: its turn runs around the job
alone, with no lock of ours held."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "parity") stratum-api:*document*))
         (turns 0) (free nil) (inside 0))
    (declare (ignorable doc))
    (check "a generator takes its turn at once unless told otherwise"
           (eq (generator-turn (make-generator "http://localhost" 4 1 0.9d0 256 "" 20 nil)) #'funcall))
    (let ((*generator* nil))
      (let ((turn (lambda (thunk) (funcall thunk))))
        (check "configure-generator! keeps the turn it is given"
               (eq turn (generator-turn (configure-generator! :url "http://localhost" :turn turn))))))
    (call-with-stub-generator
     (lambda (generator seen)
       (setf (generator-turn generator)
             (lambda (thunk)
               (incf turns)
               (setf free (session-idle? *shared-session*))
               (funcall thunk)
               (setf inside (length (funcall seen)))
               :a-turn-answers-nothing-we-need))
       (let ((*generator* generator))
         (stratum-api:generate-layer "a meadow" :name "Meadow")
         (check "the job runs inside the turn, once"
                (and (= turns 1) (= inside 1) (= 1 (length (funcall seen))))
                (list turns inside))
         (check "no lock of ours is held in it" free)
         (check "and the picture comes back whatever the turn answers"
                (find "Meadow" (stratum-api:layers)
                      :key (lambda (l) (getf l :name)) :test #'equal))
         (stratum-api:delete-layer "Meadow")
         (setf (generator-turn generator) (lambda (thunk) (declare (ignore thunk)) nil))
         (check "a turn that never comes is an error in words"
                (handler-case (progn (stratum-api:generate-layer "a meadow") nil)
                  (error (e) (search "turn never came" (princ-to-string e))))))))))

(defun test-fill-fade ()
  "A fill's fade follows the generator's pixels, not how far the
selection reaches: a long narrow selection is filled nearly to its edge."
  (let* ((w 1700) (h 400) (coverage (make-gray w h)))
    (loop for y from 100 below 300
          do (loop for x from 100 below 1600
                   do (setf (aref coverage (+ x (* y w))) 255)))
    (let* ((faded (faded-coverage coverage w h 1024))
           (coarse (faded-coverage coverage w h 256)))
      (flet ((alpha (bytes x y) (aref bytes (+ x (* y w)))))
        (check "a wide selection is whole a few pixels in from its edge"
               (>= (alpha faded 115 200) 250) (alpha faded 115 200))
        (check "and still soft at the edge itself"
               (and (< (alpha faded 100 200) 64)
                    (< (alpha faded 100 200) (alpha faded 105 200) (alpha faded 115 200)))
               (list (alpha faded 100 200) (alpha faded 105 200)))
        (check "a generator's coarser pixels widen the fade"
               (< (alpha coarse 115 200) (alpha faded 115 200))
               (alpha coarse 115 200))
        (check "and nothing outside the selection is touched"
               (and (zerop (alpha faded 99 200))
                    (every #'<= faded coverage)))))))

(defun test-first-load ()
  "What the first load of the default document leans on: a generated
layer is packed once, and a person's own pixels never are kept."
  (clear-pixel-cache)
  (let* ((pixels (generate-pixels '(:solid :color (9 8 7) :alpha 255) 64 64))
         (own (copy-seq pixels))
         (once (packed-once pixels 4)))
    (check "a generated layer's packing is made once, and is the same bytes"
           (and (eq once (packed-once pixels 4)) (equalp once (pack-pixels pixels 4))))
    (check "asked by another step it is packed by that step"
           (equalp (packed-once pixels 0) (pack-pixels pixels 0)))
    (check "an array of a person's own is packed afresh each time"
           (let ((one (packed-once own 4)))
             (and (equalp one (pack-pixels own 4)) (not (eq one (packed-once own 4))))))
    (clear-pixel-cache)
    (check "clearing the cache forgets the packings"
           (zerop (hash-table-count *packed-pixels*))))
  (warm-samples!)
  (let ((doc (make-sample-document "demo")))
    (check "warming leaves every layer and mask of the default document packed"
           (and (every (lambda (l) (gethash (the-object l pixels) *packed-pixels*))
                       (the-object doc raster-layers))
                (every (lambda (l) (gethash (the-object l mask-pixels) *packed-pixels*))
                       (the-object doc masked-layers))))))

(defun with-exif (jpeg orientation &key big-endian)
  "JPEG, a file's octets, with an EXIF segment saying ORIENTATION."
  (flet ((u16 (v) (if big-endian (list (ash v -8) (logand v 255)) (list (logand v 255) (ash v -8))))
         (u32 (v) (let ((bytes (loop for s in '(0 8 16 24) collect (logand (ash v (- s)) 255))))
                    (if big-endian (reverse bytes) bytes))))
    (let ((tiff (append (if big-endian '(77 77) '(73 73)) (u16 42) (u32 8)
                        (u16 1)
                        (u16 #x112) (u16 3) (u32 1) (u16 orientation) (u16 0)
                        (u32 0))))
      (concatenate '(vector (unsigned-byte 8))
                   (subseq jpeg 0 2)
                   (list #xff #xe1 0 (+ 8 (length tiff)) 69 120 105 102 0 0)
                   tiff
                   (subseq jpeg 2)))))

(defun test-jpeg ()
  "A JPEG read here: its orientation, its turn, the hash the page checks
it by, and the bytes Chromium decoded from the fixture when the server
has a decoder."
  (let* ((path (merge-pathnames "fixture.jpg" *tests-dir*))
         (jpeg (coerce (alexandria:read-file-into-byte-vector path) '(simple-array (unsigned-byte 8) (*)))))
    (check "a JPEG is told by its first bytes"
           (and (jpeg-octets-p jpeg) (not (jpeg-octets-p (subseq jpeg 1)))))
    (check "no EXIF is orientation 1, and so is rubbish"
           (and (= (jpeg-orientation jpeg) 1)
                (= (jpeg-orientation (subseq jpeg 0 4)) 1)
                (= (jpeg-orientation (subseq (with-exif jpeg 6) 0 26)) 1)
                (= (jpeg-orientation (with-exif jpeg 9)) 1)))
    (check "the orientation is read in either byte order"
           (loop for o from 1 to 8
                 always (and (= (jpeg-orientation (with-exif jpeg o)) o)
                             (= (jpeg-orientation (with-exif jpeg o :big-endian t)) o))))
    (let* ((w 3) (h 2) (source (make-rgba w h)))
      (dotimes (i (* w h)) (setf (aref source (* 4 i)) (1+ i)))
      (flet ((at (x y) (aref source (* 4 (+ x (* y w))))))
        (loop for (o shown) in `((1 ,(lambda (x y) (at x y)))
                                 (2 ,(lambda (x y) (at (- w 1 x) y)))
                                 (3 ,(lambda (x y) (at (- w 1 x) (- h 1 y))))
                                 (4 ,(lambda (x y) (at x (- h 1 y))))
                                 (5 ,(lambda (x y) (at y x)))
                                 (6 ,(lambda (x y) (at y (- h 1 x))))
                                 (7 ,(lambda (x y) (at (- w 1 y) (- h 1 x))))
                                 (8 ,(lambda (x y) (at (- w 1 y) x))))
              do (multiple-value-bind (pixels nw nh) (oriented-rgba source w h o)
                   (check (format nil "orientation ~d turns the picture as EXIF says" o)
                          (and (= nw (if (> o 4) h w)) (= nh (if (> o 4) w h))
                               (loop for y below nh
                                     always (loop for x below nw
                                                  always (= (aref pixels (* 4 (+ x (* y nw))))
                                                            (funcall shown x y))))))))))
    (check "the hash is the page's"
           (= (pixels-hash (coerce #(1 2 3 4 5 6 7 8) '(simple-array (unsigned-byte 8) (*)))) 2746375339))
    (when (turbojpeg?)
      (multiple-value-bind (pixels w h) (decode-jpeg jpeg)
        (check "a JPEG decodes to the bytes Chromium decodes"
               (and (= w 61) (= h 47) (= (pixels-hash pixels) 689382815))
               (list w h (pixels-hash pixels))))
      (check "a size may be refused before any pixels are made"
             (search "too big"
                     (handler-case (decode-jpeg jpeg (lambda (w h) (declare (ignore w h)) (error "too big")))
                       (error (e) (princ-to-string e)))))
      (dolist (bad (list (subseq jpeg 0 40)
                         (concatenate '(simple-array (unsigned-byte 8) (*)) (subseq jpeg 0 3) (make-array 200 :element-type '(unsigned-byte 8) :initial-element 7))))
        (check "a file that is no JPEG is an error in words"
               (search "could not be read"
                       (handler-case (progn (decode-jpeg bad) "")
                         (error (e) (princ-to-string e)))))))))

(defun test-gradient ()
  (flet ((gray (pixels x &optional (y 0) (w 10)) (aref pixels (* 4 (+ x (* y w)))))
         (alpha (pixels x &optional (y 0) (w 10)) (aref pixels (+ 3 (* 4 (+ x (* y w)))))))
    (let ((strip (make-rgba 10 1)))
      (check "a linear ramp from black to white climbs on the encoded scale and is opaque"
             (let ((g (gradient-rgba strip 10 1 0 0.5 10 0.5 '(0 0 0 255) '(255 255 255 255) :linear 1)))
               (and (= (gray g 0) 13) (= (gray g 9) 242) (= (gray g 4) 115) (= (gray g 5) 140)
                    (loop for x from 1 below 10 always (> (gray g x) (gray g (1- x))))
                    (loop for x below 10 always (= 255 (alpha g x)))))
             (coerce (gradient-rgba strip 10 1 0 0.5 10 0.5 '(0 0 0 255) '(255 255 255 255) :linear 1) 'list))
      (check "a linear ramp saturates beyond its ends"
             (let ((g (gradient-rgba strip 10 1 3 0.5 7 0.5 '(0 0 0 255) '(255 255 255 255) :linear 1)))
               (and (= 0 (gray g 0) (gray g 2)) (= 255 (gray g 7) (gray g 9)))))
      (check "a reflected ramp mirrors about its start"
             (let ((g (gradient-rgba strip 10 1 5 0.5 10 0.5 '(0 0 0 255) '(255 255 255 255) :reflected 1)))
               (and (= (gray g 4) (gray g 5)) (= (gray g 0) (gray g 9)) (< (gray g 5) (gray g 9)))))
      (check "a ramp to transparency fades out, and over white at half opacity a black ramp is linear mid-gray"
             (let ((fade (gradient-rgba strip 10 1 0 0.5 10 0.5 '(255 0 0 255) '(255 0 0 0) :linear 1))
                   (white (let ((p (make-rgba 10 1))) (fill p 255) p)))
               (and (= 242 (alpha fade 0)) (= 13 (alpha fade 9)) (> (alpha fade 0) (alpha fade 5)) (> (alpha fade 5) (alpha fade 9))
                    (loop for x below 10 always (= 255 (gray fade x)))
                    (= 188 (gray (gradient-rgba white 10 1 0 0.5 10 0.5 '(0 0 0 255) '(0 0 0 255) :linear 0.5) 5)))))
      (check "a drag of no length paints the start color"
             (let ((g (gradient-rgba strip 10 1 4 0.5 4 0.5 '(30 60 90 255) '(255 255 255 255) :radial 1)))
               (loop for x below 10 always (and (= 30 (gray g x)) (= 255 (alpha g x)))))))
    (let ((square (make-rgba 9 9)))
      (check "a radial ramp is symmetric about its center and white one length out"
             (let ((g (gradient-rgba square 9 9 4.5 4.5 8.5 4.5 '(0 0 0 255) '(255 255 255 255) :radial 1)))
               (and (= 0 (gray g 4 4 9)) (= 255 (gray g 0 4 9) (gray g 8 4 9) (gray g 4 0 9) (gray g 4 8 9))
                    (= (gray g 2 4 9) (gray g 6 4 9) (gray g 4 2 9) (gray g 4 6 9)) (< 0 (gray g 2 4 9) 255))))
      (check "a diamond ramp reaches the end color along both axes and the diagonal alike"
             (let ((g (gradient-rgba square 9 9 4.5 4.5 8.5 4.5 '(0 0 0 255) '(255 255 255 255) :diamond 1)))
               (and (= 0 (gray g 4 4 9)) (= 255 (gray g 0 4 9) (gray g 4 0 9) (gray g 8 8 9) (gray g 0 0 9))
                    (= (gray g 6 6 9) (gray g 6 4 9)) (<= 127 (gray g 6 6 9) 128))))
      (check "an angle ramp turns once around the start, a quarter turn a quarter of the way"
             (let ((g (gradient-rgba square 9 9 4.5 4.5 8.5 4.5 '(0 0 0 255) '(255 255 255 255) :angle 1)))
               (and (= 64 (gray g 4 8 9)) (<= 127 (gray g 0 4 9) 128) (= 191 (gray g 4 0 9))
                    (< (gray g 8 5 9) 20))))))
  (let* ((doc (make-sample-document "parity"))
         (sky (the-object doc (layer 0)))
         (w 320)
         (before (copy-seq (the-object sky pixels))))
    (demand doc)
    (check "a gradient over a layer changes it and rebuilds nothing"
           (progn (gradient-layer! sky 0 100 320 100 :start '(200 30 30 255) :end '(20 40 220 255))
                  (let ((after (the-object sky pixels)))
                    (and (not (equalp before after))
                         (= 200 (aref after (* 4 (+ 0 (* w 100)))))
                         (= 220 (aref after (+ 2 (* 4 (+ 319 (* w 100))))))
                         (equal (demand doc) '(1 1))))))
    (check "undo gives the layer back after a gradient"
           (and (undo-stroke! doc) (equalp before (the-object sky pixels))))
    (check "under a selection a gradient changes the selected pixels only"
           (progn (select-rect! doc 100 60 60 40)
                  (gradient-layer! sky 130 80 130 60 :shape :radial)
                  (let ((after (the-object sky pixels)))
                    (and (not (equalp before after))
                         (loop for i below (length before)
                               for px = (mod (floor i 4) w) for py = (floor (floor i 4) w)
                               always (or (and (<= 100 px 159) (<= 60 py 99))
                                          (= (aref before i) (aref after i))))))))
    (check "a gradient refuses a shape it does not know"
           (handler-case (progn (gradient-layer! sky 0 0 10 10 :shape :spiral) nil)
             (error () t)))))

(defun max-byte-diff (a b)
  (loop for x across a for y across b maximize (abs (- x y))))

(defun test-merge ()
  ;; A merge rounds the merged pair to 8 bits once, where the stack kept
  ;; doubles; through the curves and levels above, that one rounding is
  ;; up to two levels in the picture, and a second merge up to three.
  (let* ((doc (make-sample-document "parity"))
         (render (the-object doc (render-cpu)))
         (names (layer-names doc)))
    (check "Merge Down folds a screened layer into the opaque one below, within two levels"
           (and (= 0 (merge-down! doc 1))
                (equal (layer-names doc) (cons "Sky" (cddr names)))
                (eql (the-object doc (layer 0) kind) :raster)
                (<= (max-byte-diff (the-object doc (render-cpu)) render) 2))
           (list (layer-names doc) (max-byte-diff (the-object doc (render-cpu)) render)))
    (check "Merge Down applies an adjustment layer to the raster below it, within three levels"
           (and (= 0 (merge-down! doc 1))
                (string= (the-object doc (layer 0) layer-name) "Sky")
                (= (length (the-object doc layer-list)) (- (length names) 2))
                (<= (max-byte-diff (the-object doc (render-cpu)) render) 3))
           (max-byte-diff (the-object doc (render-cpu)) render))
    (check "Merge Down refuses the bottom layer and a non-raster below"
           (and (handler-case (progn (merge-down! doc 0) nil) (error () t))
                (let ((j (position :levels (mapcar (lambda (l) (the-object l kind)) (the-object doc layer-list)))))
                  (handler-case (progn (merge-down! doc (1+ j)) nil) (error () t)))))
    (check "a merge forgets the undo list"
           (progn (apply-brush-stroke (the-object doc (layer 0)) *test-stroke*)
                  (merge-down! doc 1)
                  (null (the-object doc undo-entries))))
    (check "Flatten renders exactly as the stack did, into one Background"
           (let ((before (the-object doc (render-cpu))))
             (and (= 0 (flatten! doc))
                  (equal (layer-names doc) '("Background"))
                  (equalp (the-object doc (render-cpu)) before))))))

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

(defun test-sessions ()
  (let ((a (session-table "test-a")) (b (session-table "test-b"))
        (shared-parity (find-document "parity")))
    (check "each session has its own documents; no session is the shared pool"
           (and (not (eq a b)) (eq a (session-table "test-a"))
                (eq *documents* (session-table "")) (eq *documents* (session-table nil))
                (let ((*documents* a))
                  (find-document "parity")
                  (and (gethash "parity" a) (not (gethash "parity" b))
                       (not (eq (gethash "parity" a) shared-parity))))))
    (let* ((one (session-for "test-a")) (two (session-for "test-b"))
           (doc-one (gethash "parity" a))
           (doc-two (let ((*documents* b)) (find-document "parity")))
           (layers (lambda (doc) (length (the-object doc layer-list)))))
      (flet ((run-job (session name doc forms)
               (let ((id (start-prompt-job! session doc "paste" name nil forms)))
                 (loop for status = (gethash "status" (yason:parse (prompt-job-json id)))
                       for tries from 1
                       while (and (equal status "working") (< tries 2400))
                       do (sleep 0.05)
                       finally (return status)))))
        (let ((*session* one) (*documents* a))
          (select-rect! doc-one 10 10 4 4)
          (copy-selection! (the-object doc-one (layer 0))))
        (check "a clipboard is its session's: what one tester copied another cannot paste"
               (and (sess-clipboard one)
                    (not (eq (sess-clipboard one) (clipboard)))
                    (let ((*session* two) (*documents* b))
                      (and (null (clipboard))
                           (handler-case (progn (paste-clipboard! doc-two) nil)
                             (error () t))))))
        (check "a prompt job pastes from its own session's clipboard and no other"
               (let ((before-one (funcall layers doc-one)) (before-two (funcall layers doc-two)))
                 (and (equal "done" (run-job one "test-a" doc-one "(paste)"))
                      (equal "done" (run-job two "test-b" doc-two "(paste)"))
                      (= (1+ before-one) (funcall layers doc-one))
                      (= before-two (funcall layers doc-two)))))))
    (check "an idle session is swept, and comes back empty"
           (progn (setf (sess-touched (gethash "test-a" *sessions*)) (- (get-universal-time) (* 2 *session-idle-seconds*)))
                  (sweep-sessions!)
                  (and (null (gethash "test-a" *sessions*))
                       (gethash "test-b" *sessions*)
                       (null (gethash "parity" (session-table "test-a"))))))
    (remhash "test-a" *sessions*)
    (remhash "test-b" *sessions*)))

(defun test-session-locks ()
  "Sessions run in parallel behind real doors, a session's own requests
stay in order, and what they share stands two threads at once."
  (let* ((port (free-port))
         (server (net.aserve:start :port port :server :new :listeners 4 :host "127.0.0.1"))
         (nothing (make-array 0 :element-type '(unsigned-byte 8)))
         (old-gate *gate*)
         (held nil) (release nil) (followed nil))
    (flet ((post (path) (raw-post port path nothing "application/octet-stream"))
           (await (test)
             (loop repeat 400 until (funcall test) do (sleep 0.05))
             (funcall test))
           (at-once (count fn)
             ;; FN of an index in COUNT threads; their answers, or the
             ;; words of what stopped them
             (let ((answers (make-array count :initial-element nil)))
               (mapc #'bt:join-thread
                     (loop for i below count
                           collect (let ((i i))
                                     (bt:make-thread
                                      (lambda ()
                                        (setf (aref answers i)
                                              (handler-case (funcall fn i)
                                                (serious-condition (e) (princ-to-string e)))))
                                      :name "stratum test"))))
               (coerce answers 'list))))
      (unwind-protect
           (progn
             (setf *gate* (invite-gate:make-gate :name "Stratum" :prefix "/stratum" :cookie "stratum_invite"))
             (publish-door server nil "/hold" "application/json"
                           (lambda (req) (declare (ignore req))
                             (setf held *session*)
                             (loop repeat 400 until release do (sleep 0.05))
                             "{}"))
             (publish-door server nil "/quick" "application/json"
                           (lambda (req) (declare (ignore req)) "{}"))
             (let ((holder (bt:make-thread (lambda () (post "/hold?s=locks-a")) :name "stratum test"))
                   (follower nil))
               (await (lambda () held))
               (check "a door runs on its session's lock, not a shared one"
                      (and held (eq held (gethash "locks-a" *sessions*))
                           (not (session-idle? held)) (session-idle? *shared-session*)))
               (check "one session's long request does not stop another's"
                      (eql (first (post "/quick?s=locks-b")) 200))
               (setf follower (bt:make-thread (lambda () (post "/quick?s=locks-a") (setf followed t))
                                              :name "stratum test"))
               (sleep 0.3)
               (check "the session's own next request waits its turn" (not followed))
               (setf release t)
               (bt:join-thread holder)
               (bt:join-thread follower)
               (check "and is served once the first is done" followed))
             (let* ((session (session-for "locks-c"))
                    (doc (with-session (session) (find-document "parity")))
                    (id (bt:with-lock-held (*lock*)
                          (start-prompt-job! session doc "paste" "locks-c" nil "(paste)"))))
               (check "a prompt job runs on its session's lock, not the shared one"
                      (bt:with-lock-held (*lock*)
                        (await (lambda ()
                                 (equal "done" (gethash "status" (yason:parse (prompt-job-json id)))))))))
             (let* ((spec '(:solid :color (10 20 30)))
                    (answers (progn (clear-pixel-cache)
                                    (at-once 8 (lambda (i)
                                                 (dotimes (n 200) (generate-pixels spec (+ 8 n) (+ 8 i)))
                                                 (generate-pixels spec 64 64))))))
               (check "threads asking the pixel cache at once all hold the one copy"
                      (and (typep (first answers) '(simple-array (unsigned-byte 8) (*)))
                           (every (lambda (a) (eq a (first answers))) answers))
                      (remove-if-not #'stringp answers)))
             (let ((stamps (at-once 8 (lambda (i)
                                        (declare (ignore i))
                                        (loop repeat 5000 collect (next-content-stamp))))))
               (check "content stamps stay unique across threads"
                      (let ((seen (make-hash-table)))
                        (dolist (some stamps) (dolist (stamp some) (setf (gethash stamp seen) t)))
                        (= (hash-table-count seen) 40000))))
             (let* ((one (render-checksum "parity+stroke"))
                    (answers (at-once 4 (lambda (i)
                                          (declare (ignore i))
                                          (with-session ((%make-session))
                                            (render-checksum "parity+stroke"))))))
               (check "four sessions making, painting and rendering at once get what one gets"
                      (every (lambda (a) (eql a one)) answers)
                      answers)))
        (setf release t *gate* old-gate)
        (dolist (id '("locks-a" "locks-b" "locks-c")) (remhash id *sessions*))
        (net.aserve:shutdown :server server))))
  ;; relief, where the Lisp can say how full the heap is
  (when (heap-used-fraction)
    (let* ((*sessions* (make-hash-table :test 'equal))
           (*shared-session* (%make-session))
           (*heap-high-water* 0.0)
           (idle (%make-session)) (busy (%make-session)) (mine (%make-session))
           (stop nil))
      (flet ((doc-in (session id)
               (setf (gethash id *sessions*) session)
               (let ((doc (make-sample-document "parity")))
                 (the-object doc (set-slot! :undo-entries (list :mark)))
                 (setf (gethash "parity" (sess-table session)) doc)))
             (relieve (keep)
               (bt:with-lock-held (*sessions-lock*) (relieve-memory! keep))))
        (let* ((idle-doc (doc-in idle "idle"))
               (busy-doc (doc-in busy "busy"))
               (mine-doc (doc-in mine "mine"))
               (worker (bt:make-thread (lambda ()
                                         (bt:with-lock-held ((sess-lock busy))
                                           (loop repeat 2000 until stop do (sleep 0.01))))
                                       :name "stratum test"))
               (done (progn (loop repeat 400 while (session-idle? busy) do (sleep 0.01))
                            (relieve mine))))
          (check "relief drops an idle session's history and forgets it"
                 (and (null (the-object idle-doc undo-entries)) (null (gethash "idle" *sessions*))))
          (check "relief passes over a session at work, without waiting for it"
                 (and (the-object busy-doc undo-entries) (eq busy (gethash "busy" *sessions*))))
          (check "the session asking comes last: kept, and its history goes only when nothing else is left"
                 (and (eq mine (gethash "mine" *sessions*))
                      (null (the-object mine-doc undo-entries))
                      (eql (car (last done)) :own-undo-dropped))
                 done)
          (relieve busy)
          (check "a session at work keeps its history even when it is the one asking"
                 (the-object busy-doc undo-entries))
          (setf stop t)
          (bt:join-thread worker))))))

(defun test-threads ()
  (let* ((doc (make-sample-document "parity"))
         (threaded (let ((*render-threads* 8)) (the-object doc (render-cpu))))
         (single (let ((*render-threads* 1)) (the-object doc (render-cpu)))))
    (check "a threaded render equals a single-threaded one" (equalp threaded single))))

(defun test-invite ()
  "The private beta's door: invites minted, found, revoked and reported;
no file, no gate."
  (let ((stratum::*gate* (invite-gate:make-gate :name "Stratum" :prefix "/stratum" :cookie "stratum_invite")))
    (check "no invites file: no gate" (not (stratum::invite-gate?)))
    (check "no invites file: nothing found" (null (stratum::find-invite "abc"))))
  (let* ((file (format nil "/tmp/stratum-invites-test-~d.sexp" (get-universal-time)))
         (stratum::*gate* (invite-gate:make-gate :name "Stratum" :prefix "/stratum" :cookie "stratum_invite"
                                                 :invites-file file :public-base "https://example.test/")))
    (unwind-protect
         (multiple-value-bind (token url) (stratum::mint-invite! "Ada")
           (check "token: 20 characters, letters and digits"
                  (and (= (length token) 20) (every #'alphanumericp token)))
           (check "tokens differ" (string/= token (invite-gate:random-token)))
           (check "the invite link" (string= url (format nil "https://example.test/stratum/invite/~a" token)) url)
           (check "the token is found, with its name"
                  (equal (second (stratum::find-invite token)) "Ada"))
           (check "an unknown token is not" (null (stratum::find-invite "nobody")))
           (check "the gate is on" (stratum::invite-gate?))
           (check "(stratum:gate) is the gate invite-gate's functions take"
                  (and (eq (stratum:gate) stratum::*gate*)
                       (equal (second (invite-gate:find-invite (stratum:gate) token)) "Ada")))
           (let ((second-token (stratum::mint-invite! "Grace")))
             (check "two invites in the report, unused"
                    (equal (mapcar (lambda (r) (list (getf r :name) (getf r :uses))) (stratum::invites-report))
                           '(("Ada" 0) ("Grace" 0))))
             (check "revoked: gone, the other kept"
                    (and (stratum::revoke-invite! token)
                         (null (stratum::find-invite token))
                         (stratum::find-invite second-token)))
             (check "revoking twice does nothing" (not (stratum::revoke-invite! token)))))
      (when (probe-file file) (delete-file file)))))

(defun test-noise (count &optional (seed 20261104))
  "COUNT bytes of noise, the same on every run and every implementation
(a linear congruential generator, not RANDOM, whose states differ)."
  (let ((v (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count v)
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref v i) (ldb (byte 8 16) seed)))))

(defun reference-unsharp (pixels w h radius amount threshold)
  "Unsharp Mask as it was before it went a plane at a time, every
channel at once: what unsharp-mask-rgba must equal to the last bit."
  (let* ((premul (decode-premul pixels w h))
         (blur (gaussian-blur-premul premul w h (max 0.1d0 (float radius 1d0))))
         (out (make-array (* 4 w h) :element-type 'single-float))
         (amount (float amount 1d0))
         (limit (/ (float threshold 1d0) 255d0)))
    (dotimes (i (* w h))
      (let* ((o (* 4 i))
             (dr (- (aref premul o) (aref blur o)))
             (dg (- (aref premul (+ o 1)) (aref blur (+ o 1))))
             (db (- (aref premul (+ o 2)) (aref blur (+ o 2))))
             (on (> (max (abs dr) (abs dg) (abs db)) limit)))
        (setf (aref out o) (if on (float (max 0d0 (+ (aref premul o) (* amount dr))) 1f0) (aref premul o))
              (aref out (+ o 1)) (if on (float (max 0d0 (+ (aref premul (+ o 1)) (* amount dg))) 1f0) (aref premul (+ o 1)))
              (aref out (+ o 2)) (if on (float (max 0d0 (+ (aref premul (+ o 2)) (* amount db))) 1f0) (aref premul (+ o 2)))
              (aref out (+ o 3)) (aref premul (+ o 3)))))
    (encode-premul out w h)))

(defun test-planes ()
  "The filters a plane at a time: the same numbers as every channel at
once, the reduced feather close to the exact one, and a form that runs
out of memory failing alone."
  (let* ((w 97) (h 61)
         (pixels (let ((v (test-noise (* 4 w h))))
                   ;; some empty and nearly empty pixels among them
                   (loop for i from 3 below (length v) by 4
                         when (< (aref v i) 80) do (setf (aref v i) (mod (aref v i) 2)))
                   v)))
    (check "blur a plane at a time equals every channel at once"
           (equalp (gaussian-blur-rgba pixels w h 2.5d0)
                   (encode-premul (gaussian-blur-premul (decode-premul pixels w h) w h 2.5d0) w h)))
    (check "unsharp a plane at a time equals every channel at once"
           (equalp (unsharp-mask-rgba pixels w h 2 1.5d0 0)
                   (reference-unsharp pixels w h 2 1.5d0 0)))
    (check "unsharp with a threshold, the same"
           (equalp (unsharp-mask-rgba pixels w h 3 0.8d0 12)
                   (reference-unsharp pixels w h 3 0.8d0 12))))
  (let* ((w 420) (h 300)
         (mask (rasterize-rect w h 90 70 240 160))
         (exact (feather-mask-exact mask w h 40d0))
         (reduced (feather-mask-reduced mask w h 40d0))
         (worst (loop for i below (* w h)
                      maximize (abs (- (aref exact i) (aref reduced i))))))
    (check "the reduced feather stays within two levels of the exact one" (<= worst 2) worst))
  (let ((ran (make-array 8 :initial-element nil)))
    (check "a render's slices hand a failure back, once they have all stopped"
           (handler-case
               (progn (run-kernel (lambda (start end)
                                    (setf (aref ran (floor start 65536)) t)
                                    (when (zerop start) (error "The kernel failed."))
                                    end)
                                  (* 8 65536))
                      nil)
             (error (e) (and (search "kernel failed" (princ-to-string e)) (every #'identity ran))))
           ran))
  (let ((*render-threads* 1))
    (check "one slice fails in the calling thread as it always did"
           (handler-case (progn (parallel-ranges 10 (lambda (s e) (declare (ignore s e)) (error "Alone."))) nil)
             (error (e) (search "Alone." (princ-to-string e))))))
  (check "the undo list's limit is a sixteenth of the heap, and 128 MB at the least"
         (and (>= *undo-bytes* (* 128 1024 1024))
              #+sbcl (>= *undo-bytes* (floor (sb-ext:dynamic-space-size) 16)))
         *undo-bytes*)
  ;; the blur's fast loops against the plain one, at every shape where
  ;; a row or a column is shorter than the kernel, the four-wide loop
  ;; has a remainder, or a block of columns ends part way
  (let ((*blur-block* 7))
    (dolist (shape '((64 40) (1 1) (1 30) (30 1) (2 2) (3 50) (5 5) (9 13) (31 4) (33 33) (70 6)))
      (destructuring-bind (w h) shape
        (dolist (sigma '(0.1d0 0.7d0 1d0 2.5d0 9d0))
          (let* ((noise (test-noise (* w h)))
                 (plane (map '(simple-array single-float (*)) (lambda (b) (/ (float b 1f0) 255f0)) noise))
                 (reference (plain-blur plane w h sigma))
                 (scratch (make-plane (* w h))))
            (check (format nil "the blur of ~a x ~a at ~a equals the plain loop to the last bit" w h sigma)
                   (equalp (blur-plane! (copy-seq plane) scratch w h sigma) reference)))))))
  (let* ((w 1100) (h 5)
         (plane (map '(simple-array single-float (*)) (lambda (b) (/ (float b 1f0) 255f0)) (test-noise (* w h)))))
    (check "the blur across the real block's edge equals the plain loop"
           (equalp (blur-plane! (copy-seq plane) (make-plane (* w h)) w h 2d0) (plain-blur plane w h 2d0))))
  (let* ((w 40) (h 30) (scratch (make-plane (* w h))))
    (dolist (value '(1f0 0f0 0.37f0))
      (let ((plane (make-array (* w h) :element-type 'single-float :initial-element value)))
        (check (format nil "a plane of ~a alone equals the plain loop" value)
               (equalp (blur-plane! (copy-seq plane) scratch w h 3d0) (plain-blur plane w h 3d0))))))
  ;; the feather works on the selection's bounds, to the same bytes: in
  ;; the middle, and with the kernel's reach cut off by each edge
  (let ((w 300) (h 200))
    (dolist (rect '((120 80 40 30) (0 0 25 20) (270 170 30 30) (0 90 300 15) (1 1 298 198)))
      (dolist (sigma '(0.1d0 2d0 7.5d0))
        (destructuring-bind (x y rw rh) rect
          (let ((mask (rasterize-polygon w h (list (list x y) (list (+ x rw) (+ y 3))
                                                   (list (+ x rw -2) (+ y rh)) (list (+ x 4) (+ y rh -1))))))
            (check (format nil "a feather of ~a on its bounds ~a is the whole mask's, byte for byte" sigma rect)
                   (equalp (feather-mask-exact mask w h sigma)
                           (feather-mask-exact mask w h sigma (mask-bounds mask w h)))))))))
  (let* ((agent (sexp-agent::copy-agent *agent*))
         (operator (intern "TEST-RUNS-OUT" :stratum-api))
         (*agent* agent))
    (setf (symbol-function operator) (lambda () (error (make-condition 'storage-condition))))
    (setf (sexp-agent:agent-operators agent) (list* operator (sexp-agent:agent-operators agent)))
    (let* ((stratum-api:*document* nil)
           (*documents* (make-hash-table :test 'equal)))
      (stratum-api:use-document "parity")
      (multiple-value-bind (results ok) (run-forms "(selection) (test-runs-out) (selection)")
        (check "a form out of memory fails alone, the one before it kept"
               (and (not ok) (= (length results) 2)
                    (getf (first results) :ok)
                    (search "not enough memory" (getf (second results) :error)))
               results)))
    (fmakunbound operator)))

(defun test-stamps ()
  "Content stamps: equal stamps are equal bytes, across every change of
the stack, and a stamp never outlives its pixels."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal)))
    (stratum-api:use-document "parity")
    (flet ((stamps ()
             (mapcar (lambda (l)
                       (list (and (eql (the-object l kind) :raster) (the-object l pixel-stamp))
                             (the-object l mask-stamp)))
                     (the-object stratum-api:*document* layer-list)))
           (doc () stratum-api:*document*))
      (let ((start (stamps)))
        (check "every raster layer and every mask has a stamp of its own"
               (let ((all (remove nil (apply #'append start))))
                 (and (= (length all) (+ (length (the-object (doc) raster-layers))
                                         (length (the-object (doc) masked-layers))))
                      (= (length all) (length (remove-duplicates all)))))
               start)
        (stratum-api:add-layer :curves :name "Stamp test")
        (check "an adjustment layer added renews no stamp"
               (equal (butlast (stamps)) start) (stamps))
        (stratum-api:levels 0 :gamma 1.2)
        (let ((after (butlast (stamps))))
          (check "a filter renews its layer's stamp alone"
                 (and (/= (first (first after)) (first (first start)))
                      (equal (rest after) (rest start)))
                 after)
          (stratum-api:move-layer 0 1)
          (check "a move carries each stamp with its layer"
                 (and (equal (first (stamps)) (second after)) (equal (second (stamps)) (first after))))
          (stratum-api:move-layer 1 -1))
        (let ((painted (first (first (stamps)))))
          (stratum-api:duplicate-layer 0)
          (check "a painted layer's copy has pixels and a stamp of its own"
                 (and (= (first (first (stamps))) painted)
                      (/= (first (second (stamps))) painted)
                      (not (eq (the-object (doc) (layer 0) pixels) (the-object (doc) (layer 1) pixels)))
                      (equalp (the-object (doc) (layer 0) pixels) (the-object (doc) (layer 1) pixels)))))
        ;; a mask painted, then its layer copied: two buffers, two stamps
        (let ((masked (position-if (lambda (l) (the-object l has-mask?)) (the-object (doc) layer-list))))
          (apply-brush-stroke (the-object (doc) (layer masked))
                              (list :points '((40 40) (90 60)) :radius 12 :color '(0 0 0))
                              :target :mask)
          (stratum-api:duplicate-layer masked)
          (let ((a (the-object (doc) (layer masked))) (b (the-object (doc) (layer (1+ masked)))))
            (check "a painted mask's copy has a buffer and a stamp of its own"
                   (and (not (eq (the-object a mask-pixels) (the-object b mask-pixels)))
                        (equalp (the-object a mask-pixels) (the-object b mask-pixels))
                        (/= (the-object a mask-stamp) (the-object b mask-stamp))))))
        (check "a saved document carries no stamp"
               (notany (lambda (spec) (or (getf spec :pixel-stamp) (getf spec :mask-stamp)))
                       (materialized-specs (doc))))))))

(defun plain-blur (plane w h sigma)
  "PLANE blurred the plain way, rows then columns, each pixel's taps
clamped and added in order: what blur-plane!'s loops must equal."
  (multiple-value-bind (weights extent) (gaussian-kernel (float sigma 1d0))
    (let ((scratch (make-plane (* w h)))
          (out (make-plane (* w h))))
      (dotimes (y h)
        (dotimes (x w)
          (let ((v 0d0))
            (loop for i from (- extent) to extent
                  do (incf v (* (aref weights (+ i extent))
                                (aref plane (+ (max 0 (min (1- w) (+ x i))) (* y w))))))
            (setf (aref scratch (+ x (* y w))) (float v 1f0)))))
      (dotimes (x w out)
        (dotimes (y h)
          (let ((v 0d0))
            (loop for i from (- extent) to extent
                  do (incf v (* (aref weights (+ i extent))
                                (aref scratch (+ x (* (max 0 (min (1- h) (+ y i))) w))))))
            (setf (aref out (+ x (* y w))) (float v 1f0))))))))

(defun free-port ()
  "A port nothing listens on just now."
  (let* ((socket (usocket:socket-listen "127.0.0.1" 0))
         (port (usocket:get-local-port socket)))
    (usocket:socket-close socket)
    port))

(defun raw-exchange (port path head bytes)
  "Send HEAD, a string, and BYTES over a plain socket and read the
answer to the end: a string, one character a byte.  A connection reset
here, the reply lost to bytes left unread at the close, is a failure,
as it is for a person."
  (let* ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream socket)))
    (unwind-protect
         (progn
           (write-sequence (map '(vector (unsigned-byte 8)) #'char-code head) stream)
           (write-sequence bytes stream)
           (force-output stream)
           ;; a deadline, not usocket:wait-for-input: on CCL that answers
           ;; nothing with the first byte already there to read
           (handler-case
               (glisp:with-deadline (20)
                 (with-output-to-string (out)
                   (loop for byte = (read-byte stream nil nil)
                         while byte do (write-char (code-char byte) out))))
             (serious-condition (e) (error "No answer from ~a: ~a" path e))))
      (usocket:socket-close socket))))

(defun raw-post (port path bytes &optional (content-type "application/octet-stream"))
  "POST BYTES to PATH: (status text)."
  (let ((text (raw-exchange
               port path
               (format nil "POST ~a HTTP/1.1~c~cHost: 127.0.0.1~c~cConnection: close~c~cContent-Type: ~a~c~cContent-Length: ~d~c~c~c~c"
                       path #\Return #\Newline #\Return #\Newline #\Return #\Newline
                       content-type #\Return #\Newline
                       (length bytes) #\Return #\Newline #\Return #\Newline)
               bytes)))
    (list (parse-integer text :start 9 :end 12) text)))

(defun raw-get (port path)
  "GET PATH as an HTTP/1.0 client, whose answer comes in one piece:
(status body), the body as octets."
  (let* ((text (raw-exchange port path
                             (format nil "GET ~a HTTP/1.0~c~c~c~c" path #\Return #\Newline #\Return #\Newline)
                             #()))
         (blank (search (coerce '(#\Return #\Newline #\Return #\Newline) 'string) text)))
    (list (parse-integer text :start 9 :end 12)
          (map '(simple-array (unsigned-byte 8) (*)) #'char-code (subseq text (+ blank 4))))))

(defun unpack-pixels (packed)
  "What the page makes of pack-pixels' bytes (fetchPixels in viewer.js)."
  (let ((step (aref packed 0))
        (px (if (< (length packed) 2)
                (make-array 0 :element-type '(unsigned-byte 8))
                (coerce (chipz:decompress nil 'chipz:zlib (subseq packed 1))
                        '(simple-array (unsigned-byte 8) (*))))))
    (when (plusp step)
      (loop for i from step below (length px)
            do (setf (aref px i) (logand 255 (+ (aref px i) (aref px (- i step)))))))
    px))

(defun test-uploads ()
  "The doors over a real connection, on a server of their own: a body
comes through byte for byte as octets and no string; an upload over the
limit, or at any door over the small limit, is told why; a visitor with
no invite gets the gate's page with the body unread."
  (check "body-refusal: within the limit" (null (body-refusal 10 100)))
  (check "body-refusal: over it, in megabytes" (search "MB" (or (body-refusal 200 100) "")))
  (check "body-refusal: undeclared passes to the reader" (null (body-refusal nil 100)))
  (check "body-refusal: no heap on earth has room"
         (or (null (heap-free-bytes))
             (search "no room" (or (body-refusal (expt 10 12) (expt 10 13) :upload? t) ""))))
  ;; pixels packed for the way down, several bands of them
  (let ((noise (test-noise (* 4 31 17)))
        (flat (make-array 400000 :element-type '(unsigned-byte 8) :initial-element 9))
        (none (make-array 0 :element-type '(unsigned-byte 8)))
        (*pack-band-bytes* 100))
    (check "pack-pixels: differences four apart come back exactly"
           (equalp (unpack-pixels (pack-pixels noise 4)) noise))
    (check "pack-pixels: and the bytes as they are" (equalp (unpack-pixels (pack-pixels noise 0)) noise))
    (check "pack-pixels: nothing packs to nothing"
           (and (equalp (unpack-pixels (pack-pixels none 4)) none)
                (equalp (unpack-pixels (pack-pixels none 0)) none)))
    (check "pack-pixels: a flat layer is a hundredth of its bytes"
           (and (< (length (pack-pixels flat 4)) 4000) (equalp (unpack-pixels (pack-pixels flat 4)) flat))
           (length (pack-pixels flat 4))))
  (let* ((port (free-port))
         (server (net.aserve:start :port port :server :new :listeners 2 :host "127.0.0.1"))
         (seen nil)
         (bytes (let ((v (make-array 300000 :element-type '(unsigned-byte 8))))
                  (dotimes (i (length v) v) (setf (aref v i) (mod (* i 7) 256)))))
         (old-gate *gate*)
         (old-max *max-upload-bytes*)
         (old-small *max-request-bytes*)
         (file (format nil "/tmp/stratum-upload-test-~d.sexp" (get-universal-time))))
    (flet ((post (path body &optional (content-type "application/octet-stream"))
             (raw-post port path body content-type)))
      (unwind-protect
           (progn
             (publish-door server nil "/up" "application/json"
                           (lambda (req) (setf seen (request-octets req)) "{}") :upload? t)
             (publish-door server nil "/small" "application/json" (lambda (req) (declare (ignore req)) "{}"))
             (publish-prefix-doors server nil)
             (setf *gate* (invite-gate:make-gate :name "Stratum" :prefix "/stratum" :cookie "stratum_invite"))
             (check "an upload arrives whole" (eql (first (post "/up" bytes)) 200))
             (check "byte for byte, as octets"
                    (and (typep seen '(simple-array (unsigned-byte 8) (*))) (equalp seen bytes))
                    (and seen (length seen)))
             ;; an image as the page sends it: rows of differences, deflated
             (let* ((w 31) (h 17)
                    (rgba (let ((v (test-noise (* 4 w h))))
                            (loop for i from 3 below (length v) by 4 do (setf (aref v i) 255))
                            v))
                    (image nil))
               (flet ((packed (channels)
                        (let ((rows (make-array (* channels w h) :element-type '(unsigned-byte 8))))
                          (dotimes (y h)
                            (dotimes (x w)
                              (dotimes (c channels)
                                (setf (aref rows (+ c (* channels (+ x (* y w)))))
                                      (logand 255 (- (aref rgba (+ c (* 4 (+ x (* y w)))))
                                                     (if (> x 0) (aref rgba (+ c (* 4 (+ x -1 (* y w))))) 0)))))))
                          (coerce (deflate-octets rows) '(simple-array (unsigned-byte 8) (*)))))
                      (path (enc &optional (height h)) (format nil "/img?w=~d&h=~d&enc=~a" w height enc)))
                 (publish-door server nil "/img" "application/json"
                               (lambda (req) (setf image (request-image req)) "{}") :upload? t)
                 (check "rows of differences, three bytes a pixel, come back as the image"
                        (and (eql (first (post (path "sub3") (packed 3))) 200) (equalp image rgba)))
                 (setf (aref rgba 3) 7 (aref rgba 131) 0)
                 (check "four bytes a pixel, with its alpha"
                        (and (eql (first (post (path "sub4") (packed 4))) 200) (equalp image rgba)))
                 (let ((answer (post (path "sub3" (1+ h)) (packed 3))))
                   (check "data shorter than its stated size is refused"
                          (and (eql (first answer) 500) (search "ended after" (second answer))) answer))
                 (let ((answer (post (path "sub3" (1- h)) (packed 3))))
                   (check "data longer than its stated size is refused"
                          (and (eql (first answer) 500) (search "runs past" (second answer))) answer))))
             ;; a worker thread that fails, behind a real door: the words
             ;; come back, and neither the lock nor a worker stays behind
             (publish-door server nil "/slices" "application/json"
                           (lambda (req) (declare (ignore req))
                             (parallel-ranges 64 (lambda (start end)
                                                   (declare (ignore end))
                                                   (when (zerop start) (error "A slice failed."))))
                             "{}"))
             (publish-door server nil "/slices-out" "application/json"
                           (lambda (req) (declare (ignore req))
                             (parallel-ranges 64 (lambda (start end)
                                                   (declare (ignore start end))
                                                   (error (make-condition 'storage-condition))))
                             "{}"))
             (let ((answer (post "/slices" (make-array 0 :element-type '(unsigned-byte 8)))))
               (check "a slice that fails is a 500 with its words"
                      (and (eql (first answer) 500) (search "A slice failed." (second answer))) answer))
             (check "a slice that runs out of memory is a 500 too"
                    (eql (first (post "/slices-out" (make-array 0 :element-type '(unsigned-byte 8)))) 500))
             (check "the lock is free after them"
                    (let ((got (bt:acquire-lock *lock* nil)))
                      (when got (bt:release-lock *lock*))
                      got))
             (check "no worker is left behind"
                    (notany (lambda (thread) (member (bt:thread-name thread) '("stratum filter" "stratum render")
                                                     :test #'equal))
                            (bt:all-threads)))
             (check "and the next request is answered"
                    (eql (first (post "/small" (make-array 0 :element-type '(unsigned-byte 8)))) 200))
             (check "a door tells every cache to keep nothing"
                    (search (format nil "~(cache-control: no-store~)")
                            (string-downcase (second (post "/small" (make-array 0 :element-type '(unsigned-byte 8))))))
                    (second (post "/small" (make-array 0 :element-type '(unsigned-byte 8)))))
             ;; pixels on the way down: packed for a page that asks
             (publish-door server nil "/px" "application/octet-stream"
                           (lambda (req) (declare (ignore req)) bytes) :pack 4)
             (let ((plain (raw-get port "/px")) (packed (raw-get port "/px?enc=d")))
               (check "a door of pixels answers them as they are, unasked"
                      (and (eql (first plain) 200) (equalp (second plain) bytes))
                      (length (second plain)))
               (check "and packed when asked: the same bytes, in fewer"
                      (and (eql (first packed) 200)
                           (< (length (second packed)) (length bytes))
                           (equalp (unpack-pixels (second packed)) bytes))
                      (length (second packed))))
             (setf *max-upload-bytes* 1000 *max-request-bytes* 1000)
             (let ((answer (post "/up" bytes)))
               (check "an upload over the limit is told why"
                      (and (eql (first answer) 413) (search "MB" (second answer))) answer))
             (let ((answer (post "/small" bytes)))
               (check "any other door has its limit" (eql (first answer) 413) answer))
             (setf *max-upload-bytes* old-max *max-request-bytes* old-small)
             (configure-gate! :invites-file file)
             (with-open-file (out file :direction :output :if-exists :supersede) (terpri out))
             (let ((answer (post "/up" bytes)))
               (check "no invite: the gate's page, the body unread"
                      (and (eql (first answer) 403) (search "private beta" (second answer))) answer))
             (let ((answer (post "/up" bytes "application/x-www-form-urlencoded")))
               (check "a form post is turned away before the gate reads it"
                      (and (eql (first answer) 415) (search "no form posts" (second answer))) answer))
             (let ((answer (post "/stratum/nothing-here" bytes "application/x-www-form-urlencoded")))
               (check "nothing published: our 404, the body unread"
                      (and (eql (first answer) 404) (search "Not found" (second answer))) answer))
             (let ((answer (post "/stratum/invite/nobody" bytes)))
               (check "the invite link answers GET alone" (eql (first answer) 403) answer))
             (check "the server still answers after a refusal"
                    (progn (configure-gate! :invites-file nil)
                           (eql (first (post "/up" bytes)) 200))))
        (setf *gate* old-gate *max-upload-bytes* old-max *max-request-bytes* old-small)
        (when (probe-file file) (delete-file file))
        (net.aserve:shutdown :server server)))))

(defun test-api ()
  "The Lisp API (api.lisp) and the sandbox the prompt box runs forms
through: every form the model may write, and the ones it may not."
  (let ((stratum-api:*document* nil)
        (*documents* (make-hash-table :test 'equal)))
    (let ((info (stratum-api:use-document "parity")))
      (check "use-document: the sample's size"
             (and (= (getf info :width) 320) (= (getf info :height) 200)) (getf info :width)))
    (check "describe-document names the layers"
           (search "index 0 is the bottom" (stratum-api:describe-document)))
    (let ((before (length (stratum-api:layers))))
      (let ((added (stratum-api:add-layer :curves :name "Warm")))
        (check "add-layer answers the new layer's info"
               (and (eql (getf added :kind) :curves) (equal (getf added :name) "Warm")) added))
      (check "one more layer" (= (length (stratum-api:layers)) (1+ before)))
      (let ((set (stratum-api:set-layer "Warm" :opacity 0.5 :red '((0 0) (0.5 0.6) (1 1)))))
        (check "set-layer by name: opacity and a curve"
               (and (< (abs (- (getf set :opacity) 0.5)) 1e-6) (= (length (getf set :red)) 3)) set))
      (check "set-layer refuses a setting the layer lacks"
             (handler-case (progn (stratum-api:set-layer "Warm" :gamma 2) nil)
               (error () t)))
      (stratum-api:delete-layer "Warm")
      (check "delete-layer" (= (length (stratum-api:layers)) before)))
    (stratum-api:select-rect 10 10 50 40)
    (check "select-rect's bounds" (equal (stratum-api:selection) '(10 10 50 40)) (stratum-api:selection))
    (let ((r (stratum-api:fill-selection 0 '(200 10 10))))
      (check "fill-selection answers the rectangle and the undo depth"
             (and (getf r :changed) (= (getf r :undo-depth) 1)) r))
    (check "undo" (getf (stratum-api:undo) :undone))
    (stratum-api:select-none)
    (check "select-none" (null (stratum-api:selection)))
    (let ((r (stratum-api:levels 0 :gamma 1.3)))
      (check "levels bakes into the whole layer" (equal (getf r :changed) '(0 0 320 200)) r))
    (stratum-api:undo)
    (let ((results (stratum:run-forms "(gaussian-blur 0 2) (hue-saturation 0 :hue 10) (undo) (undo) (selection)")))
      (check "run-forms: five forms, all passed"
             (and (= (length results) 5) (every (lambda (r) (getf r :ok)) results)) results))
    (let ((results (stratum:run-forms "(open-image \"/etc/hostname\") (levels 0)")))
      (check "run-forms refuses a file operation and stops there"
             (and (= (length results) 1) (not (getf (first results) :ok))
                  (search "not an operation" (getf (first results) :error)))
             results))
    (let ((results (stratum:run-forms "(levels 0 :gamma (+ 1 1))")))
      (check "run-forms refuses a nested call"
             (and (not (getf (first results) :ok)) (search "literal" (getf (first results) :error))) results))
    (let ((results (stratum:run-forms "(levels 0 :gamma #.(+ 1 1))")))
      (check "run-forms reads with read-eval off"
             (and (= (length results) 1) (not (getf (first results) :ok))) results))
    (let ((results (stratum:run-forms "(select-polygon ((0 0) (100 0) (50 80))) (crop 0 0 100 80)")))
      (check "an unquoted list is data; crop remakes the document under its name"
             (and (every (lambda (r) (getf r :ok)) results)
                  (= (getf (stratum-api:use-document "parity") :width) 100))
             results))))

(defun test-prompt ()
  "The prompt door's pieces that need no model: the reply parsed, the
picture, the reference and the rules."
  (multiple-value-bind (code prose)
      (sexp-agent:parse-forms (format nil "I will warm it up.~%```lisp~%(levels 0 :gamma 1.1)~%(undo)~%```~%"))
    (check "parse-forms: the block's code and the prose"
           (and (search "(levels 0 :gamma 1.1)" code) (equal prose "I will warm it up.")) (list code prose)))
  (multiple-value-bind (code prose) (sexp-agent:parse-forms "(undo)")
    (check "parse-forms without a block: all code" (and (equal code "(undo)") (equal prose "")) (list code prose)))
  (check "the reference document names the package" (search "stratum-api" (api-reference)))
  (check "the system text carries the reference and the rules"
         (let ((text (sexp-agent:system-text *agent*)))
           (and (search "stratum-api" text) (search "fenced code block" text))))
  (let ((stratum-api:*document* nil)
        (*documents* (make-hash-table :test 'equal)))
    (stratum-api:use-document "parity")
    (let ((png (document-picture-png stratum-api:*document* 64)))
      (check "the picture is a PNG of the scaled size"
             (and (png-octets-p png)
                  (multiple-value-bind (pixels w h) (read-png png)
                    (declare (ignore pixels))
                    (and (= w 64) (= h 40))))
             (length png))))
  (check "base64 of a known vector"
         (equal (sexp-agent:base64-encode (coerce '(77 97 110) '(simple-array (unsigned-byte 8) (*)))) "TWFu")))

(defun stub-model-reply (text)
  "A Messages response whose one text block is TEXT, or for (:refusal
why) what a refusal is: HTTP 200, stop_reason refusal, no text."
  (flet ((obj (&rest kv)
           (let ((o (make-hash-table :test 'equal)))
             (loop for (k v) on kv by #'cddr do (setf (gethash k o) v))
             o)))
    (yason:with-output-to-string* ()
      (yason:encode
       (if (consp text)
           (obj "id" "msg_stub" "type" "message" "role" "assistant"
                "content" '()
                "stop_reason" "refusal"
                "stop_details" (obj "type" "refusal" "category" nil "explanation" (second text))
                "usage" (obj "input_tokens" 100 "output_tokens" 0))
           (obj "id" "msg_stub" "type" "message" "role" "assistant"
                "content" (list (obj "type" "thinking" "thinking" "")
                                (obj "type" "text" "text" text))
                "stop_reason" "end_turn"
                "usage" (obj "input_tokens" 100 "output_tokens" 10)))))))

(defun call-with-stub-model (replies fn)
  "FN called with an agent whose model is a stand-in on a server of its
own, answering REPLIES, texts, in turn (an empty block when they run
out; (:bad-request message) answers 400), and with a function answering the requests it has had, parsed,
oldest first."
  (let* ((port (free-port))
         (server (net.aserve:start :port port :server :new :listeners 2 :host "127.0.0.1"))
         (requests '())
         (agent (sexp-agent:make-agent :name "Stratum" :package :stratum-api
                                       :operators stratum-api:*prompt-operators*
                                       :reference "The reference."
                                       :messages-url (format nil "http://127.0.0.1:~d/messages" port))))
    (unwind-protect
         (progn
           (net.aserve:publish
            :path "/messages" :server server :content-type "application/json"
            :function (lambda (req ent)
                        (push (yason:parse (gwl:request-body-text req)) requests)
                        (let* ((reply (or (pop replies) (format nil "```lisp~%```")))
                               (bad? (and (consp reply) (eq (first reply) :bad-request))))
                          (net.aserve:with-http-response (req ent :content-type "application/json"
                                                                  :response (if bad?
                                                                                net.aserve:*response-bad-request*
                                                                                net.aserve:*response-ok*))
                            (net.aserve:with-http-body (req ent)
                              (write-string (if bad?
                                                (format nil "{\"error\":{\"message\":~s}}" (second reply))
                                                (stub-model-reply reply))
                                            (net.aserve:request-reply-stream req)))))))
           (funcall fn agent (lambda () (reverse requests))))
      (net.aserve:shutdown :server server))))

(defun last-user-content (request)
  "The content blocks of REQUEST's last message: (values images texts)."
  (let ((content (gethash "content" (car (last (gethash "messages" request))))))
    (values (remove "image" content :key (lambda (b) (gethash "type" b)) :test-not #'equal)
            (format nil "~{~a~}" (loop for b in content
                                       when (equal (gethash "type" b) "text") collect (gethash "text" b))))))

(defun test-looks ()
  "Seeing: the Color blend, :intersect, the pictures (look) makes, and
prompts in which the model looks before it acts and after -- against a
stand-in for the model."
  (flet ((near (a b) (< (abs (- a b)) 1d-9))
         (luminance (r g b) (+ (* 0.2126d0 r) (* 0.7152d0 g) (* 0.0722d0 b))))
    (multiple-value-bind (r g b) (color-blend 0.5d0 0.5d0 0.5d0 0.8d0 0.1d0 0.1d0)
      (check "color blend: the backdrop's luminance, the source's hue"
             (and (near (luminance r g b) 0.5d0) (> r g) (near g b) (<= 0d0 g) (<= r 1d0)) (list r g b)))
    (multiple-value-bind (r g b) (color-blend 0.2d0 0.6d0 0.1d0 0.5d0 0.5d0 0.5d0)
      (check "color blend: a gray source leaves the backdrop's gray"
             (let ((l (luminance 0.2d0 0.6d0 0.1d0))) (and (near r l) (near g l) (near b l))) (list r g b)))
    (multiple-value-bind (r g b) (color-blend 0d0 0d0 0d0 0.8d0 0.1d0 0.1d0)
      (check "color blend: black stays black" (and (near r 0d0) (near g 0d0) (near b 0d0)) (list r g b)))
    (multiple-value-bind (r g b) (color-blend 1d0 1d0 1d0 1d0 0d0 0d0)
      (check "color blend: white stays white" (and (near r 1d0) (near g 1d0) (near b 1d0)) (list r g b))))
  (check "an OpenRaster file names the Color blend" (equal (cdr (assoc :color *ora-blend-ops*)) "svg:color"))
  (let ((gray (coerce '(0 2 4 6  10 12 14 16  100 100 100 100  200 200 200 204) '(simple-array (unsigned-byte 8) (*)))))
    (check "scale-rect: reduced, each pixel the mean of its block"
           (equalp (scale-rect gray 4 1 0 0 4 4 2 1) #(6 10 150 151)) (scale-rect gray 4 1 0 0 4 4 2 1))
    (check "scale-rect: a rectangle enlarged, each pixel a block"
           (equalp (scale-rect gray 4 1 1 0 2 1 1 2) #(2 2 4 4 2 2 4 4)) (scale-rect gray 4 1 1 0 2 1 1 2)))
  (let ((stratum-api:*document* nil)
        (*documents* (make-hash-table :test 'equal)))
    (stratum-api:use-document "parity")
    (let* ((doc stratum-api:*document*)
           (before (copy-seq (the-object doc (render-cpu))))
           (layer (find-if (lambda (info) (and (eql (getf info :kind) :raster) (plusp (getf info :index))))
                           (stratum-api:layers)))
           (index (getf layer :index)))
      (stratum-api:set-layer index :blend :color)
      (check "a layer set to Color: the shader blends it so, and the CPU picture moves"
             (and (search "blend_color(cb, s.rgb)" (the-object doc shader-source))
                  (not (equalp before (the-object doc (render-cpu))))))
      (stratum-api:set-layer index :blend (getf layer :blend))
      (check "and back: the picture as it was" (equalp before (the-object doc (render-cpu)))))
    (stratum-api:select-rect 10 10 50 40)
    (stratum-api:select-rect 30 20 100 100 :mode :intersect)
    (check "a selection intersected: what both cover"
           (equal (stratum-api:selection) '(30 20 30 30)) (stratum-api:selection))
    (stratum-api:select-none)
    (let ((whole (stratum-api:look)) (small (stratum-api:look 10 10 50 40)))
      (check "look: the whole document, a pixel a pixel"
             (and (picture-p whole) (= (picture-width whole) 320) (= (picture-height whole) 200)
                  (search "one picture pixel a document pixel" (picture-note whole))
                  (search "Nothing is selected" (picture-note whole)))
             (picture-note whole))
      (check "look: a small rectangle comes enlarged, and says how to read it"
             (and (= (picture-width small) 400) (= (picture-height small) 320)
                  (search "(10 + px / 8, 10 + py / 8)" (picture-note small)))
             (picture-note small)))
    (check "a picture larger than it may be is reduced by a whole factor"
           (search "(5 px, 5 py)" (picture-note (document-picture stratum-api:*document* :size 64))))
    (let ((plain (read-png (picture-png (stratum-api:look)))))
      (stratum-api:select-rect 100 50 100 80)
      (let ((shown (read-png (picture-png (stratum-api:look))))
            (hidden (read-png (picture-png (stratum-api:look :selection nil)))))
        (flet ((px (pixels x y) (subseq pixels (* 4 (+ x (* 320 y))) (+ 3 (* 4 (+ x (* 320 y)))))))
          (check "look shows the selection: magenta on its edge, the inside as it is, the outside dimmer"
                 (and (equalp (px shown 100 90) #(255 0 255))
                      (equalp (px shown 150 90) (px plain 150 90))
                      (every #'<= (px shown 20 20) (px plain 20 20))
                      (not (equalp shown plain)))
                 (list (px shown 100 90) (px shown 150 90) (px plain 150 90)))
          (check "and leaves it out when asked" (equalp hidden plain)))))
    (stratum-api:select-none)
    (let ((results (stratum:run-forms "(look 0 0 100 100) (look 1 2)")))
      (check "the sandbox admits look, and look says how it is called"
             (and (picture-p (getf (first results) :value))
                  (not (getf (second results) :ok))
                  (search "four numbers" (getf (second results) :error)))
             results))
    ;; a prompt in which the model looks, selects, looks again and fills
    (let ((doc stratum-api:*document*))
      (setf (gethash "parity" *documents*) doc)
      (call-with-stub-model
       (list (format nil "I will look first.~%```lisp~%(look 0 0 100 100)~%```")
             (format nil "Now the selection, and a check.~%```lisp~%(select-rect 10 10 50 40)~%(look 0 0 100 100)~%```")
             (format nil "It is right.~%```lisp~%(fill-selection 0 (200 10 10))~%(select-none)~%```"))
       (lambda (agent requests)
         (let* ((*agent* agent)
                (answer (prompt-document doc "make that corner red"))
                (requests (funcall requests)))
           (check "a prompt with two looks: three calls of the model, every form run"
                  (and (= (length requests) 3) (getf answer :ok) (= (length (getf answer :results)) 5))
                  (list (length requests) answer))
           (multiple-value-bind (images text) (last-user-content (first requests))
             (check "the request carries the document's picture and says how to read it"
                    (and (= (length images) 1) (search "The picture with this message is" text)
                         (search "make that corner red" text))
                    text))
           (multiple-value-bind (images text) (last-user-content (second requests))
             (check "a look comes back as a picture with its caption"
                    (and (= (length images) 1) (search "Picture 1: the document's rectangle x 0 y 0 w 100 h 100" text)
                         (search "more pictures" text))
                    text))
           (multiple-value-bind (images text) (last-user-content (third requests))
             (check "the second look shows the selection"
                    (and (= (length images) 1) (search "The selection is shown" text)) text))
           (check "a look's value on the page is its caption"
                  (search "the document's rectangle" (getf (first (getf answer :results)) :value))
                  (first (getf answer :results)))
           (check "the reply is every round's prose"
                  (and (search "I will look first." (getf answer :reply)) (search "It is right." (getf answer :reply)))
                  (getf answer :reply))
           (check "the fill went on the undo list and the turn on the document"
                  (and (= (length (the-object doc undo-entries)) 1)
                       (search "(look 0 0 100 100)" (cdr (first (the-object doc prompt-log)))))))))
      (stratum-api:undo)
      ;; the looks are counted
      (call-with-stub-model
       (list (format nil "Looking.~%```lisp~%(look 0 0 100 100)~%```")
             (format nil "Looking again.~%```lisp~%(look 0 0 50 50)~%```")
             (format nil "Never asked.~%```lisp~%(levels 0 :gamma 2)~%```"))
       (lambda (agent requests)
         (let* ((*agent* agent)
                (*prompt-looks* 1)
                (answer (prompt-document doc "look around"))
                (requests (funcall requests)))
           (check "one look allowed: the second picture is not sent, and the model is told"
                  (and (= (length requests) 2)
                       (search "Do not ask for another picture" (nth-value 1 (last-user-content (second requests))))
                       (= (length (getf answer :results)) 2))
                  (list (length requests) (getf answer :results)))))))))

(defun pixels-changed (a b w box)
  "How many pixels of A and B (RGBA8, W wide) differ (values inside
BOX, (x y w h), and outside it)."
  (destructuring-bind (bx by bw bh) box
    (let ((inside 0) (outside 0))
      (dotimes (i (floor (length a) 4) (values inside outside))
        (let ((x (mod i w)) (y (floor i w)) (o (* 4 i)))
          (unless (and (= (aref a o) (aref b o)) (= (aref a (+ o 1)) (aref b (+ o 1)))
                       (= (aref a (+ o 2)) (aref b (+ o 2))))
            (if (and (<= bx x) (< x (+ bx bw)) (<= by y) (< y (+ by bh)))
                (incf inside)
                (incf outside))))))))

(defun test-spills ()
  "An edit asked for one part of the picture stays there: the bakes
keep to the selection, a selection cannot silently become everything,
and an adjustment layer can be confined.  'Lighten her face' lightened
the whole picture and 'paint the thumb green' turned all of it green."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "parity") stratum-api:*document*))
         (w (the-object doc doc-width))
         (box '(100 50 60 40)))
    (flet ((layer-bytes () (copy-seq (the-object (the-object doc (layer 0)) pixels)))
           (picture () (copy-seq (the-object doc (render-cpu)))))
      (dolist (form '("(levels 0 :gamma 2)" "(hue-saturation 0 :hue 120 :saturation 0.5)"
                      "(curves 0 :green ((0 0) (0.5 0.8) (1 1)))"))
        (let ((before (layer-bytes)))
          (stratum:run-forms (format nil "(select-rect 100 50 60 40) ~a (select-none)" form))
          (multiple-value-bind (inside outside) (pixels-changed before (layer-bytes) w box)
            (check (format nil "~a under a selection changes it and nothing else" form)
                   (and (plusp inside) (zerop outside)) (list inside outside)))
          (stratum-api:undo)
          (check "and undo gives the layer back" (equalp before (layer-bytes)))))
      ;; a soft selection: the bake fades out across its edge
      (let ((before (layer-bytes)))
        (stratum:run-forms "(select-rect 100 50 60 40) (feather 8) (levels 0 :gamma 2) (select-none)")
        (let* ((after (layer-bytes))
               (o (* 4 (+ 130 (* 70 w))))
               (e (* 4 (+ 100 (* 70 w)))))
          (check "a feathered selection takes the bake by degrees"
                 (< 0 (abs (- (aref after e) (aref before e))) (abs (- (aref after o) (aref before o))))
                 (list (aref before e) (aref after e) (aref before o) (aref after o)))
          (stratum-api:undo)))
      (let ((before (layer-bytes)))
        (stratum-api:levels 0 :gamma 2)
        (check "with nothing selected it still takes the whole layer"
               (multiple-value-bind (inside outside) (pixels-changed before (layer-bytes) w box)
                 (and (plusp inside) (plusp outside))))
        (stratum-api:undo))
      ;; a selection cannot become everything by mistake
      (stratum-api:select-rect 10 10 20 20)
      (let ((results (stratum:run-forms "(select-rect 200 150 20 20 :mode :intersect) (fill-selection 0 (0 255 0))")))
        (check "an intersection with nothing in it is refused, and the fill after it does not run"
               (and (= (length results) 1) (not (getf (first results) :ok))
                    (search "leave nothing selected" (getf (first results) :error))
                    (equal (stratum-api:selection) '(10 10 20 20)))
               results))
      (dolist (form '("(select-rect 0 0 320 200 :mode :subtract)" "(select-polygon ((0 0) (320 0) (320 200) (0 200)) :mode :subtract)"))
        (stratum-api:select-rect 10 10 20 20)
        (check (format nil "~a is refused" form)
               (and (not (getf (first (stratum:run-forms form)) :ok))
                    (equal (stratum-api:selection) '(10 10 20 20)))))
      (stratum-api:select-all)
      (check "inverting everything is refused"
             (and (not (getf (first (stratum:run-forms "(invert-selection)")) :ok))
                  (stratum-api:selection)))
      (stratum-api:select-none)
      (check "select-none still clears" (null (stratum-api:selection)))
      ;; an adjustment layer confined to the selection
      (let ((before (picture)))
        (stratum:run-forms "(select-rect 100 50 60 40) (add-layer :levels :name \"Lighter\") (set-layer \"Lighter\" :gamma 2) (set-mask \"Lighter\" :selection) (select-none)")
        (multiple-value-bind (inside outside) (pixels-changed before (picture) w box)
          (check "an adjustment layer with the selection as its mask changes that part alone"
                 (and (plusp inside) (zerop outside)) (list inside outside)))
        (check "and has a mask to say so" (getf (stratum-api:layer-info "Lighter") :mask))
        (stratum-api:add-layer :raster :name "Above")
        (multiple-value-bind (inside outside) (pixels-changed before (picture) w box)
          (check "the mask lasts through a change to the stack" (and (plusp inside) (zerop outside))
                 (list inside outside)))
        (stratum-api:delete-layer "Above")
        (stratum-api:delete-layer "Lighter"))
      (check "a mask from nothing selected is refused"
             (not (getf (first (stratum:run-forms "(set-mask 0 :selection)")) :ok))))))

(defun test-prompt-log ()
  "The prompt log: nothing without a directory; with one, each prompt's
words, forms and small pictures, however it ended; never a token; and a
log that cannot be written costs nobody their prompt."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "demo") stratum-api:*document*))
         (root (format nil "/tmp/stratum-prompt-log-~d/" (get-universal-time)))
         (*prompt-log-directory* nil))
    (flet ((turns () (sort (mapcar #'namestring (uiop:subdirectories root)) #'string<))
           (read-turn (dir)
             (with-open-file (in (merge-pathnames "turn.sexp" dir) :external-format :utf-8)
               (let ((*package* (find-package :common-lisp)))
                 (read in))))
           (png-size (file)
             (multiple-value-bind (pixels w h) (read-png (stratum-api::file-octets file))
               (declare (ignore pixels))
               (max w h))))
      (unwind-protect
           (progn
             (prompt-with doc "(select-rect 10 10 20 20)")
             (check "no directory, no log" (null (probe-file root)))
             (stratum-api:select-none)
             (let ((*prompt-log-turns* 2))
               (check "configure-prompt-log! makes the directory"
                      (and (configure-prompt-log! :directory root) (probe-file root)))
               (call-with-stub-model
                (list (format nil "Looking.~%```lisp~%(look 0 0 1000 800)~%```")
                      (format nil "Done.~%```lisp~%(select-rect 40 40 60 60)~%```"))
                (lambda (agent requests)
                  (declare (ignore requests))
                  (let ((*agent* agent))
                    (prompt-document doc "make it blue — please" :session "s1" :who "Alice"))))
               (let* ((dir (first (turns))) (turn (read-turn dir)) (outcome (getf turn :outcome)))
                 (check "a prompt's words, who asked, its forms and how it went"
                        (and (equal (getf turn :prompt) "make it blue — please")
                             (equal (getf turn :who) "Alice")
                             (equal (getf turn :rounds) 2)
                             (search "index 0 is the bottom" (getf turn :state))
                             (getf outcome :ok)
                             (= (length (getf outcome :results)) 2)
                             (search "Done." (getf outcome :reply)))
                        turn)
                 (check "the caption of what it looked at"
                        (search "x 0 y 0 w 1000 h 800" (first (getf turn :looks))) (getf turn :looks))
                 (check "the pictures, small: before, the look, after"
                        (every (lambda (name) (<= (png-size (merge-pathnames name dir)) *prompt-log-picture-size*))
                               '("before.png" "look-1.png" "after.png"))))
               (stratum-api:select-none)
               ;; a prompt that failed outright
               (let ((*agent* (sexp-agent:make-agent :name "Stratum" :package :stratum-api
                                                     :operators stratum-api:*prompt-operators*
                                                     :reference "The reference."
                                                     :messages-url (format nil "http://127.0.0.1:~d/none" (free-port)))))
                 (handler-case (prompt-document doc "this one fails" :who "Bob")
                   (error () nil)))
               (let ((turn (read-turn (second (turns)))))
                 (check "a prompt that failed is kept, with what went wrong"
                        (and (equal (getf turn :prompt) "this one fails") (stringp (getf turn :failure))
                             (null (getf turn :outcome)))
                        turn))
               (prompt-with doc "(select-none)")
               (check "the oldest go past the limit"
                      (and (= (length (turns)) 2)
                           (equal (getf (read-turn (first (turns))) :prompt) "this one fails"))
                      (turns)))
             ;; a directory that cannot be written
             (let ((*prompt-log-directory* (merge-pathnames "turn.sexp/" (first (turns)))))
               (check "a log that cannot be written costs nobody their prompt"
                      (getf (prompt-with doc "(select-none)") :ok))))
        (setf *prompt-log-directory* nil)
        (when (probe-file root) (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t))))))

(defun test-long-prompt ()
  "A prompt whose pictures outgrow a request drops the oldest, and one
whose later call fails keeps what it did and says so."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "parity") stratum-api:*document*))
         (look (format nil "Looking.~%```lisp~%(look 0 0 100 100)~%```")))
    (flet ((ask (&rest replies)
             (let (seen answer)
               (call-with-stub-model
                replies
                (lambda (agent requests)
                  (let ((*agent* agent))
                    (setf answer (prompt-document doc "do something")
                          seen (funcall requests)))))
               (values answer seen)))
           (images (request)
             (loop for message in (gethash "messages" request)
                   for content = (gethash "content" message)
                   when (consp content)
                     sum (count "image" content :key (lambda (b) (gethash "type" b)) :test #'equal)))
           (words (request)
             (format nil "~{~a~}"
                     (loop for message in (gethash "messages" request)
                           for content = (gethash "content" message)
                           if (stringp content) collect content
                           else append (loop for b in content
                                             when (equal (gethash "type" b) "text")
                                               collect (gethash "text" b))))))
      (multiple-value-bind (answer seen) (ask look look)
        (check "with room, every picture goes on every round"
               (and (getf answer :ok) (equal (mapcar #'images seen) '(1 2 3))) seen))
      (multiple-value-bind (answer seen)
          (let ((sexp-agent:*max-picture-characters* 1)) (ask look look))
        (check "past the limit only the newest goes, and the prompt goes on"
               (and (getf answer :ok) (equal (mapcar #'images seen) '(1 1 1))
                    (= 1 (length (last-user-content (car (last seen))))))
               seen)
        (check "and the model is told what was left out"
               (search "left out" (words (car (last seen))))))
      (multiple-value-bind (answer seen)
          (ask (format nil "Selecting.~%```lisp~%(select-rect 10 10 20 20)~%(look 0 0 100 100)~%```")
               '(:bad-request "the request body is too big"))
        (declare (ignore seen))
        (check "a later call failing: what ran stands, and the reply says what stopped it"
               (and (equal (stratum-api:selection) '(10 10 20 20))
                    (search "Selecting." (getf answer :reply))
                    (search "too big" (getf answer :reply))
                    (getf answer :stopped) (not (getf answer :ok))
                    (= 2 (length (getf answer :results))))
               answer)
        (check "and the page hears it"
               (eq (gethash "stopped" (yason:parse (prompt-json answer))) t)))
      (multiple-value-bind (answer seen) (ask (format nil "Fine.~%```lisp~%(selection)~%```"))
        (declare (ignore answer))
        (check "the next prompt remembers the stopped one"
               (and (search "Selecting." (words (first seen)))
                    (search "too big" (words (first seen)))))
        (stratum-api:undo)
        (check "and the stopped prompt is one undo like any other" (null (stratum-api:selection)))))))

(defun test-declined ()
  "The model declining reads as that, not as a blank turn: on the first
round and after forms have run, in the page's answer, and without
leaving an empty turn to break the next request."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "parity") stratum-api:*document*)))
    (flet ((ask (&rest replies)
             (let (seen answer)
               (call-with-stub-model
                replies
                (lambda (agent requests)
                  (let ((*agent* agent))
                    (setf answer (prompt-document doc "do something")
                          seen (funcall requests)))))
               (values answer seen))))
      (let ((answer (ask '(:refusal "It falls outside what may be done."))))
        (check "a refusal says so, with the reason, and marks the answer"
               (and (search "The model declined this request." (getf answer :reply))
                    (search "falls outside" (getf answer :reply))
                    (getf answer :declined) (not (getf answer :ok)) (null (getf answer :results)))
               answer)
        (check "and the page hears it"
               (eq (gethash "declined" (yason:parse (prompt-json answer))) t)))
      (let ((answer (ask '(:refusal nil))))
        (check "a refusal with no reason given reads the same"
               (equal (getf answer :reply) "The model declined this request.") answer))
      (multiple-value-bind (answer seen) (ask (format nil "Done.~%```lisp~%(selection)~%```"))
        (declare (ignore answer))
        (check "the turn after a refusal goes out with no empty turn in its history"
               (notany (lambda (message)
                         (and (equal (gethash "role" message) "assistant")
                              (equal (gethash "content" message) "")))
                       (gethash "messages" (first seen)))
               seen))
      (let ((answer (ask (format nil "Selecting.~%```lisp~%(select-rect 10 10 20 20)~%(look 0 0 100 100)~%```")
                         '(:refusal nil))))
        (check "a refusal part way: what ran stands, the words join the reply"
               (and (equal (stratum-api:selection) '(10 10 20 20))
                    (search "Selecting." (getf answer :reply))
                    (search "The model declined" (getf answer :reply))
                    (getf answer :declined))
               answer)
        (stratum-api:undo)
        (check "and it is one undo like any other" (null (stratum-api:selection))))
      (let ((answer (ask "")))
        (check "a model that says nothing is not a blank turn either"
               (search "gave no answer" (getf answer :reply)) answer))
      (let ((answer (ask (format nil "Fine.~%```lisp~%(selection)~%```"))))
        (check "an ordinary answer is not declined"
               (and (getf answer :ok) (not (getf answer :declined))) answer)))))

(defun call-with-stub-generator (fn &key (status 202) (job-state "completed") (color '(0 255 0 255)) paint
                                         capabilities)
  "FN called with a generator on a server of its own, a stand-in for
sd-server's job API: a job is accepted with STATUS and is then JOB-STATE
at its second asking, with a picture of COLOR at the size asked -- or
of PAINT, a function of (w h init), INIT the picture it was sent or nil,
answering RGBA.  CAPABILITIES, a string, is the answer to asking what
it can do; with none it has no such door.  And with a function answering
what it has seen: (body lock-free?) each, oldest first."
  (let* ((port (free-port))
         (server (net.aserve:start :port port :server :new :listeners 2 :host "127.0.0.1"))
         (seen '())
         (asked 0)
         (size nil)
         (init nil))
    (flet ((json (req ent code value)
             (net.aserve:with-http-response (req ent :content-type "application/json" :response code)
               (net.aserve:with-http-body (req ent)
                 (write-string (yason:with-output-to-string* () (yason:encode value))
                               (net.aserve:request-reply-stream req)))))
           (object (&rest kv)
             (let ((o (make-hash-table :test 'equal)))
               (loop for (k v) on kv by #'cddr do (setf (gethash k o) v))
               o)))
      (net.aserve:publish
       :path "/sdcpp/v1/img_gen" :server server :content-type "application/json"
       :function (lambda (req ent)
                   (let ((body (yason:parse (gwl:request-body-text req)))
                         (free (let ((got (bt:acquire-lock *lock* nil)))
                                 (when got (bt:release-lock *lock*))
                                 (and got t))))
                     (push (list body free) seen)
                     (setf asked 0
                           size (list (gethash "width" body) (gethash "height" body))
                           init (let ((url (gethash "init_image" body)))
                                  (and url (read-png (cl-base64:base64-string-to-usb8-array
                                                      (subseq url (1+ (position #\, url))))))))
                     (json req ent
                           (if (eql status 202)
                               (net.aserve::make-resp 202 "Accepted")
                               net.aserve:*response-internal-server-error*)
                           (object "id" "j1" "kind" "img_gen" "status" "queued"
                                   "poll_url" "/sdcpp/v1/jobs/j1")))))
      (when capabilities
        (net.aserve:publish
         :path "/sdcpp/v1/capabilities" :server server :content-type "application/json"
         :function (lambda (req ent)
                     (net.aserve:with-http-response (req ent :content-type "application/json")
                       (net.aserve:with-http-body (req ent)
                         (write-string capabilities (net.aserve:request-reply-stream req)))))))
      (net.aserve:publish
       :path "/sdcpp/v1/jobs/j1" :server server :content-type "application/json"
       :function (lambda (req ent)
                   (incf asked)
                   (json req ent net.aserve:*response-ok*
                         (cond ((< asked 2) (object "status" "generating"))
                               ((equal job-state "completed")
                                (destructuring-bind (w h) size
                                  (let ((rgba (if paint
                                                  (funcall paint w h init)
                                                  (make-rgba w h))))
                                    (unless paint
                                      (dotimes (i (* w h)) (replace rgba color :start1 (* 4 i))))
                                    (object "status" "completed"
                                            "result" (object "output_format" "png"
                                                             "images" (list (object "index" 0 "b64_json"
                                                                                    (sexp-agent:base64-encode (png-octets rgba w h)))))))))
                               (t (object "status" job-state
                                          "error" (object "code" "generation_failed" "message" "out of memory")))))))
      (unwind-protect
           (let ((*generator-poll-seconds* 0.05))
             (funcall fn
                      (make-generator (format nil "http://127.0.0.1:~d" port) 4 1 0.9d0 256 "" 20 nil)
                      (lambda () (reverse seen))))
        (net.aserve:shutdown :server server)))))

(defun test-generate ()
  "New pixels from a generator, against a stand-in: a crop and a mask go
out; what comes back lands inside the selection alone, on a layer of its
own, and undoes with the prompt; the lock stays free meanwhile."
  (check "sizes for a generator are multiples of 64, the longer side as asked"
         (multiple-value-bind (w h) (fitted-size 3000 1000 1024)
           (and (= w 1024) (= h 320) (zerop (mod h 64)))))
  (check "a selection's crop has a margin, inside the picture"
         (and (equal (grown-rect '(100 100 100 100) 1000 1000) '(65 65 170 170))
              (equal (grown-rect '(0 0 100 100) 110 1000) '(0 0 110 135))))
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "parity") stratum-api:*document*))
         (w (the-object doc doc-width))
         (box '(100 50 60 40)))
    (flet ((picture () (copy-seq (the-object doc (render-cpu)))))
      (let ((*generator* nil))
        (check "no generator: the form says so"
               (search "no image generator"
                       (getf (first (stratum:run-forms "(generate-layer \"a cat\")")) :error))))
      (call-with-stub-generator
       (lambda (generator seen)
         (let ((*generator* generator)
               (before (picture))
               (layers (length (stratum-api:layers))))
           (check "generate-fill without a selection is told why"
                  (search "Nothing is selected"
                          (getf (first (stratum:run-forms "(generate-fill \"grass\")")) :error)))
           (stratum-api:select-rect 100 50 60 40)
           (let ((result (first (stratum:run-forms "(generate-fill \"grass\" :name \"Grass\" :keep :all)"))))
             (check "generate-fill: a layer of that name on top"
                    (and (getf result :ok) (equal (getf (getf result :value) :name) "Grass")
                         (= (length (stratum-api:layers)) (1+ layers)))
                    result))
           (multiple-value-bind (inside outside) (pixels-changed before (picture) w box)
             (check "the picture changes inside the selection and nowhere else"
                    (and (plusp inside) (zerop outside)) (list inside outside)))
           (let* ((pixels (the-object (the-object doc (layer (1- (length (stratum-api:layers))))) pixels))
                  (alpha (lambda (x y) (aref pixels (+ 3 (* 4 (+ x (* y w))))))))
             (check "the layer fades in from the selection's edge, and is whole in its middle"
                    (and (= (funcall alpha 130 70) 255)
                         (< (funcall alpha 100 70) 40)
                         (< (funcall alpha 100 70) (funcall alpha 105 70) (funcall alpha 130 70))
                         (zerop (funcall alpha 99 70)))
                    (list (funcall alpha 99 70) (funcall alpha 100 70) (funcall alpha 105 70) (funcall alpha 130 70))))
           (destructuring-bind (body free) (first (funcall seen))
             (flet ((png-of (key)
                      (let ((url (gethash key body)))
                        (cl-base64:base64-string-to-usb8-array (subseq url (1+ (position #\, url)))))))
               (check "a crop and a mask, at the generator's size, with the prompt"
                      (and (equal (gethash "prompt" body) "grass")
                           (= (gethash "sample_steps" (gethash "sample_params" body)) 4)
                           (png-octets-p (png-of "init_image"))
                           (<= (max (gethash "width" body) (gethash "height" body)) 256))
                      (list (gethash "width" body) (gethash "height" body)))
               ;; byte 25 of a PNG is its color type: 0 is one gray channel
               (check "the mask is a PNG of one channel, as the job API asks"
                      (let ((mask (png-of "mask_image")))
                        (and (png-octets-p mask) (zerop (aref mask 25))))
                      (aref (png-of "mask_image") 25)))
             (check "the lock is free while the generator works" free))
           (stratum-api:select-none)
           (stratum-api:delete-layer "Grass")
           (check "taking the layer off gives the picture back" (equalp before (picture)))
           (stratum:run-forms "(generate-layer \"a meadow\" :name \"Meadow\")")
           (destructuring-bind (body free) (car (last (funcall seen)))
             (declare (ignore free))
             (check "generate-layer: a picture of the document's shape, no mask"
                    (and (null (gethash "mask_image" body)) (null (gethash "init_image" body))
                         (getf (stratum-api:layer-info "Meadow") :index))
                    (gethash "prompt" body)))
           (stratum-api:delete-layer "Meadow")
           ;; from a prompt: the layer goes with the prompt's one undo
           (let ((answer (prompt-with doc "(select-rect 100 50 60 40) (generate-fill \"grass\" :name \"Grass\") (select-none)")))
             (check "a prompt that generates runs, and says it had a generator"
                    (getf answer :ok) answer))
           (let ((after (picture)))
             (check "the prompt changed the picture" (not (equalp before after)))
             (stratum-api:undo)
             (check "one undo takes the generated layer off"
                    (and (equalp before (picture)) (= (length (stratum-api:layers)) layers)))))))
      ;; a generator that hands back the left half painted and the
      ;; right half as it was sent: only the left half is kept
      (call-with-stub-generator
       (lambda (generator seen)
         (declare (ignore seen))
         (let ((*generator* generator)
               (before (picture)))
           (flet ((alpha (x y)
                    (let ((pixels (the-object (the-object doc (layer (1- (length (stratum-api:layers))))) pixels)))
                      (aref pixels (+ 3 (* 4 (+ x (* y w))))))))
             (stratum-api:select-rect 100 50 60 40)
             (check ":keep takes :changed or :all"
                    (search ":changed or :all"
                            (getf (first (stratum:run-forms "(generate-fill \"grass\" :keep :some)")) :error)))
             (stratum-api:generate-fill "grass" :name "Half")
             (check "what the generator painted is kept, what it gave back as sent is left clear"
                    (and (= (alpha 115 70) 255) (zerop (alpha 150 70)) (zerop (alpha 158 70)))
                    (list (alpha 115 70) (alpha 150 70) (alpha 158 70)))
             (multiple-value-bind (inside outside) (pixels-changed before (picture) w '(100 50 60 40))
               (check "and the picture changed inside the selection alone"
                      (and (plusp inside) (zerop outside)) (list inside outside)))
             (stratum-api:delete-layer "Half")
             (stratum-api:generate-fill "grass" :name "All" :keep :all)
             (check ":keep :all keeps the whole selection"
                    (and (= (alpha 115 70) 255) (= (alpha 150 70) 255))
                    (list (alpha 115 70) (alpha 150 70)))
             (stratum-api:delete-layer "All")
             (stratum-api:select-none)
             (check "nothing of it is left" (equalp before (picture))))))
       :paint (lambda (w h init)
                (let ((rgba (copy-seq init)))
                  (dotimes (y h rgba)
                    (dotimes (x (floor w 2))
                      (replace rgba '(0 255 0 255) :start1 (* 4 (+ x (* y w)))))))))
      (call-with-stub-generator
       (lambda (generator seen)
         (declare (ignore seen))
         (let ((*generator* generator))
           (stratum-api:select-rect 100 50 60 40)
           (let ((result (first (stratum:run-forms "(generate-fill \"grass\")"))))
             (check "a generator that fails is a failed form in words, and nothing changes"
                    (and (not (getf result :ok)) (search "answered 500" (getf result :error)))
                    result))
           (stratum-api:select-none)))
       :status 500)
      (call-with-stub-generator
       (lambda (generator seen)
         (declare (ignore seen))
         (let ((*generator* generator))
           (stratum-api:select-rect 100 50 60 40)
           (let ((result (first (stratum:run-forms "(generate-fill \"grass\")"))))
             (check "a job that fails says why"
                    (and (not (getf result :ok)) (search "out of memory" (getf result :error)))
                    result))
           (stratum-api:select-none)))
       :job-state "failed")
      (let ((*generator* (make-generator (format nil "http://127.0.0.1:~d" (free-port)) 4 1 0.9d0 256 "" 5 nil)))
        (stratum-api:select-rect 100 50 60 40)
        (check "a generator that is down says so"
               (search "could not be reached"
                       (getf (first (stratum:run-forms "(generate-fill \"grass\")")) :error)))
        (stratum-api:select-none)))))

(defun prompt-with (doc forms)
  "A prompt on DOC whose model answers FORMS, text, and then nothing."
  (call-with-stub-model
   (list (format nil "Done.~%```lisp~%~a~%```" forms))
   (lambda (agent requests)
     (declare (ignore requests))
     (let ((*agent* agent))
       (prompt-document doc "do it")))))

(defun test-prompt-undo ()
  "A prompt is one undo: whatever its forms did to the stack, the
settings, the selection, the mode and the pixels comes back at once,
and comes back again on redo; the undo list before it stays good."
  (let* ((stratum-api:*document* nil)
         (*documents* (make-hash-table :test 'equal))
         (doc (progn (stratum-api:use-document "parity") stratum-api:*document*))
         (untouched (copy-seq (the-object doc (render-cpu)))))
    (flet ((picture () (copy-seq (the-object doc (render-cpu))))
           (depths () (list (length (the-object doc undo-entries)) (length (the-object doc redo-entries))))
           (layer-names () (mapcar (lambda (i) (getf i :name)) (stratum-api:layers)))
           (opacity () (getf (stratum-api:layer-info 0) :opacity)))
      ;; a stroke from before the prompt
      (stratum-api:select-rect 5 5 20 20)
      (stratum-api:fill-selection 0 '(0 200 0))
      (stratum-api:select-none)
      (let ((before (picture)) (names (layer-names)) (was (opacity)))
        (prompt-with doc "(select-rect 40 40 60 60)
(fill-selection 0 (10 10 200))
(add-layer :raster :name \"Tint\")
(fill-selection \"Tint\" (200 30 30))
(set-layer \"Tint\" :blend :color)
(set-layer 0 :opacity 0.5)
(gaussian-blur 0 2)
(feather 2)")
        (let ((after (picture)) (selected (stratum-api:selection)))
          (check "a prompt that added a layer: one entry over the stroke before it"
                 (and (equal (depths) '(2 0)) (not (equalp before after))
                      (= (length (layer-names)) (1+ (length names))))
                 (depths))
          (stratum-api:undo)
          (check "one undo: the picture, byte for byte"
                 (equalp (picture) before))
          (check "and the stack, the setting and the selection"
                 (and (equal (layer-names) names) (= (opacity) was) (null (stratum-api:selection))
                      (equal (depths) '(1 1)))
                 (list (layer-names) (opacity) (depths)))
          (stratum-api:redo)
          (check "redo: the prompt's picture, byte for byte, and its selection"
                 (and (equalp (picture) after) (equal (stratum-api:selection) selected)
                      (equal (depths) '(2 0)) (search "Tint" (format nil "~a" (layer-names))))
                 (depths))
          (stratum-api:undo)
          (stratum-api:undo)
          (check "the undo list before the prompt still works: the stroke comes off"
                 (equalp (picture) untouched))
          (stratum-api:redo)
          (stratum-api:redo)
          (check "and both come back" (equalp (picture) after))
          ;; the words 'undo that'
          (prompt-with doc "(undo)")
          (check "a prompt of (undo) takes the last prompt back and leaves no entry of its own"
                 (and (equalp (picture) before) (equal (depths) '(1 1))) (depths))
          (prompt-with doc "(redo)")
          (check "and (redo) brings it back" (and (equalp (picture) after) (equal (depths) '(2 0))) (depths))
          (stratum-api:undo)))
      (let ((before (picture)))
        (prompt-with doc "(look 0 0 50 50) (layers) (selection)")
        (check "a prompt that only looked leaves no undo" (equal (depths) '(1 1)) (depths))
        (prompt-with doc "(color-mode :grayscale)")
        (check "grayscale from a prompt, taken back whole"
               (and (eql (the-object doc color-mode) :grayscale)
                    (progn (stratum-api:undo)
                           (and (eql (the-object doc color-mode) :rgb) (equalp (picture) before)))))
        (prompt-with doc "(add-layer :raster :name \"Masked\")
(set-mask 0 :reveal)
(paint 0 ((30 30) (60 60)) :radius 8 :color (0 0 0) :target :mask)
(levels 0 :gamma 1.4)
(levels 0 :gamma 0.8)
(delete-layer \"Masked\")")
        (stratum-api:undo)
        (check "a mask made and painted, two filters and a layer deleted: back in one"
               (and (equalp (picture) before) (not (getf (stratum-api:layer-info 0) :mask)))))
      (prompt-with doc "(select-rect 10 10 30 30) (undefined-operation 1)")
      (check "a prompt that failed part way is one undo as well"
             (and (equal (stratum-api:selection) '(10 10 30 30))
                  (progn (stratum-api:undo) (null (stratum-api:selection))))
             (stratum-api:selection))
      (check "the journal is put away when a prompt ends" (null (the-object doc prompt-journal)))
      (let ((name (the-object doc doc-name)))
        (prompt-with doc "(crop 0 0 100 100)")
        (let ((new (gethash name *documents*)))
          (check "a prompt that remade the document has no undo, as a crop has none"
                 (and (not (eq new doc)) (= (the-object new doc-width) 100)
                      (null (the-object doc prompt-journal))
                      (null (the-object new undo-entries))))))
      (check "a whole layer goes in the journal once"
             (let* ((stratum-api:*document* nil)
                    (d (progn (stratum-api:use-document "demo") stratum-api:*document*))
                    (journal (begin-prompt-journal! d)))
               (stratum-api:levels 0 :gamma 1.2)
               (stratum-api:levels 0 :gamma 0.9)
               (stratum-api:gaussian-blur 0 1)
               (prog1 (= (length (prompt-journal-items journal)) 1)
                 (end-prompt-journal! d journal t)))))))

(defun run-tests ()
  "Run every check; print one line each; T when all passed."
  (setf *checks* nil)
  (dolist (test '(test-rebuilds test-layer-types test-curves test-adjustments test-rect
                  test-brush test-clone test-heal test-history test-mode test-masks test-adjust test-selection test-wand test-modify-selection test-cut-out test-refine-selection test-select-subject test-floating test-transform test-resize test-rotate
                  test-file-format test-ora test-drawing test-filter test-gradient test-merge
                  test-layers test-files test-png test-sessions test-session-locks test-invite test-uploads test-planes test-stamps test-api test-prompt test-looks test-prompt-undo test-spills test-prompt-log test-declined test-long-prompt test-generate test-generate-direct test-generate-references test-generate-turn test-fill-fade test-first-load test-selection-rects test-jpeg
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
