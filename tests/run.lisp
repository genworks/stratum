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
                      (not (all-gray (getf *clipboard* :pixels)))
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
    (check "an idle session is swept, and comes back empty"
           (progn (setf (cdr (gethash "test-a" *sessions*)) (- (get-universal-time) (* 2 *session-idle-seconds*)))
                  (sweep-sessions!)
                  (and (null (gethash "test-a" *sessions*))
                       (gethash "test-b" *sessions*)
                       (null (gethash "parity" (session-table "test-a"))))))
    (remhash "test-a" *sessions*)
    (remhash "test-b" *sessions*)))

(defun test-threads ()
  (let* ((doc (make-sample-document "parity"))
         (threaded (let ((*render-threads* 8)) (the-object doc (render-cpu))))
         (single (let ((*render-threads* 1)) (the-object doc (render-cpu)))))
    (check "a threaded render equals a single-threaded one" (equalp threaded single))))

(defun run-tests ()
  "Run every check; print one line each; T when all passed."
  (setf *checks* nil)
  (dolist (test '(test-rebuilds test-layer-types test-curves test-adjustments test-rect
                  test-brush test-clone test-heal test-mode test-masks test-adjust test-selection test-wand test-floating test-transform test-resize
                  test-file-format test-filter test-gradient test-merge
                  test-layers test-files test-png test-sessions
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
