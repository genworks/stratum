;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;
;; The Lisp API: everything the editor does from its menus and tools,
;; as functions of literal arguments on a current document, so that
;; anything done interactively can be done by evaluating s-expressions
;; -- at a REPL, from a program, or by the prompt box's agent
;; (prompt.lisp), which runs the model's forms through sexp-agent's
;; sandbox over the operators in *prompt-operators*.  docs/api.md is
;; the reference; keep the two together.
;;
;; Every operation works on *document* (use-document sets it; the
;; prompt door binds it to the page's document), names layers by index
;; (0 is the bottom) or by name, and returns a plist or a list that
;; prints plainly.  Nothing here takes a GDL object as an argument.
;;

(defpackage :stratum-api
  (:use :cl)
  (:export #:*document* #:*prompt-operators*
           #:use-document #:documents #:new-document #:close-document
           #:describe-document #:document-info #:layers #:layer-info
           #:add-layer #:delete-layer #:duplicate-layer #:move-layer #:merge-down #:flatten
           #:set-layer #:set-mask
           #:select-rect #:select-polygon #:select-wand #:select-all #:select-none
           #:invert-selection #:feather #:contract-selection #:expand-selection #:refine-selection #:select-subject #:selection #:look #:generate-fill #:generate-layer #:generate-background #:generate-picture
           #:generator-models #:use-generator-model
           #:references #:place-reference #:add-reference #:remove-reference #:clear-references
           #:fill-selection #:clear-selection #:paint #:gradient #:move-pixels #:transform
           #:copy #:cut #:paste #:cut-out
           #:levels #:curves #:hue-saturation #:gaussian-blur #:unsharp-mask #:add-noise
           #:crop #:image-size #:canvas-size #:rotate #:color-mode
           #:undo #:redo #:set-history-source
           #:drawing-sources #:place-drawing
           #:open-image #:export-png #:save-document #:load-document))

(in-package :stratum-api)

(defvar *document* nil
  "The document the operations work on; use-document sets it.")

(defparameter *prompt-operators*
  '(documents describe-document document-info layers layer-info look generate-fill generate-layer generate-background generate-picture
    add-layer delete-layer duplicate-layer move-layer merge-down flatten set-layer set-mask
    select-rect select-polygon select-wand select-all select-none invert-selection feather
    contract-selection expand-selection refine-selection select-subject selection
    fill-selection clear-selection paint gradient move-pixels transform copy cut paste cut-out
    levels curves hue-saturation gaussian-blur unsharp-mask add-noise
    crop image-size canvas-size rotate color-mode undo redo set-history-source
    drawing-sources place-drawing references place-reference)
  "The operations the prompt box's agent may run: everything but the
file operations, the choice of document and the attaching of reference
pictures, which are the page's.")

;;; Finding things.

(defun doc ()
  (or *document* (error "No document is in use: (use-document \"name\") first.")))

(defun layer-of (which)
  "The layer WHICH names: an index from the bottom, or a name."
  (let ((layers (gdl:the-object (doc) layer-list)))
    (cond ((typep which 'stratum:image-layer) which)
          ((and (integerp which) (<= 0 which) (< which (length layers)))
           (nth which layers))
          ((integerp which)
           (error "There is no layer ~a; the document has ~a (0 to ~a)."
                  which (length layers) (1- (length layers))))
          ((stringp which)
           (or (find which layers :key (lambda (l) (gdl:the-object l layer-name)) :test #'string-equal)
               (error "There is no layer named ~s." which)))
          (t (error "A layer is named by its index or its name, not ~s." which)))))

(defun raster-layer-of (which)
  (let ((layer (layer-of which)))
    (unless (eql (gdl:the-object layer kind) :raster)
      (error "Layer ~a (~s) is a ~(~a~) layer; this operation needs a raster layer."
             (gdl:the-object layer index) (gdl:the-object layer layer-name) (gdl:the-object layer kind)))
    layer))

(defun index (layer) (gdl:the-object layer index))

(defun points-list (points)
  (mapcar (lambda (p) (list (float (first p) 1.0) (float (second p) 1.0))) points))

(defun rect-result (layer rect)
  (list :layer (index layer) :changed rect
        :undo-depth (length (gdl:the-object (doc) undo-entries))))

(defun check-size (width height)
  (unless (and (integerp width) (integerp height) (<= 1 width 16384) (<= 1 height 16384))
    (error "A size is two whole numbers from 1 to 16384, not ~a x ~a." width height)))

;;; Documents.

(defun use-document (name)
  "Make the document NAME current (a sample is made on first sight);
its description."
  (setf *document* (stratum:find-document name))
  (document-info))

(defun documents ()
  "The names of the open documents."
  (let ((names '()))
    (maphash (lambda (name doc) (declare (ignore doc)) (push name names)) stratum::*documents*)
    (sort names #'string<)))

(defun new-document (name width height &key (background :white))
  "A new WIDTH x HEIGHT document of one Background layer, BACKGROUND
:white, :transparent or (r g b); made current."
  (check-size width height)
  (let* ((name (stratum::unique-document-name name))
         (doc (stratum::new-document name width height
                                     (if (listp background) (mapcar #'round background) background))))
    (setf (gethash name stratum::*documents*) doc
          *document* doc)
    (document-info)))

(defun close-document (&optional name)
  "Forget the document NAME, or the current one."
  (let ((name (or name (gdl:the-object (doc) doc-name))))
    (remhash name stratum::*documents*)
    (when (and *document* (string= name (gdl:the-object *document* doc-name)))
      (setf *document* nil))
    (list :closed name)))

(defun layer-info (which)
  "Layer WHICH as a plist: :index :name :kind :opacity :visible :mask,
:blend for a raster layer, each setting of an adjustment layer, and
the four channels' points of a curves layer."
  (let* ((layer (layer-of which))
         (kind (gdl:the-object layer kind)))
    (append (list :index (index layer)
                  :name (gdl:the-object layer layer-name)
                  :kind kind
                  :opacity (float (gdl:the-object layer opacity) 1.0)
                  :visible (and (gdl:the-object layer visible?) t)
                  :mask (and (gdl:the-object layer has-mask?) t))
            (when (eql kind :raster) (list :blend (gdl:the-object layer blend-mode)))
            (loop for name in (gdl:the-object layer setting-names)
                  append (list name (float (gdl:the-object layer (gdl:evaluate name)) 1.0)))
            (when (eql kind :curves)
              (list :master (points-list (stratum::normalize-points (gdl:the-object layer master)))
                    :red (points-list (stratum::normalize-points (gdl:the-object layer red)))
                    :green (points-list (stratum::normalize-points (gdl:the-object layer green)))
                    :blue (points-list (stratum::normalize-points (gdl:the-object layer blue))))))))

(defun layers ()
  "Every layer's layer-info, bottom first."
  (mapcar #'layer-info (gdl:the-object (doc) layer-list)))

(defun selection ()
  "The selection's bounds (x y w h), or nil when everything is selected."
  (let ((selection (gdl:the-object (doc) selection)))
    (and selection (getf selection :rect))))

(defun document-info ()
  "The current document as a plist: :name :width :height :mode :layers
(layer-info each) :selection :undo-depth :redo-depth."
  (let ((doc (doc)))
    (list :name (gdl:the-object doc doc-name)
          :width (gdl:the-object doc doc-width)
          :height (gdl:the-object doc doc-height)
          :mode (gdl:the-object doc color-mode)
          :layers (layers)
          :selection (selection)
          :undo-depth (length (gdl:the-object doc undo-entries))
          :redo-depth (length (gdl:the-object doc redo-entries)))))

(defun describe-document ()
  "The current document in words: size, mode, each layer with its
state, the selection, the undo depth."
  (let ((doc (doc))
        (*print-pretty* nil))
    (with-output-to-string (out)
      (format out "Document ~s: ~a x ~a pixels, ~a, ~a layer~:p (index 0 is the bottom):~%"
              (gdl:the-object doc doc-name) (gdl:the-object doc doc-width) (gdl:the-object doc doc-height)
              (if (eql (gdl:the-object doc color-mode) :grayscale) "grayscale" "RGB")
              (length (gdl:the-object doc layer-list)))
      (dolist (info (layers))
        (format out "  ~a ~s ~(~a~) opacity ~,2f~a~a~a"
                (getf info :index) (getf info :name) (getf info :kind) (getf info :opacity)
                (if (getf info :visible) "" " hidden")
                (if (getf info :mask) " with a mask" "")
                (if (getf info :blend) (format nil " blend ~(~a~)" (getf info :blend)) ""))
        (loop for (key value) on info by #'cddr
              unless (member key '(:index :name :kind :opacity :visible :mask :blend :master :red :green :blue))
                do (format out " ~(~a~) ~,2f" key value))
        (when (eql (getf info :kind) :curves)
          (format out " curves master ~a red ~a green ~a blue ~a"
                  (getf info :master) (getf info :red) (getf info :green) (getf info :blue)))
        (terpri out))
      (let ((selection (selection)))
        (if selection
            (format out "Selection: x ~a y ~a w ~a h ~a.~%" (first selection) (second selection)
                    (third selection) (fourth selection))
            (format out "Selection: none (everything).~%")))
      (format out "Undo depth ~a, redo depth ~a."
              (length (gdl:the-object doc undo-entries)) (length (gdl:the-object doc redo-entries))))))

;;; The stack.

(defun add-layer (kind &key above name)
  "A new layer of KIND (:raster, :curves, :levels or :hue-saturation)
above layer ABOVE, or on top; NAME names it.  Its layer-info."
  (let ((kind (or (find kind '(:raster :curves :levels :hue-saturation))
                  (error "A layer's kind is :raster, :curves, :levels or :hue-saturation, not ~s." kind)))
        (doc (doc)))
    (let ((spec (gdl:the-object doc (new-layer-spec kind))))
      (when name (setf (getf spec :name) name))
      (layer-info (gdl:the-object doc (add-layer! spec (and above (index (layer-of above)))))))))

(defun delete-layer (layer)
  "Layer LAYER removed; the index of the layer to work on next."
  (let ((doc (doc)))
    (list :active (gdl:the-object doc (delete-layer! (index (layer-of layer))))
          :layers (length (gdl:the-object doc layer-list)))))

(defun duplicate-layer (layer)
  "A copy of LAYER above it; its layer-info."
  (layer-info (gdl:the-object (doc) (duplicate-layer! (index (layer-of layer))))))

(defun move-layer (layer &optional (delta 1))
  "LAYER moved DELTA places up the stack (down when negative); its layer-info."
  (layer-info (gdl:the-object (doc) (move-layer! (index (layer-of layer)) delta))))

(defun merge-down (layer)
  "Raster LAYER merged into the raster layer under it; the merged layer's info."
  (layer-info (stratum::merge-down! (doc) (index (layer-of layer)))))

(defun flatten ()
  "Every layer merged into one; its layer-info."
  (layer-info (stratum::flatten! (doc))))

(defun set-layer (layer &rest changes &key opacity visible blend &allow-other-keys)
  "LAYER's state changed: :opacity 0-1, :visible t or nil, :blend
(:normal :multiply :screen :add :color, raster layers), a levels layer's
:in-black :in-white :gamma :out-black :out-white, a hue/saturation
layer's :hue :saturation :lightness, a curves layer's :master :red
:green :blue (points ((x y) ...) on 0-1).  Its layer-info after."
  (declare (ignore opacity visible blend))
  (let ((layer (layer-of layer)))
    (loop for (key value) on changes by #'cddr
          do (case key
               (:opacity (gdl:the-object layer (set-slot! :opacity (max 0d0 (min 1d0 (float value 1d0))))))
               (:visible (gdl:the-object layer (set-slot! :visible? (and value t))))
               (:blend
                (unless (eql (gdl:the-object layer kind) :raster)
                  (error "Only a raster layer has a blend mode."))
                (unless (member value stratum::*blend-modes*)
                  (error "A blend mode is one of ~{~(~a~)~^, ~}, not ~s." stratum::*blend-modes* value))
                (gdl:the-object layer (set-slot! :blend-mode value)))
               ((:master :red :green :blue)
                (unless (eql (gdl:the-object layer kind) :curves)
                  (error "Only a curves layer has a ~(~a~) curve." key))
                (gdl:the-object layer (set-slot! key (stratum::normalize-points value))))
               (t
                (unless (member key (gdl:the-object layer setting-names))
                  (error "Layer ~a (~(~a~)) has no setting ~s; it has ~{~(~s~)~^ ~}."
                         (index layer) (gdl:the-object layer kind) key (gdl:the-object layer setting-names)))
                (gdl:the-object layer (set-slot! key (float value 1d0))))))
    (layer-info (index layer))))

(defun set-mask (layer value)
  "LAYER's mask: :reveal (a white mask), :hide (a black one), :selection
(the selection, so what the layer does stays inside it) or :delete.
The undo list is emptied.  The layer-info after."
  (let ((layer (layer-of layer)))
    (if (eql value :selection)
        (let ((selection (gdl:the-object (doc) selection)))
          (unless selection (error "Nothing is selected to make a mask from."))
          (gdl:the-object layer (set-mask! '(:flat :value 0)))
          (gdl:the-object layer (set-slot! :own-mask (copy-seq (getf selection :mask))))
          (gdl:the-object layer (set-slot! :mask-version (1+ (gdl:the-object layer mask-version)))))
        (gdl:the-object layer (set-mask! (case value
                                           (:reveal '(:flat :value 255))
                                           (:hide '(:flat :value 0))
                                           (:delete nil)
                                           (t (error "A mask operation is :reveal, :hide, :selection or :delete, not ~s." value))))))
    (gdl:the-object (doc) (set-slot! :undo-entries nil))
    (layer-info (index layer))))

;;; The selection.

(defun mode-key (mode)
  (case mode
    ((:new nil) :new)
    (:add :add)
    (:subtract :subtract)
    (:intersect :intersect)
    (t (error "A selection mode is :new, :add, :subtract or :intersect, not ~s." mode))))

(defun selection-result () (list :selection (selection)))

(defun selecting (operation)
  "OPERATION, a function changing the selection, and the selection's
bounds.  One that leaves nothing selected is refused and the selection
stays: nothing selected means everything to the operations after it,
so a subtraction that took too much would turn a fill on one thing into
a fill of the whole layer."
  (let ((before (gdl:the-object (doc) selection)))
    (funcall operation)
    (unless (gdl:the-object (doc) selection)
      (gdl:the-object (doc) (set-slot! :selection before))
      (error "That would leave nothing selected, which every later operation takes for the whole picture.  The selection stays as it was."))
    (selection-result)))

(defun select-rect (x y w h &key (mode :new))
  "The W x H rectangle at X, Y selected; MODE :new, :add, :subtract or
:intersect (what both the old selection and this one cover).  The
selection's bounds."
  (selecting (lambda () (stratum::select-rect! (doc) (round x) (round y) (round w) (round h) (mode-key mode)))))

(defun select-polygon (points &key (mode :new))
  "The polygon through POINTS, ((x y) ...), selected."
  (selecting (lambda () (stratum::select-polygon! (doc) points (mode-key mode)))))

(defun select-wand (x y &key (tolerance 32) (contiguous t) (mode :new) layer)
  "The Magic Wand at X, Y: the colors within TOLERANCE of that pixel's,
contiguous or not, on LAYER (the composite when nil)."
  (selecting (lambda ()
               (stratum::select-wand! (doc) (round x) (round y)
                                      :tolerance tolerance :contiguous (and contiguous t)
                                      :mode (mode-key mode)
                                      :layer (and layer (layer-of layer))))))

(defun feather (radius)
  "The selection's edge softened over RADIUS pixels."
  (stratum::feather-selection! (doc) radius)
  (selection-result))

(defun contract-selection (pixels)
  "The selection's edge moved PIXELS in, except where it reaches the
picture's edge."
  (stratum::contract-selection! (doc) pixels)
  (selection-result))

(defun expand-selection (pixels)
  "The selection's edge moved PIXELS out."
  (stratum::expand-selection! (doc) pixels)
  (selection-result))

(defun refine-selection (&key (radius 8) (passes 5) layer)
  "The selection's edge moved onto the subject's own outline, judged by
color within RADIUS pixels either side of where it is, again around
where it went for each of PASSES, on LAYER (the composite when nil).
The edge comes out hard: feather after."
  (selecting (lambda ()
               (stratum::refine-selection! (doc) :radius radius :passes passes
                                           :layer (and layer (layer-of layer))))))

(defun select-subject (x y w h &key points exclude (mode :new) layer)
  "The person or thing in the W x H box at X, Y selected by model, its
edge on the subject's own; POINTS ((x y) ...) on it and EXCLUDE points
off it steer the model.  The box may be rough.  Judged on LAYER's
pixels, or the composite when nil."
  (selecting (lambda ()
               (stratum::select-subject! (doc) x y w h :points points :exclude exclude
                                                         :mode (mode-key mode)
                                                         :layer (and layer (layer-of layer))))))

(defun select-all ()
  "Everything selected."
  (stratum::select-all! (doc))
  (selection-result))

(defun select-none ()
  "Nothing selected, which is everything."
  (stratum::select-none! (doc))
  (selection-result))

(defun invert-selection ()
  "The selection inverted."
  (selecting (lambda () (stratum::select-inverse! (doc)))))

;;; Pixels.

(defun fill-selection (layer color &key (opacity 1))
  "The selection on raster LAYER filled with COLOR (r g b) at OPACITY."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::fill-selection! layer (mapcar #'round color) opacity nil))))

(defun clear-selection (layer)
  "The selection on raster LAYER made transparent."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::fill-selection! layer '(0 0 0) 1 t))))

(defun paint (layer points &key radius hardness spacing flow opacity color (mode :paint) (target :layer))
  "A brush stroke through POINTS, ((x y) ... or (x y pressure) ...), on
raster LAYER (TARGET :layer) or on LAYER's mask (:mask): RADIUS in
pixels, HARDNESS 0-1, SPACING as a fraction of the radius, FLOW and
OPACITY 0-1, COLOR (r g b), MODE :paint or :erase.  Unsaid brush
numbers take the default brush's."
  (let ((layer (if (eql target :mask) (layer-of layer) (raster-layer-of layer))))
    (unless (member mode '(:paint :erase))
      (error "A stroke's mode is :paint or :erase, not ~s." mode))
    (unless (member target '(:layer :mask))
      (error "A stroke's target is :layer or :mask, not ~s." target))
    (when (and (eql target :mask) (not (gdl:the-object layer has-mask?)))
      (error "Layer ~a has no mask to paint on." (index layer)))
    (let ((stroke (append (list :points points :mode mode
                                :color (or color (getf stratum::*default-brush* :color)))
                          (when radius (list :radius radius))
                          (when hardness (list :hardness hardness))
                          (when spacing (list :spacing spacing))
                          (when flow (list :flow flow))
                          (when opacity (list :opacity opacity)))))
      (let ((rect (stratum::apply-brush-stroke layer stroke :target target)))
        (if rect
            (rect-result layer rect)
            (list :layer (index layer) :changed nil))))))

(defun gradient (layer x0 y0 x1 y1 &key (shape :linear) (start '(0 0 0 255)) (end '(255 255 255 255)) (opacity 1))
  "A gradient from X0, Y0 (color START, (r g b a)) to X1, Y1 (color END)
laid over raster LAYER inside the selection; SHAPE :linear, :reflected,
:radial, :diamond or :angle."
  (let ((layer (raster-layer-of layer)))
    (unless (member shape stratum::*gradient-shapes*)
      (error "A gradient's shape is one of ~{~(~a~)~^, ~}, not ~s." stratum::*gradient-shapes* shape))
    (rect-result layer (stratum::gradient-layer! layer x0 y0 x1 y1
                                                 :shape shape :start start :end end :opacity opacity))))

(defun move-pixels (layer dx dy)
  "The selection's pixels on raster LAYER (all of them when nothing is
selected) moved DX, DY."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::move-pixels! layer (round dx) (round dy)))))

(defun transform (layer matrix)
  "The selection's pixels on raster LAYER through the affine MATRIX
(a b c d e f): x' = a x + b y + c, y' = d x + e y + f."
  (let ((layer (raster-layer-of layer)))
    (unless (and (listp matrix) (= (length matrix) 6) (every #'realp matrix))
      (error "A transform is six numbers (a b c d e f), not ~s." matrix))
    (rect-result layer (stratum::transform-pixels! layer matrix))))

(defun copy (layer)
  "The selection on raster LAYER copied to the clipboard; its size."
  (stratum::copy-selection! (raster-layer-of layer))
  (list :clipboard (list :width (getf (stratum::clipboard) :w) :height (getf (stratum::clipboard) :h))))

(defun cut (layer)
  "The selection on raster LAYER copied to the clipboard and cleared."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::cut-selection! layer))))

(defun paste (&optional above)
  "The clipboard as a new raster layer above layer ABOVE, or on top;
its layer-info."
  (unless (stratum::clipboard) (error "The clipboard is empty."))
  (layer-info (stratum::paste-clipboard! (doc) (and above (index (layer-of above))))))

(defun cut-out (layer &key name (defringe 3))
  "The selection on raster LAYER as a new raster layer on top, NAME its
name, the colors at its edge the subject's own rather than a mix with
what was behind it, and those within DEFRINGE pixels of the edge drawn
from deeper inside; its layer-info."
  (layer-info (stratum::cut-out-selection! (raster-layer-of layer) :name name :defringe defringe)))

;;; Adjustments and filters, baked into a raster layer's pixels.

(defun levels (layer &key (in-black 0) (in-white 1) (gamma 1) (out-black 0) (out-white 1))
  "Levels baked into raster LAYER: the input range IN-BLACK..IN-WHITE
(0-1) stretched to the output range OUT-BLACK..OUT-WHITE, GAMMA above
1 lifting the midtones."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::adjust-layer! layer :levels
                                               :settings (list :in-black in-black :in-white in-white
                                                               :gamma gamma :out-black out-black
                                                               :out-white out-white)))))

(defun curves (layer &key master red green blue)
  "Curves baked into raster LAYER: each channel a list of points ((x y)
...) on 0-1, (0 0) to (1 1) left unchanged."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::adjust-layer! layer :curves
                                               :curves (append (when master (list :master (stratum::normalize-points master)))
                                                               (when red (list :red (stratum::normalize-points red)))
                                                               (when green (list :green (stratum::normalize-points green)))
                                                               (when blue (list :blue (stratum::normalize-points blue))))))))

(defun hue-saturation (layer &key (hue 0) (saturation 0) (lightness 0))
  "Hue/Saturation baked into raster LAYER: HUE turned by degrees,
SATURATION -1 (gray) to 1 (doubled), LIGHTNESS -1 (black) to 1 (white)."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::adjust-layer! layer :hue-saturation
                                               :settings (list :hue hue :saturation saturation
                                                               :lightness lightness)))))

(defun gaussian-blur (layer radius)
  "Raster LAYER blurred by RADIUS pixels inside the selection."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::filter-layer! layer :gaussian-blur :settings (list :radius radius)))))

(defun unsharp-mask (layer &key (radius 2) (amount 100) (threshold 0))
  "Raster LAYER sharpened: AMOUNT in percent over RADIUS pixels, edges
below THRESHOLD levels left alone."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::filter-layer! layer :unsharp-mask
                                               :settings (list :radius radius :amount amount
                                                               :threshold threshold)))))

(defun add-noise (layer &key (amount 10) monochrome (seed 1))
  "Noise of AMOUNT percent added to raster LAYER, in color or MONOCHROME."
  (let ((layer (raster-layer-of layer)))
    (rect-result layer (stratum::filter-layer! layer :add-noise
                                               :settings (list :amount amount :monochrome (and monochrome t)
                                                               :seed seed)))))

;;; The whole image.  Each of these remakes the document under its
;;; name; the page reloads after one.

(defun replace-document! (new)
  (let ((name (gdl:the-object new doc-name)))
    (setf (gethash name stratum::*documents*) new
          *document* new)
    (list :width (gdl:the-object new doc-width) :height (gdl:the-object new doc-height))))

(defun crop (&optional x y w h)
  "The document cropped to the W x H rectangle at X, Y, or to the
selection; the new size."
  (let ((doc (doc)))
    (if x
        (replace-document! (stratum::crop-document doc (round x) (round y) (round w) (round h)))
        (let ((selection (gdl:the-object doc selection)))
          (unless selection (error "Nothing is selected to crop to."))
          (replace-document! (apply #'stratum::crop-document doc (getf selection :rect)))))))

(defun image-size (width height)
  "The document resampled to WIDTH x HEIGHT; the new size."
  (check-size width height)
  (replace-document! (stratum::resize-document (doc) width height)))

(defun canvas-size (width height &key (anchor '(0.5 0.5)))
  "The canvas made WIDTH x HEIGHT around the picture, ANCHOR (ax ay) on
0-1 saying where the old picture sits ((0 0) top left); the new size."
  (check-size width height)
  (replace-document! (stratum::canvas-size-document (doc) width height
                                                    (float (first anchor) 1d0) (float (second anchor) 1d0))))

(defun rotate (turn)
  "The whole image turned: TURN :cw, :ccw, :180, :flip-h, :flip-v, or
degrees clockwise; the new size."
  (replace-document!
   (cond ((realp turn) (stratum::rotate-document-arbitrary (doc) turn))
         ((member turn '(:cw :ccw :180 :flip-h :flip-v)) (stratum::rotate-document (doc) turn))
         (t (error "A turn is :cw, :ccw, :180, :flip-h, :flip-v or a number of degrees, not ~s." turn)))))

(defun color-mode (mode)
  "Image > Mode: :rgb or :grayscale."
  (unless (member mode '(:rgb :grayscale))
    (error "The color mode is :rgb or :grayscale, not ~s." mode))
  (stratum::set-color-mode! (doc) mode)
  (list :mode mode))

(defun undo ()
  "The last change undone; whether there was one, and the depths after."
  (let* ((doc (doc))
         (entry (stratum::undo-stroke! doc)))
    (list :undone (and entry t)
          :undo-depth (length (gdl:the-object doc undo-entries))
          :redo-depth (length (gdl:the-object doc redo-entries)))))

(defun redo ()
  "The last undo redone."
  (let* ((doc (doc))
         (entry (stratum::redo-stroke! doc)))
    (list :redone (and entry t)
          :undo-depth (length (gdl:the-object doc undo-entries))
          :redo-depth (length (gdl:the-object doc redo-entries)))))

(defun set-history-source ()
  "The History Brush's source set to the document as it is now."
  (list :history-version (stratum::set-history! (doc))))

;;; Gendl drawings.

(defun drawing-sources ()
  "The names of the Gendl models that can be drawn into a layer."
  (stratum::drawing-source-names))

(defun place-drawing (source &key (projection :trimetric) (hidden-lines :draw) (color '(0 0 0))
                                  (weight 1.5) (margin 0.05) above)
  "The registered Gendl model SOURCE drawn as a new raster layer:
PROJECTION a view name (drawing-sources lists the models; views are
:top :bottom :left :right :front :rear :trimetric :isometric),
HIDDEN-LINES :draw, :remove or :dashed, COLOR (r g b), line WEIGHT,
MARGIN as a fraction of the picture.  The layer's info."
  (let ((doc (doc)))
    (unless (and (stringp source) (gethash source stratum::*drawing-sources*))
      (error "No Gendl model is registered as ~s; drawing-sources lists them." source))
    (unless (member projection stratum::*drawing-projections*)
      (error "A view is one of ~{~(~a~)~^, ~}, not ~s." stratum::*drawing-projections* projection))
    (unless (member hidden-lines '(:draw :remove :dashed))
      (error "Hidden lines are :draw, :remove or :dashed, not ~s." hidden-lines))
    (layer-info (stratum::place-drawing! doc source
                                         :projection projection :hidden-lines hidden-lines
                                         :color (stratum::doc-color doc (mapcar #'round color))
                                         :weight (float weight 1d0) :margin (float margin 1d0)
                                         :above (and above (index (layer-of above)))))))

;;; Files, for Lisp callers (not operations of the prompt box).

(defun open-image (path &optional name)
  "The PNG file at PATH opened as a new document, NAME or the file's
name; made current."
  (setf *document* (stratum:open-png-file path name))
  (document-info))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun write-octets (path octets)
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence octets out))
  (list :wrote (namestring path) :bytes (length octets)))

(defun export-png (path)
  "The document flattened and written as a PNG file at PATH."
  (let ((doc (doc)))
    (write-octets path (stratum:png-octets (gdl:the-object doc (render-cpu))
                                           (gdl:the-object doc doc-width) (gdl:the-object doc doc-height)))))

(defun save-document (path)
  "The document, layers and all, written as a .stratum file at PATH."
  (write-octets path (stratum::document-octets (doc))))

(defun load-document (path &optional name)
  "The .stratum or OpenRaster file at PATH opened as a new document,
NAME or the file's name; made current."
  (let* ((name (stratum::unique-document-name (or name (pathname-name path) "untitled")))
         (octets (file-octets path))
         (doc (if (stratum::zip-octets-p octets)
                  (stratum::read-ora-document octets name)
                  (stratum::read-document-octets octets name))))
    (setf (gethash name stratum::*documents*) doc
          *document* doc)
    (document-info)))
