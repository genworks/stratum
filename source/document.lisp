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
;; The document: a layer stack as a GDL object tree, so non-destructive
;; editing is dependency tracking.  Every setting is a slot.  The slots
;; that change the compositing PROGRAM (a layer's kind, blend mode, mask
;; presence, its place in the stack) feed `plan', and only `plan' feeds
;; `shader-source' and `cpu-kernel'; opacity, visibility and curve
;; points feed the uniform data.  So dragging a slider invalidates a
;; table and nothing else, and changing a blend mode rebuilds the
;; program exactly once, without any code saying when to.
;; `build-counts' shows it: [shader builds, kernel builds].
;;

(defparameter *blend-modes* '(:normal :multiply :screen :add))

(defun plist-merge (new old)
  "A plist with NEW's keys and values first and OLD's remaining ones after."
  (let ((result (copy-list new)))
    (loop for (k v) on old by #'cddr
          unless (loop for (key) on new by #'cddr thereis (eq key k))
            do (setf result (append result (list k v))))
    result))

(define-object image-layer ()
  :documentation
  (:description "What every layer of an image-document has, from its
spec plist: :name, :opacity, :visible? and :mask (a mask generator
spec).  raster-layer and curves-layer mix this in.")

  :input-slots
  (spec doc-width doc-height
   (layer-name (getf (the spec) :name (format nil "Layer ~a" (1+ (the index)))))
   (opacity (getf (the spec) :opacity 1.0) :settable)
   (visible? (getf (the spec) :visible? t) :settable)
   (mask-spec (getf (the spec) :mask) :settable)
   ("Vector of (unsigned-byte 8), or nil.  This layer's own mask buffer,
a copy of the generated mask made by the first stroke painted on the
mask; nil until then.  The spec's :mask-pixels carries it across a
change of the stack."
    own-mask (getf (the spec) :mask-pixels) :settable)
   ("Integer.  Counts the strokes and undos applied to own-mask in place;
bumping it is the signal that the mask moved.  It feeds mask-pixels,
never the plan."
    mask-version 0 :settable))

  :computed-slots
  ((has-mask? (and (the mask-spec) t))
   (mask-pixels (progn (the mask-version)
                       (when (the has-mask?)
                         (or (the own-mask)
                             (generate-mask (the mask-spec) (the doc-width) (the doc-height))))))
   (effective-opacity (if (the visible?) (coerce (the opacity) 'double-float) 0.0))
   (mask-index (position self (the parent masked-layers)))
   ("List of keywords. The numeric settings the page shows as sliders and
may set; each is a settable slot of this layer."
    setting-names nil)
   ("List of four numbers. This layer's entry in the shader's u_params."
    params (list 0 0 0 0))
   ("Plist.  The state this class keeps beyond the common keys, for
spec-now; by default its numeric settings.  A subclass with more
overrides it."
    own-state (loop for name in (the setting-names)
                    append (list name (the (evaluate name))))))

  :functions
  ((spec-now
    ()
    "This layer's spec with its current state written back into it:
name, opacity, visibility, mask, and own-state.  A change of the
document's layer-specs makes every layer object anew, so this is what a
layer carries across one."
    (plist-merge (list* :kind (the kind) :name (the layer-name)
                        :opacity (the opacity) :visible? (the visible?)
                        :mask (the mask-spec) :mask-pixels (the own-mask)
                        (the own-state))
                 (the spec)))

   (ensure-own-mask!
    ()
    "This layer's own mask buffer, copied from the generated mask on the
first call.  The layer must have a mask."
    (unless (the has-mask?) (error "~a has no mask." (the layer-name)))
    (or (the own-mask)
        (progn (the (set-slot! :own-mask (copy-seq (generate-mask (the mask-spec)
                                                                  (the doc-width) (the doc-height)))))
               (the own-mask))))

   (set-mask!
    (spec)
    "Give this layer the mask SPEC (a generator spec, or nil for none) in
place of what it had, own buffer included.  Mask presence is part of
the plan, so the programs follow."
    (the (set-slot! :own-mask nil))
    (the (set-slot! :mask-spec spec))
    (the (set-slot! :mask-version (1+ (the mask-version))))
    (the parent (set-slot! :undo-entries nil))
    (the parent (set-slot! :redo-entries nil)))))

(define-object lut-layer (image-layer)
  :documentation
  (:description "An adjustment applied through three 256-entry tables,
one per channel, on the encoded scale: curves and levels.  A subclass
supplies `luts'.")

  :computed-slots
  ((lut-row (* 3 (position self (the parent lut-layers))))
   (plan-entry (list (the kind) :lut (the lut-row)
                     :mask (the mask-index) :param (the index)))))

(define-object raster-layer (image-layer)
  :documentation
  (:description "Pixels from :source (a generator spec), composited
with :blend (:normal, :multiply, :screen or :add).")

  :input-slots
  ((blend-mode (getf (the spec) :blend :normal) :settable)
   ("Generator spec.  Where the pixels come from before any stroke; a
layer made without one is transparent."
    source (getf (the spec) :source '(:solid)))
   ("Vector of (unsigned-byte 8), or nil.  This layer's own pixel buffer,
a copy of the source made by its first stroke; nil until then, so an
unpainted layer shares the pixel cache's copy and never paints into it.
The spec's :pixels carries it across a change of the stack."
    own-pixels (getf (the spec) :pixels) :settable)
   ("Integer.  Counts the strokes and undos applied to own-pixels.  They
change the buffer in place, so bumping this is the signal that the
pixels moved: it feeds pixels (and so raster-vector and the CPU render)
and never the plan.  A mutable buffer with a version counter, rather
than a settable list of strokes replayed over the source, because a
replay of many strokes on a 4K layer costs seconds; undo keeps the
rectangles instead (see brush.lisp)."
    paint-version 0 :settable))

  :computed-slots
  ((kind :raster)
   (source-pixels (generate-pixels (the source) (the doc-width) (the doc-height)))
   (pixels (progn (the paint-version)
                  (or (the own-pixels) (the source-pixels))))
   (texture-index (position self (the parent raster-layers)))
   (plan-entry (list :raster :tex (the texture-index) :blend (the blend-mode)
                     :mask (the mask-index) :param (the index)))
   (own-state (list :blend (the blend-mode) :pixels (the own-pixels))))

  :functions
  ((ensure-own-pixels!
    ()
    "This layer's own pixel buffer, copied from the source on the first call."
    (or (the own-pixels)
        (progn (the (set-slot! :own-pixels (copy-seq (the source-pixels))))
               (the own-pixels))))))

(define-object curves-layer (lut-layer)
  :documentation
  (:description "An adjustment of everything below it: the :master
curve, then the :red, :green and :blue curves, each a list of (x y)
points in [0,1].")

  :input-slots
  ((master (getf (the spec) :master *identity-curve*) :settable)
   (red (getf (the spec) :red *identity-curve*) :settable)
   (green (getf (the spec) :green *identity-curve*) :settable)
   (blue (getf (the spec) :blue *identity-curve*) :settable))

  :computed-slots
  ((kind :curves)
   (luts (list (channel-lut (the master) (the red))
               (channel-lut (the master) (the green))
               (channel-lut (the master) (the blue))))
   (own-state (list :master (the master) :red (the red)
                    :green (the green) :blue (the blue)))))

(define-object levels-layer (lut-layer)
  :documentation
  (:description "Levels on all three channels: :in-black and :in-white
(the input range stretched to full), :gamma (above 1 lifts the
midtones), :out-black and :out-white (the output range), on the
encoded [0,1] scale.")

  :input-slots
  ((in-black (getf (the spec) :in-black 0) :settable)
   (in-white (getf (the spec) :in-white 1) :settable)
   (gamma (getf (the spec) :gamma 1) :settable)
   (out-black (getf (the spec) :out-black 0) :settable)
   (out-white (getf (the spec) :out-white 1) :settable))

  :computed-slots
  ((kind :levels)
   (setting-names '(:in-black :in-white :gamma :out-black :out-white))
   (luts (let ((lut (map '(simple-array double-float (*))
                         (lambda (v) (coerce v 'double-float))
                         (levels-lut (float (the in-black) 1d0) (float (the in-white) 1d0)
                                     (float (the gamma) 1d0)
                                     (float (the out-black) 1d0) (float (the out-white) 1d0)
                                     256))))
           (list lut lut lut)))))

(define-object hue-saturation-layer (image-layer)
  :documentation
  (:description "Hue, saturation and lightness of everything below it,
in HSL on the encoded scale: :hue turns by degrees, :saturation scales
by 1 + s (-1 removes the color), :lightness moves toward white (above
0) or black (below).")

  :input-slots
  ((hue (getf (the spec) :hue 0) :settable)
   (saturation (getf (the spec) :saturation 0) :settable)
   (lightness (getf (the spec) :lightness 0) :settable))

  :computed-slots
  ((kind :hue-saturation)
   (setting-names '(:hue :saturation :lightness))
   (params (list (/ (float (the hue) 1d0) 360) (the saturation) (the lightness) 0))
   (plan-entry (list :hue-saturation :mask (the mask-index) :param (the index)))))

(define-object image-document ()
  :documentation
  (:description "A layered raster document.  LAYER-SPECS lists the
layers bottom first, each a plist whose :kind (:raster, the default, or
:curves) picks its type; see image-layer, raster-layer and curves-layer
for the other keys.")

  :input-slots
  ((doc-name "untitled")
   (doc-width 1024)
   (doc-height 768)
   (layer-specs nil :settable)
   (background-srgb (list 1.0 1.0 1.0) :settable)
   ("Number.  The background's alpha for the CPU render: 1 for a document
as shown; 0 composites the stack over transparency, which a merge uses."
    background-alpha 1.0)
   ("Keyword.  Image > Mode: :rgb, or :grayscale, in which every raster
layer holds gray and whatever is painted, filled, placed or pasted is
turned to gray first (set-color-mode!, mode.lisp).  The pixels are RGBA8
either way."
    color-mode :rgb :settable)
   ("List or nil.  The History Brush's source: the layer specs of the
state it paints from, the document as opened (ensure-history-specs!
takes them at the first request) or the state Set Source chose;
history.lisp."
    history-specs nil :settable)
   ("Vector or nil.  The composite of history-specs, RGBA8 straight, made
when first wanted and kept until the source changes."
    history-pixels nil :settable)
   ("Integer.  Bumped when the history source changes, so the viewer
re-uploads its copy."
    history-version 0 :settable)
   ("List.  What the last strokes overwrote, newest first: (layer-index
target x y w h bytes) each, at most *undo-depth* of them.  undo-stroke!
in brush.lisp pops one and puts the bytes back."
    undo-entries nil :settable)
   ("List.  What the last undos took away, newest first, in the same
shape as undo-entries; redo-stroke! pops one and puts it back.  Any new
change empties it."
    redo-entries nil :settable)
   ("Plist (:mask bytes :rect (x y w h)), or nil.  The selection: an
8-bit coverage over the document and its bounds; nil selects nothing,
which paints everywhere.  selection.lisp makes and uses it."
    selection nil :settable))

  :objects
  ((layers :type (:sequence (the layer-types))
           :sequence (:size (length (the layer-specs)))
           :spec (nth (the-child index) (the layer-specs))
           :doc-width (the doc-width)
           :doc-height (the doc-height)))

  :computed-slots
  ((layer-types (mapcar (lambda (spec)
                          (ecase (getf spec :kind :raster)
                            (:raster 'raster-layer)
                            (:curves 'curves-layer)
                            (:levels 'levels-layer)
                            (:hue-saturation 'hue-saturation-layer)))
                        (the layer-specs)))
   (layer-list (list-elements (the layers)))
   (raster-layers (remove-if-not (lambda (l) (eql (the-object l kind) :raster))
                                 (the layer-list)))
   (lut-layers (remove-if-not (lambda (l) (typep l 'lut-layer)) (the layer-list)))
   (masked-layers (remove-if-not (lambda (l) (the-object l has-mask?))
                                 (the layer-list)))
   (lut-rows (* 3 (length (the lut-layers))))

   (plan (mapcar (lambda (l) (the-object l plan-entry)) (the layer-list)))
   (plan-version (format nil "~36r" (sxhash (prin1-to-string (the plan)))))
   (build-counts (make-array 2 :initial-element 0))
   (shader-source (prog1 (plan->glsl (the plan))
                    (incf (aref (the build-counts) 0))))
   (cpu-kernel (prog1 (compile nil (plan->lisp (the plan)))
                 (incf (aref (the build-counts) 1))))

   (opacity-vector (map '(simple-array double-float (*))
                        (lambda (l) (the-object l effective-opacity))
                        (the layer-list)))
   (lut-vector (let ((v (make-array (* 256 (max 1 (the lut-rows)))
                                    :element-type 'double-float :initial-element 0.0)))
                 (dolist (l (the lut-layers) v)
                   (loop for lut in (the-object l luts)
                         for row from (the-object l lut-row)
                         do (replace v lut :start1 (* 256 row))))))
   (params-vector (let ((v (make-array (* 4 (max 1 (length (the layer-list))))
                                       :element-type 'double-float :initial-element 0d0)))
                    (loop for l in (the layer-list)
                          for i from 0
                          do (loop for p in (the-object l params)
                                   for k from 0
                                   do (setf (aref v (+ (* 4 i) k)) (float p 1d0))))
                    v))
   (raster-vector (map 'simple-vector (lambda (l) (the-object l pixels))
                       (the raster-layers)))
   (mask-vector (map 'simple-vector (lambda (l) (the-object l mask-pixels))
                     (the masked-layers)))
   (background-linear (mapcar (lambda (c) (srgb->linear (coerce c 'double-float)))
                              (the background-srgb))))

  :functions
  ((layer (i) (nth i (the layer-list)))

   ;; The stack's structure: each operation writes every layer's current
   ;; state back into its spec (spec-now) and sets layer-specs, which
   ;; makes the layer objects anew from the new list; the plan changes
   ;; and the programs follow.  The undo list goes with it, since its
   ;; layer indices would be stale.
   (specs-now () (mapcar (lambda (l) (the-object l spec-now)) (the layer-list)))

   (replace-specs!
    (specs)
    (the (set-slot! :undo-entries nil))
    (the (set-slot! :redo-entries nil))
    (the (set-slot! :layer-specs specs)))

   (new-layer-spec
    (kind)
    "A fresh spec of KIND (:raster, :curves, :levels or :hue-saturation),
named in sequence as other editors do: Layer 3, Curves 1."
    (let ((count (count kind (the layer-list) :key (lambda (l) (the-object l kind)))))
      (ecase kind
        (:raster (list :kind :raster :name (format nil "Layer ~d" (1+ (length (the layer-list))))
                       :source '(:solid)))
        (:curves (list :kind :curves :name (format nil "Curves ~d" (1+ count))))
        (:levels (list :kind :levels :name (format nil "Levels ~d" (1+ count))))
        (:hue-saturation (list :kind :hue-saturation
                               :name (format nil "Hue/Saturation ~d" (1+ count)))))))

   (add-layer!
    (spec &optional above)
    "Insert SPEC above layer ABOVE, or on top; the new layer's index."
    (let* ((specs (the specs-now))
           (at (if above (1+ above) (length specs))))
      (the (replace-specs! (append (subseq specs 0 at) (list spec) (subseq specs at))))
      at))

   (delete-layer!
    (i)
    "Remove layer I; the index of the layer to work on next, or nil when
none is left."
    (let ((specs (the specs-now)))
      (the (replace-specs! (append (subseq specs 0 i) (subseq specs (1+ i)))))
      (let ((n (length (the layer-list))))
        (when (> n 0) (min i (1- n))))))

   (move-layer!
    (i delta)
    "Move layer I up the stack by DELTA (down when negative), no further
than the ends; its new index."
    (let* ((specs (the specs-now))
           (j (max 0 (min (1- (length specs)) (+ i delta))))
           (spec (nth i specs))
           (rest (append (subseq specs 0 i) (subseq specs (1+ i)))))
      (the (replace-specs! (append (subseq rest 0 j) (list spec) (subseq rest j))))
      j))

   (duplicate-layer!
    (i)
    "A copy of layer I just above it, with its own copy of any painted
pixels; the copy's index."
    (let* ((spec (the (layer i) spec-now))
           (copy (plist-merge (list :name (format nil "~a copy" (getf spec :name))
                                    :pixels (and (getf spec :pixels) (copy-seq (getf spec :pixels))))
                              spec)))
      (the (add-layer! copy i))))

   (render-cpu
    ()
    "RGBA8 pixels of the whole document, composited by the compiled kernel."
    (let ((out (make-rgba (the doc-width) (the doc-height)))
          (bg (the background-linear)))
      (run-kernel (the cpu-kernel) (* (the doc-width) (the doc-height))
                  (the raster-vector) (the mask-vector) (the lut-vector) (the opacity-vector)
                  (the params-vector)
                  (first bg) (second bg) (third bg) (float (the background-alpha) 1d0)
                  out)
      out))))
