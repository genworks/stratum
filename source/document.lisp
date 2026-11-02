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
   (mask-spec (getf (the spec) :mask) :settable))

  :computed-slots
  ((has-mask? (and (the mask-spec) t))
   (mask-pixels (when (the has-mask?)
                  (generate-mask (the mask-spec) (the doc-width) (the doc-height))))
   (effective-opacity (if (the visible?) (coerce (the opacity) 'double-float) 0.0))
   (mask-index (position self (the parent masked-layers)))
   ("List of keywords. The numeric settings the page shows as sliders and
may set; each is a settable slot of this layer."
    setting-names nil)
   ("List of four numbers. This layer's entry in the shader's u_params."
    params (list 0 0 0 0))))

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
   (source (getf (the spec) :source))
   ("Vector of (unsigned-byte 8), or nil.  This layer's own pixel buffer,
a copy of the source made by its first stroke; nil until then, so an
unpainted layer shares the pixel cache's copy and never paints into it."
    own-pixels nil :settable)
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
                     :mask (the mask-index) :param (the index))))

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
               (channel-lut (the master) (the blue))))))

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
   ("List.  What the last strokes overwrote, newest first: (layer-index x
y w h rgba-bytes) each, at most *undo-depth* of them.  undo-stroke! in
brush.lisp pops one and puts the bytes back."
    undo-entries nil :settable))

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

   (render-cpu
    ()
    "RGBA8 pixels of the whole document, composited by the compiled kernel."
    (let ((out (make-rgba (the doc-width) (the doc-height)))
          (bg (the background-linear)))
      (run-kernel (the cpu-kernel) (* (the doc-width) (the doc-height))
                  (the raster-vector) (the mask-vector) (the lut-vector) (the opacity-vector)
                  (the params-vector)
                  (first bg) (second bg) (third bg)
                  out)
      out))))
