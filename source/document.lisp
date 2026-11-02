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
  (:description "One layer of an image-document, described by a spec
plist.  :kind :raster takes :source (a generator spec) and :blend;
:kind :curves takes :master, :red, :green and :blue point lists.
Either kind takes :name, :opacity, :visible? and :mask (a mask
generator spec).")

  :input-slots
  (spec doc-width doc-height
   (kind (getf (the spec) :kind :raster))
   (layer-name (getf (the spec) :name (format nil "Layer ~a" (1+ (the index)))))
   (opacity (getf (the spec) :opacity 1.0) :settable)
   (visible? (getf (the spec) :visible? t) :settable)
   (blend-mode (getf (the spec) :blend :normal) :settable)
   (source (getf (the spec) :source))
   (mask-spec (getf (the spec) :mask) :settable)
   (master (getf (the spec) :master *identity-curve*) :settable)
   (red (getf (the spec) :red *identity-curve*) :settable)
   (green (getf (the spec) :green *identity-curve*) :settable)
   (blue (getf (the spec) :blue *identity-curve*) :settable))

  :computed-slots
  ((has-mask? (and (the mask-spec) t))
   (pixels (when (eql (the kind) :raster)
             (generate-pixels (the source) (the doc-width) (the doc-height))))
   (mask-pixels (when (the has-mask?)
                  (generate-mask (the mask-spec) (the doc-width) (the doc-height))))
   (effective-opacity (if (the visible?) (coerce (the opacity) 'double-float) 0.0))
   (texture-index (position self (the parent raster-layers)))
   (mask-index (position self (the parent masked-layers)))
   (lut-row (let ((p (position self (the parent curves-layers))))
              (when p (* 3 p))))
   (luts (when (eql (the kind) :curves)
           (list (channel-lut (the master) (the red))
                 (channel-lut (the master) (the green))
                 (channel-lut (the master) (the blue)))))
   (plan-entry
    (ecase (the kind)
      (:raster (list :raster :tex (the texture-index) :blend (the blend-mode)
                     :mask (the mask-index) :param (the index)))
      (:curves (list :curves :lut (the lut-row)
                     :mask (the mask-index) :param (the index)))))))

(define-object image-document ()
  :documentation
  (:description "A layered raster document.  LAYER-SPECS lists the
layers bottom first; see image-layer for a spec's keys.")

  :input-slots
  ((doc-name "untitled")
   (doc-width 1024)
   (doc-height 768)
   (layer-specs nil :settable)
   (background-srgb (list 1.0 1.0 1.0) :settable))

  :objects
  ((layers :type 'image-layer
           :sequence (:size (length (the layer-specs)))
           :spec (nth (the-child index) (the layer-specs))
           :doc-width (the doc-width)
           :doc-height (the doc-height)))

  :computed-slots
  ((layer-list (list-elements (the layers)))
   (raster-layers (remove-if-not (lambda (l) (eql (the-object l kind) :raster))
                                 (the layer-list)))
   (curves-layers (remove-if-not (lambda (l) (eql (the-object l kind) :curves))
                                 (the layer-list)))
   (masked-layers (remove-if-not (lambda (l) (the-object l has-mask?))
                                 (the layer-list)))
   (lut-rows (* 3 (length (the curves-layers))))

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
                 (dolist (l (the curves-layers) v)
                   (loop for lut in (the-object l luts)
                         for row from (the-object l lut-row)
                         do (replace v lut :start1 (* 256 row))))))
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
      (funcall (the cpu-kernel)
               (the raster-vector) (the mask-vector) (the lut-vector) (the opacity-vector)
               (first bg) (second bg) (third bg)
               out (the doc-width) (the doc-height))))))
