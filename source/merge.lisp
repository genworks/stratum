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
;; Layer > Merge Down and Flatten Image: the compositing the document
;; already does, pointed at part of the stack.  Merge Down renders the
;; active layer and the one below it, with their opacities, blend modes
;; and masks, over transparency (a document of the two with its
;; background alpha 0) into one raster layer that takes the lower
;; layer's place and name, at full opacity, normal, without a mask.
;; Flatten renders the whole document over its background into one
;; Background layer.  Both change the stack, so the undo and redo lists
;; go with it.
;;

(defun composite-specs (doc specs &key (alpha 0))
  "SPECS composited as a stack of DOC's size over DOC's background at
ALPHA: RGBA8 straight alpha."
  (the-object (make-object 'image-document
                           :doc-name "merge"
                           :doc-width (the-object doc doc-width)
                           :doc-height (the-object doc doc-height)
                           :background-srgb (the-object doc background-srgb)
                           :background-alpha alpha
                           :layer-specs specs)
              (render-cpu)))

(defun merge-down! (doc i)
  "Merge layer I into the raster layer below it; the merged layer's
index."
  (let ((layer (the-object doc (layer i)))
        (below (and (> i 0) (the-object doc (layer (1- i))))))
    (unless below (error "~a has nothing below it." (the-object layer layer-name)))
    (unless (eql (the-object below kind) :raster)
      (error "~a is not a raster layer to merge into." (the-object below layer-name)))
    (let* ((specs (the-object doc specs-now))
           (merged (composite-specs doc (list (nth (1- i) specs) (nth i specs))))
           (spec (list :kind :raster :name (the-object below layer-name)
                       :pixels merged :source '(:solid)
                       :visible? (the-object below visible?))))
      (the-object doc (replace-specs! (append (subseq specs 0 (1- i)) (list spec) (subseq specs (1+ i)))))
      (1- i))))

(defun flatten! (doc)
  "Flatten the document into one Background layer; its index, 0."
  (let ((pixels (the-object doc (render-cpu))))
    (the-object doc (replace-specs! (list (list :kind :raster :name "Background"
                                                :pixels pixels :source '(:solid)))))
    0))
