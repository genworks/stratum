;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;; The History Brush paints the document as it was -- the composite of
;;; every layer at the history source -- back onto the current layer,
;;; through the brush.  The source is the document as opened, unless
;;; Set Source has chosen the current state since.  Keeping the source
;;; costs nothing until it is painted from: the specs the document was
;;; made of still hold the opened pixels (a layer copies them before
;;; its first stroke), so the source is those specs, and its composite
;;; is rendered once, when first wanted, by composite-specs
;;; (merge.lisp).  A crop or a resize makes a new document, whose
;;; source is the cropped state.

(defun ensure-history-specs! (doc)
  "DOC's history source, taken now if it has none yet: the layer specs
as they stand.  Called at every request, so the first request's state,
the document as opened, is what sticks."
  (or (the-object doc history-specs)
      (let ((specs (the-object doc layer-specs)))
        (the-object doc (set-slot! :history-specs specs))
        specs)))

(defun history-pixels (doc)
  "The composite of DOC's history source, RGBA8 straight alpha over
transparency, rendered when first wanted and kept."
  (or (the-object doc history-pixels)
      (let ((pixels (composite-specs doc (ensure-history-specs! doc))))
        (the-object doc (set-slot! :history-pixels pixels))
        pixels)))

(defun set-history! (doc)
  "Set the history source to DOC's current state: every layer's pixels
and mask copied into the specs, the kept composite dropped; the new
history version."
  (let ((specs (mapcar (lambda (spec)
                         (let ((spec (copy-list spec)))
                           (dolist (key '(:pixels :mask-pixels) spec)
                             (when (getf spec key)
                               (setf (getf spec key) (copy-seq (getf spec key)))))))
                       (materialized-specs doc))))
    (let ((version (1+ (the-object doc history-version))))
      (the-object doc (set-slot! :history-specs specs))
      (the-object doc (set-slot! :history-pixels nil))
      (the-object doc (set-slot! :history-version version))
      version)))
