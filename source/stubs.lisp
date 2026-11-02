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
;; Open pieces.  Each stub states the contract its implementation must
;; meet and signals NOT-YET until then.  README.md lists them with
;; their acceptance tests.  The brush engine was the first of these;
;; it is brush.lisp now.
;;

(define-condition not-yet (error)
  ((what :initarg :what :reader not-yet-what))
  (:report (lambda (c s) (format s "Not built yet: ~a." (not-yet-what c)))))

(defun make-tile-store (width height &key (tile-size 256))
  "Pixel storage for one layer as a grid of TILE-SIZE square tiles.

The contract: tiles are copy-on-write and shared between document
versions, so undo is a list of versions and costs only the tiles each
edit touched; tiles live in foreign memory (static-vectors) so the
garbage collector never moves them; a cold tile can be written out and
read back.  The GPU side keeps a resident set per zoom level (virtual
texturing) once a document outgrows the texture array, which a 4K
document of more than about twelve raster layers does on most GPUs."
  (declare (ignore width height tile-size))
  (error 'not-yet :what "tiled layer storage (make-tile-store)"))

(defun convert-with-profile (rgba width height &key from to)
  "RGBA converted between ICC profiles FROM and TO (pathnames or
:srgb), through LittleCMS over CFFI.  Today every document is sRGB
and this is the identity."
  (declare (ignore width height))
  (if (and (member from '(nil :srgb)) (member to '(nil :srgb)))
      rgba
      (error 'not-yet :what "ICC color management (convert-with-profile)")))
