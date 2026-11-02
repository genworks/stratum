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
;; their acceptance tests.
;;

(define-condition not-yet (error)
  ((what :initarg :what :reader not-yet-what))
  (:report (lambda (c s) (format s "Not built yet: ~a." (not-yet-what c)))))

(defun apply-brush-stroke (layer stroke)
  "Paint STROKE into raster LAYER and return the dirty rectangle
(x y w h).  STROKE is a plist: :points, a vector of (x y pressure tilt
time) samples in document pixels; :radius, :hardness, :spacing (a
fraction of the radius), :flow, :color (sRGB 0-255) and :mode (:paint
or :erase).

The contract: dabs are placed along the stroke at the given spacing and
composited in linear light, as the shader composites layers; the result
is the same whether the viewer painted the stroke locally first or not,
because the viewer's preview runs the same dab code (a dual function or
a generated shader) and the server's copy is the one kept.  Pixels move
in tiles (see tile-store): the stroke returns only the tiles it
touched, so the browser uploads those tiles alone, and undo keeps the
tiles' previous versions."
  (declare (ignore layer stroke))
  (error 'not-yet :what "the brush engine (apply-brush-stroke)"))

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
