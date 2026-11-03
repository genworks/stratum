;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(gwl:define-package :stratum
    (:export #:image-document #:image-layer #:raster-layer #:curves-layer
             #:make-sample-document #:find-document
             #:png-octets #:read-png #:read-png-file #:open-png-file #:publish-stratum!
             #:define-dual-function #:dual-functions-js
             #:plan->glsl #:plan->lisp
             #:place-dabs #:paint-dab #:apply-brush-stroke #:undo-stroke!))
