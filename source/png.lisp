;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

(defun png-octets (rgba width height)
  "RGBA, straight-alpha sRGB bytes, encoded as a PNG in memory."
  (let ((png (make-instance 'zpng:png :color-type :truecolor-alpha
                                      :width width :height height
                                      :image-data rgba)))
    (flexi-streams:with-output-to-sequence (s)
      (zpng:write-png-stream png s))))
