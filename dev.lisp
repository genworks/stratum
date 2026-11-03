;;;; -*- Mode: Lisp; Package: cl-user -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; Load Stratum into a running Gendl image and publish it:
;;;
;;;   (load "/projects/gw/stratum/dev.lisp")
;;;
;;; then browse /stratum/ on that image's HTTP port (?doc=demo,
;;; ?doc=parity, ?doc=stress).  zpng, salza2 and chipz come from the
;;; genworks/quicklisp checkout when the image does not carry them.
;;;

(in-package :cl-user)

(let ((software #p"/projects/gw/quicklisp/dists/quicklisp/software/"))
  (dolist (pattern '("zpng-*/" "salza2-*/" "chipz-*/"))
    (dolist (dir (directory (merge-pathnames pattern software)))
      (pushnew dir asdf:*central-registry* :test #'equal))))

;; Built into the Gendl images without ASDF knowing: say so, or salza2
;; and stratum go looking for them.
(dolist (name '(:trivial-gray-streams :flexi-streams))
  (when (and (find-package name) (not (asdf:find-system name nil)))
    (asdf:register-preloaded-system name)))

(pushnew (make-pathname :name nil :type nil :defaults *load-truename*)
         asdf:*central-registry* :test #'equal)

(asdf:load-system :stratum)

(funcall (intern "PUBLISH-STRATUM!" :stratum))
