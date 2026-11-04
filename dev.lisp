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

;; The invite gate (Gendl's gwl/source/invite-gate.lisp, 2026-11-04):
;; from the image when it carries it, else the one file from a Gendl
;; checkout beside this one, or wherever INVITE_GATE_FILE points.
(unless (find-package :invite-gate)
  (let ((file (or (uiop:getenv "INVITE_GATE_FILE")
                  (probe-file "/projects/gw/gendl/gwl/source/invite-gate.lisp")
                  (probe-file (merge-pathnames "../gendl/gwl/source/invite-gate.lisp" *load-truename*))
                  (error "Stratum needs invite-gate (gendl gwl/source/invite-gate.lisp): set INVITE_GATE_FILE."))))
    (load file)))

(asdf:load-system :stratum)

(funcall (intern "PUBLISH-STRATUM!" :stratum))
