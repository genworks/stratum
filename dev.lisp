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

;; Two of Gendl's gwl files (2026-11-04) an image built before them
;; does not carry: the invite gate and sexp-agent (the prompt box's
;; agent).  From the image when it has them, else the one file from a
;; Gendl checkout beside this one, or wherever INVITE_GATE_FILE and
;; SEXP_AGENT_FILE point.
(dolist (need '((:invite-gate "INVITE_GATE_FILE" "invite-gate")
                (:sexp-agent "SEXP_AGENT_FILE" "sexp-agent")))
  (destructuring-bind (package variable name) need
    (unless (find-package package)
      (let ((file (or (uiop:getenv variable)
                      (probe-file (format nil "/projects/gw/gendl/gwl/source/~a.lisp" name))
                      (probe-file (merge-pathnames (format nil "../gendl/gwl/source/~a.lisp" name) *load-truename*))
                      (error "Stratum needs ~a (gendl gwl/source/~a.lisp): set ~a." package name variable))))
        (load file)))))

(asdf:load-system :stratum)

(funcall (intern "PUBLISH-STRATUM!" :stratum))
