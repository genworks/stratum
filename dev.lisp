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
(dolist (name '(:trivial-gray-streams :flexi-streams :cffi))
  (when (and (find-package name) (not (asdf:find-system name nil)))
    (asdf:register-preloaded-system name)))

(pushnew (make-pathname :name nil :type nil :defaults *load-truename*)
         asdf:*central-registry* :test #'equal)

;; Two of Gendl's gwl files (2026-11-04) an image built before them
;; does not carry: the invite gate and sexp-agent (the prompt box's
;; agent).  From the image when it has them as Stratum needs them --
;; the last name of each entry is a function the image's copy must
;; have, when there is one: sexp-agent learned to send pictures back to
;; the model, to say when it declines and to keep a request within its
;; limit, after the first images
;; carried it -- else the one file
;; from a Gendl checkout beside this one, or wherever INVITE_GATE_FILE
;; and SEXP_AGENT_FILE point.  An image with an older copy and no file
;; to be found keeps its copy.  A third, visitor-log (VISITOR_LOG_FILE),
;; has what the admin page is built from.
(dolist (need '((:invite-gate "INVITE_GATE_FILE" "invite-gate" nil)
                (:visitor-log "VISITOR_LOG_FILE" "visitor-log" "ADMIN-PAGE")
                (:sexp-agent "SEXP_AGENT_FILE" "sexp-agent" "TRIMMED-PICTURES")))
  (destructuring-bind (package variable name proof) need
    (unless (and (find-package package)
                 (or (null proof)
                     (let ((symbol (find-symbol proof package)))
                       (and symbol (fboundp symbol)))))
      (let ((file (or (uiop:getenv variable)
                      (probe-file (format nil "/projects/gw/gendl/gwl/source/~a.lisp" name))
                      (probe-file (merge-pathnames (format nil "../gendl/gwl/source/~a.lisp" name) *load-truename*)))))
        (cond (file (load file))
              ((find-package package))
              (t (error "Stratum needs ~a (gendl gwl/source/~a.lisp): set ~a." package name variable)))))))

(asdf:load-system :stratum)

(funcall (intern "PUBLISH-STRATUM!" :stratum))
