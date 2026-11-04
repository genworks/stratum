;;;; -*- Mode: Lisp; Package: gdl-user -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; ci/run-tests.lisp -- the Lisp half of Stratum's regression suite on
;;;; a COLD Gendl image, one that has never seen this repository.
;;;;
;;;; Run by .gitlab-ci.yml inside the published genworks/gendl images,
;;;; once on CCL and once on SBCL.  It loads Quicklisp from the image,
;;;; loads Stratum through ASDF from the checkout (any WARNING while it
;;;; loads fails the job: a warm image cannot catch a file out of order
;;;; or a dependency nobody declared), then runs tests/run.lisp.  Exit
;;;; status: 0 every check passed, 1 a check failed or Stratum loaded
;;;; with warnings, 2 something else went wrong.
;;;;
;;;; Locally, with STRATUM_DIR naming a checkout:
;;;;   cd /opt/gendl && STRATUM_DIR=/path/to/stratum \
;;;;     ./gdl/program/gdl-ccl -n -b --load /path/to/stratum/ci/run-tests.lisp

(in-package :gdl-user)

(defun ci-exit (code control &rest args)
  (finish-output *standard-output*)
  (format *error-output* "~&~?~%" control args)
  (finish-output *error-output*)
  (uiop:quit code))

;;; Quicklisp first: the QL package must exist before any form below
;;; naming it is READ.
(handler-case (load-quicklisp)
  (error (e) (ci-exit 2 "CI: could not load Quicklisp: ~a" e)))

(defparameter *stratum-dir*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "STRATUM_DIR") (uiop:getenv "CI_PROJECT_DIR")
       (ci-exit 2 "CI: set STRATUM_DIR or CI_PROJECT_DIR to the stratum checkout."))))

;;; The Gendl images carry these without ASDF knowing; loading them
;;; again from the dist over the live packages would only make noise.
(dolist (name '(:trivial-gray-streams :flexi-streams))
  (when (find-package name)
    (asdf:register-preloaded-system name)))

;;; The invite gate (Gendl's gwl/source/invite-gate.lisp): from the
;;; image when it carries it, else the file INVITE_GATE_FILE names
;;; (.gitlab-ci.yml fetches it from the Gendl repository).
(handler-case
    (unless (find-package :invite-gate)
      (let ((file (or (uiop:getenv "INVITE_GATE_FILE")
                      (probe-file "/projects/gw/gendl/gwl/source/invite-gate.lisp")
                      (ci-exit 2 "CI: no invite-gate in the image; set INVITE_GATE_FILE."))))
        (load file)))
  (error (e) (ci-exit 2 "CI: invite-gate did not load: ~a" e)))

(handler-case
    (let ((warnings '()))
      (ql:quickload '(:zpng :chipz) :silent t)
      (pushnew *stratum-dir* asdf:*central-registry* :test #'equal)
      (handler-bind ((warning (lambda (w)
                                (unless (typep w 'style-warning)
                                  (push (princ-to-string w) warnings)))))
        (asdf:load-system :stratum))
      (when warnings
        (ci-exit 1 "CI: ~d warning~:p while Stratum loaded:~%~{  ~a~%~}"
                 (length warnings) (reverse warnings))))
  (error (e) (ci-exit 2 "CI: Stratum did not load: ~a" e)))

(handler-case (load (merge-pathnames "tests/run.lisp" *stratum-dir*))
  (error (e) (ci-exit 2 "CI: tests/run.lisp did not load: ~a" e)))

(let ((passed (handler-case (funcall (find-symbol "RUN-TESTS" :stratum))
                (error (e) (ci-exit 2 "CI: run-tests failed outright: ~a" e)))))
  (ci-exit (if passed 0 1) "CI: ~:[some checks FAILED~;every check passed~]." passed))
