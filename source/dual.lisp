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
;; One definition, two runtimes.  A dual function is an ordinary Lisp
;; function, and the same form handed to Parenscript, so the browser
;; computes a curve exactly as the server does with no second copy to
;; drift.  The body keeps to what both sides read the same way: float
;; arithmetic, AREF and LENGTH on vectors, LET*, DOTIMES, LOOP, COND,
;; and NEW-VECTOR in place of MAKE-ARRAY (Parenscript's MAKE-ARRAY
;; leaks globals).  No docstrings in the body.  Callers hand the Lisp
;; side double-floats, which is what a JavaScript number is.
;;

(defvar *dual-functions* nil
  "Alist of (name . Parenscript defun form), newest first.")

(defun new-vector (n)
  (make-array n :initial-element 0d0))

(defparameter *dual-js-prelude*
  "function newVector(n) { var a = new Array(n); for (var i = 0; i < n; i++) a[i] = 0; return a; }
")

(defmacro define-dual-function (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list ,@body)
     (setf *dual-functions*
           (acons ',name '(ps:defun ,name ,lambda-list ,@body)
                  (remove ',name *dual-functions* :key #'car)))
     ',name))

(defun dual-functions-js ()
  "The JavaScript for every dual function, in definition order."
  (with-output-to-string (s)
    (write-string *dual-js-prelude* s)
    (dolist (entry (reverse *dual-functions*))
      (write-string (ps:ps* (cdr entry)) s)
      (terpri s))))
