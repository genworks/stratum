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
;; Parenscript prints (* a (* b c)) as a * b * c, which groups from the
;; left, and floats do not associate: at 0.1, 0.2, 0.3 the two groupings
;; differ in the last bit.  NORMALIZE-DUAL flattens a product or a sum
;; nested to the right of another before either side sees the body, as
;; parenscript-dual and Tala do.  No body here nests one today; the next
;; one that does would otherwise differ, and only the stroke check
;; (exact) would see it.
;;
;; LEVELS-LUT calls EXPT, which is Math.pow in the browser, and pow
;; need not match the server's libm in the last bit (node 18 differs
;; from SBCL in 30 of 256 entries of a gamma-1.2 table, one unit in the
;; last place each; node 24 does not).  So the levels tables agree
;; within a tolerance, which the self-test states (*levels-tolerance*
;; in viewer.js), while the curves tables, all + - * / sqrt and
;; comparisons, agree to the last bit.
;;

(defvar *dual-functions* nil
  "Alist of (name . Parenscript defun form), newest first.")

(defun normalize-dual (form)
  "FORM with a product or sum nested to the right of another flattened,
(* a (* b c)) to (* a b c), at every depth: the left-to-right grouping
Parenscript prints, made the Lisp grouping too."
  (cond ((atom form) form)
        ((member (car form) '(* +))
         (cons (car form)
               (loop for arg in (cdr form)
                     for first? = t then nil
                     for inner = (normalize-dual arg)
                     if (and (not first?) (consp inner) (eq (car inner) (car form)))
                       append (cdr inner)
                     else collect inner)))
        (t (mapcar #'normalize-dual form))))

(defun new-vector (n)
  (make-array n :initial-element 0d0))

(defparameter *dual-js-prelude*
  "function newVector(n) { var a = new Array(n); for (var i = 0; i < n; i++) a[i] = 0; return a; }
")

(defmacro define-dual-function (name lambda-list &body body)
  "NAME as a Lisp function and the same form for Parenscript, both from
BODY through NORMALIZE-DUAL."
  (let ((body (mapcar #'normalize-dual body)))
    `(progn
       (defun ,name ,lambda-list ,@body)
       (setf *dual-functions*
             (acons ',name '(ps:defun ,name ,lambda-list ,@body)
                    (remove ',name *dual-functions* :key #'car)))
       ',name)))

(defun dual-functions-js ()
  "The JavaScript for every dual function, in definition order."
  (with-output-to-string (s)
    (write-string *dual-js-prelude* s)
    (dolist (entry (reverse *dual-functions*))
      (write-string (ps:ps* (cdr entry)) s)
      (terpri s))))
