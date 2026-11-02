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
;; Curves: monotone cubic interpolation through the control points
;; (Fritsch and Carlson, 1980), so a curve never overshoots between its
;; points, which an adjustment curve must not do.  Points are (x y) in
;; [0,1] with x strictly increasing.  The three curve functions are
;; dual: the viewer runs them in the browser while a point is dragged,
;; and the CPU kernel's tables come from the same definitions.
;;

(define-dual-function curve-tangents (xs ys)
  (let* ((n (length xs))
         (d (new-vector n))
         (m (new-vector n)))
    (dotimes (k (- n 1))
      (setf (aref d k) (/ (- (aref ys (+ k 1)) (aref ys k))
                          (- (aref xs (+ k 1)) (aref xs k)))))
    (setf (aref m 0) (aref d 0))
    (setf (aref m (- n 1)) (aref d (- n 2)))
    (loop for k from 1 below (- n 1)
          do (setf (aref m k)
                   (if (<= (* (aref d (- k 1)) (aref d k)) 0)
                       0
                       (/ (+ (aref d (- k 1)) (aref d k)) 2))))
    (dotimes (k (- n 1))
      (if (= (aref d k) 0)
          (progn (setf (aref m k) 0)
                 (setf (aref m (+ k 1)) 0))
          (let* ((a (/ (aref m k) (aref d k)))
                 (b (/ (aref m (+ k 1)) (aref d k)))
                 (s (+ (* a a) (* b b))))
            (when (> s 9)
              (let* ((tau (/ 3 (sqrt s))))
                (setf (aref m k) (* tau a (aref d k)))
                (setf (aref m (+ k 1)) (* tau b (aref d k))))))))
    m))

(define-dual-function curve-eval (xs ys ms x)
  (let* ((n (length xs))
         (top (- n 1)))
    (cond ((<= x (aref xs 0)) (aref ys 0))
          ((>= x (aref xs top)) (aref ys top))
          (t (let* ((k 0))
               (loop while (> x (aref xs (+ k 1))) do (setf k (+ k 1)))
               (let* ((h (- (aref xs (+ k 1)) (aref xs k)))
                      (u (/ (- x (aref xs k)) h))
                      (u2 (* u u))
                      (u3 (* u2 u)))
                 (+ (* (+ (- (* 2 u3) (* 3 u2)) 1) (aref ys k))
                    (* (+ (- u3 (* 2 u2)) u) h (aref ms k))
                    (* (+ (* -2 u3) (* 3 u2)) (aref ys (+ k 1)))
                    (* (- u3 u2) h (aref ms (+ k 1))))))))))

(define-dual-function composed-lut (mxs mys cxs cys size)
  (let* ((mm (curve-tangents mxs mys))
         (cm (curve-tangents cxs cys))
         (lut (new-vector size))
         (step (/ 1.0d0 (- size 1))))
    (dotimes (i size)
      (let* ((x (* i step))
             (y (min 1 (max 0 (curve-eval mxs mys mm x)))))
        (setf (aref lut i) (min 1 (max 0 (curve-eval cxs cys cm y))))))
    lut))

;;; Levels as a table: input black and white points, gamma (above 1
;;; lifts the midtones), output black and white points, on the encoded
;;; [0,1] scale as other editors work.  Dual, like the curves.
(define-dual-function levels-lut (in-black in-white gamma out-black out-white size)
  (let* ((lut (new-vector size))
         (step (/ 1.0d0 (- size 1)))
         (span (max 1.0d-6 (- in-white in-black))))
    (dotimes (i size)
      (let* ((x (* i step))
             (v (min 1 (max 0 (/ (- x in-black) span))))
             (g (expt v (/ 1 gamma))))
        (setf (aref lut i) (min 1 (max 0 (+ out-black (* g (- out-white out-black))))))))
    lut))

(defparameter *identity-curve* '((0 0) (1 1)))

(defun normalize-points (points)
  "POINTS clamped to [0,1], sorted by x, one point per x; the identity
curve when fewer than two remain."
  (let* ((clamped (mapcar (lambda (p)
                            (list (max 0d0 (min 1d0 (coerce (first p) 'double-float)))
                                  (max 0d0 (min 1d0 (coerce (second p) 'double-float)))))
                          points))
         (unique (remove-duplicates (sort clamped #'< :key #'first)
                                    :key #'first :test #'= :from-end t)))
    (if (< (length unique) 2) *identity-curve* unique)))

(defun points->vectors (points)
  (let ((points (normalize-points points)))
    (values (map 'vector (lambda (p) (coerce (first p) 'double-float)) points)
            (map 'vector (lambda (p) (coerce (second p) 'double-float)) points))))

(defun channel-lut (master channel &optional (size 256))
  "Double-float table of CHANNEL's curve applied after MASTER's."
  (multiple-value-bind (mx my) (points->vectors master)
    (multiple-value-bind (cx cy) (points->vectors channel)
      (map '(simple-array double-float (*))
           (lambda (v) (coerce v 'double-float))
           (composed-lut mx my cx cy size)))))
