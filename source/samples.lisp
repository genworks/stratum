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
;; Sample documents, built from procedural layers.  "demo" is the
;; everyday one; "parity" is the same stack small enough to compare the
;; GPU against the CPU pixel by pixel; "stress" is the spike's success
;; test, 20 layers on a 4K canvas.
;;

(defparameter *demo-layers*
  '((:kind :raster :name "Sky"
     :source (:gradient :from (34 58 112 255) :to (247 179 107 255) :angle 90))
    (:kind :raster :name "Clouds" :blend :screen :opacity 0.8
     :source (:clouds :seed 7 :color (255 248 236) :threshold 0.52 :alpha 235))
    (:kind :curves :name "Contrast"
     :master ((0 0) (0.25 0.18) (0.75 0.85) (1 1)))
    (:kind :levels :name "Levels" :in-black 0.04 :in-white 0.97 :gamma 1.15)
    (:kind :raster :name "Discs"
     :source (:discs :count 18 :seed 3)
     :mask (:radial :cx 0.5 :cy 0.55 :inner 0.15 :outer 0.55))
    (:kind :raster :name "Stripes" :blend :multiply :opacity 0.5
     :source (:stripes :period 48 :angle 30 :color (30 30 44) :alpha 150)
     :mask (:linear :angle 0 :from 0.35 :to 0.75))
    (:kind :raster :name "Glow" :blend :add :opacity 0.6
     :source (:discs :count 6 :seed 11 :softness 0.9 :alpha 200 :palette ((255 210 120))))
    (:kind :hue-saturation :name "Hue/Saturation" :hue 14 :saturation 0.25 :lightness -0.04
     :mask (:radial :cx 0.5 :cy 0.5 :inner 0.25 :outer 0.75 :invert? t))
    (:kind :curves :name "Warm" :opacity 0.9
     :red ((0 0) (0.5 0.56) (1 1))
     :blue ((0 0) (0.5 0.44) (1 1))
     :mask (:linear :angle 90 :from 0.0 :to 1.0))))

(defun stress-layers ()
  "Twenty layers, ten raster and ten curves, over five distinct sources
(the pixel cache keeps one copy of each), every blend mode, and masks
on a third of them."
  (let ((sources '((:gradient :from (20 30 60 255) :to (230 120 80 255) :angle 60)
                   (:clouds :seed 5 :color (255 255 255) :threshold 0.5 :alpha 220)
                   (:discs :count 30 :seed 9)
                   (:stripes :period 64 :angle 15 :color (10 10 20) :alpha 120)
                   (:clouds :seed 21 :scale 0.01 :color (120 200 255) :threshold 0.55 :alpha 200)))
        (blends '(:normal :screen :normal :multiply :add))
        (masks '(nil (:radial :inner 0.1 :outer 0.7) nil))
        (layers '()))
    (dotimes (i 10 (nreverse layers))
      (push (list :kind :raster :name (format nil "Raster ~d" (1+ i))
                  :source (nth (mod i 5) sources)
                  :blend (if (zerop i) :normal (nth (mod i 5) blends))
                  :opacity (if (zerop i) 1.0 0.55)
                  :mask (nth (mod i 3) masks))
            layers)
      (push (list :kind :curves :name (format nil "Curves ~d" (1+ i))
                  :opacity 0.5
                  :master `((0 0) (0.3 ,(+ 0.25 (* 0.01 i))) (0.7 ,(- 0.75 (* 0.01 i))) (1 1))
                  :mask (nth (mod (1+ i) 3) masks))
            layers))))

(defparameter *recipes*
  '(("demo" :width 1920 :height 1080 :layers :demo)
    ("parity" :width 320 :height 200 :layers :demo)
    ("stress" :width 3840 :height 2160 :layers :stress)))

(defun make-sample-document (name)
  (let ((recipe (rest (assoc name *recipes* :test #'string=))))
    (unless recipe (error "No sample document named ~s." name))
    (destructuring-bind (&key width height layers) recipe
      (make-object 'image-document
                   :doc-name name
                   :doc-width width
                   :doc-height height
                   :layer-specs (ecase layers
                                  (:demo (copy-tree *demo-layers*))
                                  (:stress (stress-layers)))))))
