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
;; Stratum documents as OpenRaster files and back (File > Open of an
;; .ora, File > Save As OpenRaster).  OpenRaster has raster layers with
;; an opacity, a visibility and a blend mode, and nothing else Stratum
;; has, so:
;;
;; - A raster layer goes out as its pixels, its mask baked into the
;;   alpha (a layer under a black mask is transparent in GIMP, as it is
;;   here); :normal, :multiply, :screen and :add map onto svg:src-over,
;;   svg:multiply, svg:screen, svg:plus and svg:color.
;; - An adjustment layer (curves, levels, hue/saturation) has no
;;   OpenRaster shape.  It goes out as a hidden, empty 1 x 1 layer that
;;   keeps its name, with Stratum's own attribute `stratum:spec' holding
;;   the layer's spec and `stratum:mask' naming its mask's PNG under
;;   data/; GIMP shows a hidden empty layer, and Stratum reads the
;;   adjustment back whole.  The merged image is the full render, so a
;;   viewer sees the adjusted picture either way.
;; - Coming in, a layer at an offset is placed on the canvas, a nested
;;   stack is flattened (ora.lisp), and a blend mode Stratum has not got
;;   becomes :normal and is reported.
;;

(defparameter *ora-blend-ops*
  '((:normal . "svg:src-over") (:multiply . "svg:multiply")
    (:screen . "svg:screen") (:add . "svg:plus") (:color . "svg:color")))

(defparameter *stratum-xmlns*
  '("stratum" . "https://github.com/genworks/stratum")
  "The namespace of Stratum's attributes in stack.xml.")

(defparameter *layer-kinds* '(:raster :curves :levels :hue-saturation))

(defparameter *empty-png* (png-octets (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0) 1 1)
  "A transparent 1 x 1 PNG: the placeholder of an adjustment layer.")

(defun masked-rgba (pixels mask)
  "PIXELS with MASK (one byte a pixel) multiplied into the alpha."
  (let ((out (copy-seq pixels)))
    (dotimes (i (length mask) out)
      (let ((a (+ 3 (* 4 i))))
        (setf (aref out a) (round (* (aref out a) (aref mask i)) 255))))))

(defun gray-png-octets (gray w h)
  "GRAY (one byte a pixel) as an opaque gray PNG."
  (let ((rgba (make-array (* 4 w h) :element-type '(unsigned-byte 8) :initial-element 255)))
    (dotimes (i (* w h) (png-octets rgba w h))
      (let ((v (aref gray i)) (o (* 4 i)))
        (setf (aref rgba o) v (aref rgba (+ o 1)) v (aref rgba (+ o 2)) v)))))

(defun png-gray-octets (png)
  "(values gray width height): the first channel of the PNG."
  (multiple-value-bind (rgba w h) (read-png png)
    (let ((gray (make-array (* w h) :element-type '(unsigned-byte 8))))
      (dotimes (i (* w h) (values gray w h))
        (setf (aref gray i) (aref rgba (* 4 i)))))))

(defun spec-string (spec)
  "SPEC printed to be read back by read-spec-string."
  (with-standard-io-syntax (prin1-to-string spec)))

(defun read-spec-string (string)
  "The layer spec in STRING, read with evaluation off and checked to be
a spec of a known kind."
  (let ((spec (with-standard-io-syntax
                (let ((*read-eval* nil))
                  (read-from-string string)))))
    (unless (and (listp spec) (evenp (length spec)) (member (getf spec :kind) *layer-kinds*))
      (error "Not a Stratum layer spec: ~s" string))
    spec))

(defun document-ora-image (doc)
  "DOC as an ora-image, ready for ora-octets."
  (let ((w (the-object doc doc-width))
        (h (the-object doc doc-height))
        (layers '())
        (files '()))
    (loop for spec in (materialized-specs doc)
          for k from 1
          do (push
              (if (eql (getf spec :kind :raster) :raster)
                  (let ((pixels (getf spec :pixels))
                        (mask (getf spec :mask-pixels)))
                    (make-ora-layer
                     :name (getf spec :name "Layer")
                     :opacity (float (getf spec :opacity 1.0) 1d0)
                     :visible? (getf spec :visible? t)
                     :composite-op (or (cdr (assoc (getf spec :blend :normal) *ora-blend-ops*)) "svg:src-over")
                     :png (png-octets (if mask (masked-rgba pixels mask) pixels) w h)))
                  (let ((spec (copy-list spec))
                        (mask (getf spec :mask-pixels))
                        (attributes '()))
                    (when mask
                      (let ((path (format nil "data/mask~d.png" k)))
                        (push (cons path (gray-png-octets mask w h)) files)
                        (push (cons "stratum:mask" path) attributes)
                        (remf spec :mask-pixels)))
                    (push (cons "stratum:spec" (spec-string spec)) attributes)
                    (make-ora-layer
                     :name (getf spec :name "Adjustment")
                     :visible? nil
                     :png *empty-png*
                     :attributes (nreverse attributes))))
              layers))
    (make-ora-image :width w :height h
                    :layers layers
                    :merged (png-octets (the-object doc (render-cpu)) w h)
                    :files (nreverse files)
                    :namespaces (list *stratum-xmlns*))))

(defun document-ora-octets (doc)
  "DOC as the bytes of an .ora file."
  (ora-octets (document-ora-image doc)))

(defun ora-image-document (image name)
  "(values document unsupported): an image-document named NAME from the
ora-image IMAGE, and a list of (layer-name composite-op) for the
layers whose blend mode became :normal for want of it here."
  (let ((w (ora-image-width image))
        (h (ora-image-height image))
        (specs '())
        (unsupported '()))
    (dolist (layer (ora-image-layers image))
      (let ((spec-string (cdr (assoc "stratum:spec" (ora-layer-attributes layer) :test #'string=)))
            (mask-path (cdr (assoc "stratum:mask" (ora-layer-attributes layer) :test #'string=))))
        (push (if spec-string
                  (let ((spec (read-spec-string spec-string)))
                    (when mask-path
                      (let ((png (ora-file image mask-path)))
                        (unless png (error "OpenRaster: ~a names ~a, which is not in the archive."
                                           (ora-layer-name layer) mask-path))
                        (multiple-value-bind (gray mw mh) (png-gray-octets png)
                          (unless (and (= mw w) (= mh h))
                            (error "OpenRaster: the mask ~a is ~d x ~d on a ~d x ~d image." mask-path mw mh w h))
                          (setf (getf spec :mask-pixels) gray))))
                    spec)
                  (multiple-value-bind (pixels lw lh) (ora-layer-rgba layer)
                    (let ((blend (car (rassoc (ora-layer-composite-op layer) *ora-blend-ops* :test #'string=)))
                          (x (ora-layer-x layer))
                          (y (ora-layer-y layer)))
                      (unless blend
                        (push (list (ora-layer-name layer) (ora-layer-composite-op layer)) unsupported))
                      (list :kind :raster
                            :name (or (ora-layer-name layer) "Layer")
                            :pixels (if (and (= lw w) (= lh h) (zerop x) (zerop y))
                                        pixels
                                        (place-rgba pixels lw lh w h x y))
                            :source '(:solid)
                            :opacity (ora-layer-opacity layer)
                            :visible? (ora-layer-visible? layer)
                            :blend (or blend :normal)))))
              specs)))
    (values (make-object 'image-document
                         :doc-name name
                         :doc-width w
                         :doc-height h
                         :layer-specs specs)
            (nreverse unsupported))))

(defun read-ora-document (input name)
  "(values document unsupported): the .ora file INPUT (octets or a path)
as an image-document named NAME."
  (ora-image-document (read-ora input) name))

(defun open-ora-file (path &optional name)
  "File > Open from Lisp: the .ora file at PATH as a new document, NAME
or the file's own name made unique, registered to be served; the
document."
  (let ((name (unique-document-name (or name (pathname-name path) "untitled"))))
    (setf (gethash name *documents*) (read-ora-document path name))))

(defun save-ora-file (doc path)
  "DOC written as the .ora file at PATH; PATH."
  (write-ora-file (document-ora-image doc) path))
