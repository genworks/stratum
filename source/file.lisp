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
;; The native file, .stratum: a document with its layers, as the
;; materialized specs already describe it.  The file is a line "STRATUM1",
;; a line with the header's length in bytes, the header -- one
;; s-expression, read back with *read-eval* off -- and then the blobs it
;; refers to, each a layer's pixels or a mask as a zlib stream (salza2 to
;; write, chipz to read).  Nothing else: no undo, no selection, no
;; clipboard.
;;
;;   (:stratum 1 :name "demo" :width 1920 :height 1080 :background (1 1 1)
;;    :layers ((:kind :raster :name "Sky" :pixels (:blob 0) ...) ...)
;;    :blobs ((packed-length raw-length) ...))
;;

(defparameter *file-magic* "STRATUM1")

(defun string-octets (string)
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun octets-string (octets)
  (flexi-streams:octets-to-string octets :external-format :utf-8))

;;; zlib framing, not raw deflate: chipz reads some of salza2's raw
;;; streams (the most compressible layers) as ending early, and zlib's
;;; trailer settles where a blob ends and checks it.
(defun deflate-octets (octets)
  (salza2:compress-data octets 'salza2:zlib-compressor))

(defun inflate-octets (octets)
  (coerce (chipz:decompress nil 'chipz:zlib octets) '(simple-array (unsigned-byte 8) (*))))

(defun document-octets (doc)
  "DOC as the bytes of a .stratum file."
  (let ((blobs '())
        (count 0))
    (flet ((blob (bytes)
             (push (list (deflate-octets bytes) (length bytes)) blobs)
             (prog1 (list :blob count) (incf count))))
      (let* ((layers (mapcar (lambda (spec)
                               (let ((spec (copy-list spec)))
                                 (dolist (key '(:pixels :mask-pixels) spec)
                                   (when (getf spec key)
                                     (setf (getf spec key) (blob (getf spec key)))))))
                             (materialized-specs doc)))
             (blobs (reverse blobs))
             (header (list :stratum 1
                           :name (the-object doc doc-name)
                           :width (the-object doc doc-width)
                           :height (the-object doc doc-height)
                           :background (the-object doc background-srgb)
                           :layers layers
                           :blobs (mapcar (lambda (b) (list (length (first b)) (second b))) blobs)))
             (header-octets (string-octets (with-standard-io-syntax (prin1-to-string header))))
             (prefix (string-octets (format nil "~a~%~d~%" *file-magic* (length header-octets))))
             (out (make-array (+ (length prefix) (length header-octets)
                                 (reduce #'+ blobs :key (lambda (b) (length (first b)))))
                              :element-type '(unsigned-byte 8)))
             (pos 0))
        (flet ((put (bytes) (replace out bytes :start1 pos) (incf pos (length bytes))))
          (put prefix)
          (put header-octets)
          (dolist (b blobs) (put (first b))))
        out))))

(defun read-document-octets (octets name)
  "A document, named NAME, from the bytes of a .stratum file."
  (let ((magic-end (position 10 octets)))
    (unless (and magic-end (string= (octets-string (subseq octets 0 magic-end)) *file-magic*))
      (error "Not a Stratum file."))
    (let* ((length-end (position 10 octets :start (1+ magic-end)))
           (header-length (parse-integer (octets-string (subseq octets (1+ magic-end) length-end))))
           (header-start (1+ length-end))
           (header (with-standard-io-syntax
                     (let ((*read-eval* nil))
                       (read-from-string
                        (octets-string (subseq octets header-start (+ header-start header-length)))))))
           (pos (+ header-start header-length)))
      (unless (eql (getf header :stratum) 1)
        (error "Unknown Stratum file version ~a." (getf header :stratum)))
      (let* ((blobs (mapcar (lambda (entry)
                              (destructuring-bind (packed-length raw-length) entry
                                (let ((raw (inflate-octets (subseq octets pos (+ pos packed-length)))))
                                  (unless (= (length raw) raw-length)
                                    (error "A blob came back ~a bytes, not ~a." (length raw) raw-length))
                                  (incf pos packed-length)
                                  raw)))
                            (getf header :blobs)))
             (layers (mapcar (lambda (spec)
                               (let ((spec (copy-list spec)))
                                 (dolist (key '(:pixels :mask-pixels) spec)
                                   (let ((ref (getf spec key)))
                                     (when (and (consp ref) (eql (first ref) :blob))
                                       (setf (getf spec key) (nth (second ref) blobs)))))))
                             (getf header :layers))))
        (make-object 'image-document
                     :doc-name name
                     :doc-width (getf header :width)
                     :doc-height (getf header :height)
                     :background-srgb (getf header :background)
                     :layer-specs layers)))))
