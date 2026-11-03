;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;; Writing: zpng.  Reading: the decoder below, about two hundred lines
;;; over chipz's inflate, since the Gendl images carry no PNG reader.
;;; It takes every PNG: the five color types at every bit depth, a
;;; palette with or without transparency, a transparent key color, the
;;; five filters, and Adam7 interlace; every chunk's CRC is checked.
;;; Samples come out as the bytes in the file -- 16-bit samples rounded
;;; to 8, smaller ones scaled up -- with no gamma or color-space
;;; conversion, exactly as the browser's decoder hands them over (no
;;; premultiply, no color conversion), so a file reads the same on both
;;; halves.  Ancillary chunks (gAMA, iCCP, sRGB, tEXt ...) are skipped.

(defun png-octets (rgba width height)
  "RGBA, straight-alpha sRGB bytes, encoded as a PNG in memory."
  (let ((png (make-instance 'zpng:png :color-type :truecolor-alpha
                                      :width width :height height
                                      :image-data rgba)))
    (flexi-streams:with-output-to-sequence (s)
      (zpng:write-png-stream png s))))

(defparameter *png-signature* (coerce #(137 80 78 71 13 10 26 10) '(simple-array (unsigned-byte 8) (*))))

(defun png-octets-p (octets)
  "True when OCTETS begin with the PNG signature."
  (and (>= (length octets) 8)
       (loop for i below 8 always (= (aref octets i) (aref *png-signature* i)))))

(defparameter *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c) (logxor #xedb88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32 (octets start end)
  "The CRC-32 of OCTETS from START below END, as PNG chunks carry it."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum start end))
  (let ((crc #xffffffff) (table *crc-table*))
    (declare (type (unsigned-byte 32) crc) (type (simple-array (unsigned-byte 32) (256)) table))
    (loop for i from start below end
          do (setf crc (logxor (aref table (logand (logxor crc (aref octets i)) #xff)) (ash crc -8))))
    (logxor crc #xffffffff)))

(defun be32 (octets i)
  (logior (ash (aref octets i) 24) (ash (aref octets (+ i 1)) 16)
          (ash (aref octets (+ i 2)) 8) (aref octets (+ i 3))))

(defun png-chunks (octets)
  "The chunks of a PNG as a list of (type start end), START below END
being the chunk's data in OCTETS; every CRC checked; ends at IEND."
  (unless (png-octets-p octets) (error "Not a PNG file."))
  (let ((n (length octets)))
    (loop with i = 8
          while (<= (+ i 12) n)
          collect (let* ((len (be32 octets i))
                         (type (map 'string #'code-char (subseq octets (+ i 4) (+ i 8))))
                         (start (+ i 8)) (end (+ start len)))
                    (when (> (+ end 4) n) (error "PNG chunk ~a runs past the end of the file." type))
                    (unless (= (be32 octets end) (crc32 octets (+ i 4) end))
                      (error "PNG chunk ~a fails its CRC." type))
                    (setf i (+ end 4))
                    (list type start end))
            into chunks
          until (equal (first (first (last chunks))) "IEND")
          finally (return chunks))))

(defparameter *adam7-passes* '((0 0 8 8) (4 0 8 8) (0 4 4 8) (2 0 4 4) (0 2 2 4) (1 0 2 2) (0 1 1 2))
  "Each Adam7 pass as (x-start y-start x-step y-step).")

(defun unfilter-row! (row prior filter bpp)
  "Undo PNG FILTER on ROW in place, PRIOR being the row above (zeros for
the first) and BPP the bytes per complete pixel, at least one."
  (declare (type (simple-array (unsigned-byte 8) (*)) row prior) (type fixnum filter bpp))
  (let ((n (length row)))
    (flet ((a (i) (if (>= i bpp) (aref row (- i bpp)) 0))
           (b (i) (aref prior i))
           (c (i) (if (>= i bpp) (aref prior (- i bpp)) 0)))
      (declare (inline a b c))
      (ecase filter
        (0)
        (1 (loop for i below n do (setf (aref row i) (logand #xff (+ (aref row i) (a i))))))
        (2 (loop for i below n do (setf (aref row i) (logand #xff (+ (aref row i) (b i))))))
        (3 (loop for i below n do (setf (aref row i) (logand #xff (+ (aref row i) (floor (+ (a i) (b i)) 2))))))
        (4 (loop for i below n
                 do (let* ((a (a i)) (b (b i)) (c (c i))
                           (p (- (+ a b) c))
                           (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c)))
                           (pred (cond ((and (<= pa pb) (<= pa pc)) a) ((<= pb pc) b) (t c))))
                      (setf (aref row i) (logand #xff (+ (aref row i) pred))))))))
    row))

(defun png-sample (row index depth)
  "Sample INDEX of an unfiltered ROW at bit DEPTH, as the file holds it."
  (declare (type (simple-array (unsigned-byte 8) (*)) row) (type fixnum index depth))
  (case depth
    (8 (aref row index))
    (16 (logior (ash (aref row (* 2 index)) 8) (aref row (1+ (* 2 index)))))
    (t (let ((bit (* index depth)))
         (ldb (byte depth (- 8 depth (mod bit 8))) (aref row (floor bit 8)))))))

(defun read-png (input)
  "(values rgba width height): the PNG in INPUT, a vector of octets,
decoded to RGBA8, straight alpha, the bytes as the file holds them."
  (let* ((octets (if (typep input '(simple-array (unsigned-byte 8) (*)))
                     input
                     (coerce input '(simple-array (unsigned-byte 8) (*)))))
         (chunks (png-chunks octets))
         (ihdr (or (find "IHDR" chunks :key #'first :test #'string=) (error "PNG without IHDR.")))
         (width (be32 octets (second ihdr)))
         (height (be32 octets (+ 4 (second ihdr))))
         (depth (aref octets (+ 8 (second ihdr))))
         (color-type (aref octets (+ 9 (second ihdr))))
         (interlace (aref octets (+ 12 (second ihdr))))
         (channels (ecase color-type (0 1) (2 3) (3 1) (4 2) (6 4)))
         (bpp (max 1 (floor (* channels depth) 8)))
         (palette (let ((c (find "PLTE" chunks :key #'first :test #'string=)))
                    (and c (subseq octets (second c) (third c)))))
         (trns (let ((c (find "tRNS" chunks :key #'first :test #'string=)))
                 (and c (subseq octets (second c) (third c)))))
         (data (inflate-octets
                (let ((parts (remove "IDAT" chunks :key #'first :test-not #'string=)))
                  (when (null parts) (error "PNG without image data."))
                  (if (= (length parts) 1)
                      (subseq octets (second (first parts)) (third (first parts)))
                      (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                             (mapcar (lambda (c) (subseq octets (second c) (third c))) parts))))))
         (out (make-rgba width height))
         (full (- (ash 1 depth) 1))
         (pos 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) data out) (type fixnum pos))
    (unless (and (plusp width) (plusp height)) (error "PNG of no size."))
    (unless (member depth (ecase color-type ((0) '(1 2 4 8 16)) ((2 4 6) '(8 16)) ((3) '(1 2 4 8))))
      (error "PNG color type ~a at bit depth ~a is not allowed." color-type depth))
    (when (and (= color-type 3) (null palette)) (error "Indexed PNG without a palette."))
    ;; the transparent key color, for gray and truecolor
    (let ((key (and trns (member color-type '(0 2))
                    (loop for k below channels collect (logior (ash (aref trns (* 2 k)) 8) (aref trns (1+ (* 2 k))))))))
      (labels ((to-byte (v) (cond ((= depth 8) v)
                                ((= depth 16) (round (* v 255) 65535))
                                (t (round (* v 255) full))))
             (pass-rows (xs ys xstep ystep)
               ;; One pass's rows, unfiltered and written into OUT.
               (let* ((pw (if (< xs width) (ceiling (- width xs) xstep) 0))
                      (ph (if (< ys height) (ceiling (- height ys) ystep) 0))
                      (stride (ceiling (* pw channels depth) 8))
                      (prior (make-array stride :element-type '(unsigned-byte 8) :initial-element 0))
                      (row (make-array stride :element-type '(unsigned-byte 8))))
                 (when (and (plusp pw) (plusp ph))
                   (dotimes (py ph)
                     (when (> (+ pos 1 stride) (length data)) (error "PNG image data ends early."))
                     (let ((filter (aref data pos)))
                       (replace row data :start2 (1+ pos) :end2 (+ pos 1 stride))
                       (incf pos (1+ stride))
                       (unfilter-row! row prior filter bpp)
                       (let ((y (+ ys (* py ystep))))
                         (dotimes (px pw)
                           (let ((o (* 4 (+ xs (* px xstep) (* width y)))))
                             (ecase color-type
                               (6 (if (= depth 8)
                                      (replace out row :start1 o :start2 (* 4 px) :end2 (* 4 (1+ px)))
                                      (dotimes (k 4) (setf (aref out (+ o k)) (round (* 255 (png-sample row (+ (* 4 px) k) 16)) 65535)))))
                               (2 (let ((r (png-sample row (* 3 px) depth))
                                        (g (png-sample row (1+ (* 3 px)) depth))
                                        (b (png-sample row (+ 2 (* 3 px)) depth)))
                                    (setf (aref out o) (to-byte r) (aref out (+ o 1)) (to-byte g) (aref out (+ o 2)) (to-byte b)
                                          (aref out (+ o 3)) (if (and key (equal key (list r g b))) 0 255))))
                               (0 (let* ((v (png-sample row px depth)) (g (to-byte v)))
                                    (setf (aref out o) g (aref out (+ o 1)) g (aref out (+ o 2)) g
                                          (aref out (+ o 3)) (if (and key (= v (first key))) 0 255))))
                               (4 (let ((g (to-byte (png-sample row (* 2 px) depth)))
                                        (a (to-byte (png-sample row (1+ (* 2 px)) depth))))
                                    (setf (aref out o) g (aref out (+ o 1)) g (aref out (+ o 2)) g (aref out (+ o 3)) a)))
                               (3 (let ((i (png-sample row px depth)))
                                    (when (>= (* 3 i) (length palette)) (error "PNG palette index ~a beyond the palette." i))
                                    (setf (aref out o) (aref palette (* 3 i))
                                          (aref out (+ o 1)) (aref palette (1+ (* 3 i)))
                                          (aref out (+ o 2)) (aref palette (+ 2 (* 3 i)))
                                          (aref out (+ o 3)) (if (and trns (< i (length trns))) (aref trns i) 255))))))))
                       (rotatef row prior)))))))
        (if (= interlace 1)
            (dolist (pass *adam7-passes*) (apply #'pass-rows pass))
            (pass-rows 0 0 1 1))))
    (values out width height)))

(defun read-png-file (path)
  "(values rgba width height) of the PNG file at PATH."
  (read-png (with-open-file (in path :element-type '(unsigned-byte 8))
              (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
                (read-sequence octets in)
                octets))))

(defun open-png-file (path &optional name)
  "File > Open from Lisp: the PNG file at PATH as a new document, NAME
or the file's own name made unique, registered to be served; the
document."
  (multiple-value-bind (pixels w h) (read-png-file path)
    (let ((name (unique-document-name (or name (pathname-name path) "untitled"))))
      (setf (gethash name *documents*) (make-file-document name w h pixels)))))
