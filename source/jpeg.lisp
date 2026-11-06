;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; A JPEG file read here, so the page can send the file, a fifth of the
;; packed pixels, instead of what it decoded.  The page shows its own
;; decode, so the server's must be the same bytes: libjpeg-turbo gives
;; them (measured against Chromium on photographs, no difference at
;; all), and the page sends a hash of what it decoded so that any other
;; browser's decoder, which may not agree, is found out and the page
;; sends the packed pixels as before.  TurboJPEG rather than plain
;; libjpeg because libjpeg's default answer to a bad file is to exit the
;; process.  With no library the page is told so and nothing changes.
;;

(defparameter *turbojpeg-file* (or (uiop:getenv "TURBOJPEG_LIBRARY") "libturbojpeg.so.0")
  "The TurboJPEG shared library.")

(defvar *turbojpeg* :unknown
  "Whether the library is loaded: true, nil, or :unknown before the
first asking.")

(defun turbojpeg? ()
  "Whether this server can read a JPEG."
  (when (eq *turbojpeg* :unknown)
    (setf *turbojpeg* (handler-case (progn (cffi:load-foreign-library *turbojpeg-file*) t)
                        (error () nil))))
  *turbojpeg*)

(defun jpeg-octets-p (octets)
  (and (> (length octets) 3)
       (= (aref octets 0) #xff) (= (aref octets 1) #xd8) (= (aref octets 2) #xff)))

(defconstant +tjpf-rgba+ 7 "TurboJPEG's name for four bytes a pixel, red first.")

(defun decode-jpeg (octets &optional (check (constantly t)))
  "OCTETS, a JPEG file, as (values pixels w h), RGBA.  CHECK, a function
of W and H called once the header is read and before any pixels are
made, may refuse a size by an error.  A file that cannot be read is an
error in words."
  (unless (turbojpeg?)
    (error "This server has no JPEG decoder."))
  (let ((handle (cffi:foreign-funcall "tjInitDecompress" :pointer)))
    (when (cffi:null-pointer-p handle)
      (error "The JPEG decoder would not start."))
    (unwind-protect
         (flet ((failed ()
                  (error "The JPEG could not be read: ~a"
                         (cffi:foreign-funcall "tjGetErrorStr2" :pointer handle :string))))
           (cffi:with-pointer-to-vector-data (source octets)
             (multiple-value-bind (w h)
                 (cffi:with-foreign-objects ((w :int) (h :int) (subsampling :int) (colorspace :int))
                   (unless (zerop (cffi:foreign-funcall "tjDecompressHeader3"
                                                        :pointer handle :pointer source
                                                        :unsigned-long (length octets)
                                                        :pointer w :pointer h
                                                        :pointer subsampling :pointer colorspace
                                                        :int))
                     (failed))
                   (values (cffi:mem-ref w :int) (cffi:mem-ref h :int)))
               (funcall check w h)
               (let ((pixels (make-rgba w h)))
                 (cffi:with-pointer-to-vector-data (target pixels)
                   (unless (zerop (cffi:foreign-funcall "tjDecompress2"
                                                        :pointer handle :pointer source
                                                        :unsigned-long (length octets)
                                                        :pointer target
                                                        :int w :int 0 :int h
                                                        :int +tjpf-rgba+ :int 0
                                                        :int))
                     (failed)))
                 (values pixels w h)))))
      (cffi:foreign-funcall "tjDestroy" :pointer handle :int))))

(defun jpeg-orientation (octets)
  "The EXIF orientation of a JPEG file, 1 to 8: 1 with none, as a page
reads it.  Anything malformed is 1, and the page's hash will say so."
  (or (ignore-errors
       (let ((at 2))
         (flet ((u16be (p) (+ (ash (aref octets p) 8) (aref octets (1+ p)))))
           (loop
             (unless (= (aref octets at) #xff) (return nil))
             (let ((marker (aref octets (1+ at)))
                   (length (u16be (+ at 2))))
               (when (member marker '(#xd9 #xda)) (return nil))
               (when (and (= marker #xe1)
                          (equalp (subseq octets (+ at 4) (+ at 10))
                                  #(69 120 105 102 0 0))) ; Exif, two zeros
                 (let* ((tiff (+ at 10))
                        (little? (= (aref octets tiff) 73)))
                   (flet ((u16 (p)
                            (let ((a (aref octets (+ tiff p))) (b (aref octets (+ tiff p 1))))
                              (if little? (+ a (ash b 8)) (+ b (ash a 8)))))
                          (u32 (p)
                            (let ((a (aref octets (+ tiff p))) (b (aref octets (+ tiff p 1)))
                                  (c (aref octets (+ tiff p 2))) (d (aref octets (+ tiff p 3))))
                              (if little?
                                  (+ a (ash b 8) (ash c 16) (ash d 24))
                                  (+ d (ash c 8) (ash b 16) (ash a 24))))))
                     (let ((directory (u32 4)))
                       (dotimes (i (u16 directory))
                         (let ((entry (+ directory 2 (* 12 i))))
                           (when (= (u16 entry) #x112)
                             (let ((value (u16 (+ entry 8))))
                               (return-from jpeg-orientation
                                 (if (<= 1 value 8) value 1)))))))))
                 (return nil))
               (incf at (+ 2 length)))))))
      1))

(defun oriented-rgba (pixels w h orientation)
  "PIXELS, W x H, as EXIF ORIENTATION says to show them: (values pixels
w h)."
  (flet ((turned (&rest turns)
           (dolist (turn turns (values pixels w h))
             (multiple-value-bind (nw nh) (turned-size w h turn)
               (setf pixels (turn-rgba pixels w h turn)
                     w nw
                     h nh)))))
    (ecase orientation
      (1 (values pixels w h))
      (2 (turned :flip-h))
      (3 (turned :180))
      (4 (turned :flip-v))
      (5 (turned :cw :flip-h))
      (6 (turned :cw))
      (7 (turned :ccw :flip-h))
      (8 (turned :ccw)))))

(defun pixels-hash (pixels)
  "A 32-bit hash of PIXELS, four bytes at a time, as the page's
hashPixels computes it.  The product stays inside a fixnum on every
Lisp here, which is why the multiplier is this one."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels))
  (let ((hash 2166136261))
    (declare (type (unsigned-byte 32) hash))
    (loop for o of-type fixnum from 0 below (- (length pixels) 3) by 4
          do (setf hash (logand (* (logxor hash
                                           (logior (aref pixels o)
                                                   (ash (aref pixels (+ o 1)) 8)
                                                   (ash (aref pixels (+ o 2)) 16)
                                                   (ash (aref pixels (+ o 3)) 24)))
                                   16777619)
                                #xffffffff)))
    hash))
