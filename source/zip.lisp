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
;; A zip archive, as much of it as OpenRaster needs: entries stored or
;; deflated, read by the central directory and written with every size
;; known up front (no data descriptors, no Zip64, no encryption).  An
;; archive another program wrote may carry data descriptors; the sizes
;; come from the central directory, so they do no harm.
;;
;; With ora.lisp this is the spin-out candidate "openraster": it uses
;; nothing of Stratum but crc32 (png.lisp), the two string helpers of
;; file.lisp, chipz to inflate and salza2 to deflate.
;;

(defun le16 (octets i)
  (logior (aref octets i) (ash (aref octets (1+ i)) 8)))

(defun le32 (octets i)
  (logior (le16 octets i) (ash (le16 octets (+ i 2)) 16)))

(defun put-le16 (octets i value)
  (setf (aref octets i) (ldb (byte 8 0) value)
        (aref octets (1+ i)) (ldb (byte 8 8) value)))

(defun put-le32 (octets i value)
  (put-le16 octets i (ldb (byte 16 0) value))
  (put-le16 octets (+ i 2) (ldb (byte 16 16) value)))

(defun octet-vector (sequence)
  (coerce sequence '(simple-array (unsigned-byte 8) (*))))

(defun concat-octets (pieces)
  "One octet vector of the octet vectors PIECES in order."
  (let ((out (make-array (reduce #'+ pieces :key #'length) :element-type '(unsigned-byte 8)))
        (pos 0))
    (dolist (piece pieces out)
      (replace out piece :start1 pos)
      (incf pos (length piece)))))

(defun padded (octets count)
  "OCTETS followed by COUNT zero bytes."
  (let ((out (make-array (+ (length octets) count) :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace out octets)
    out))

(defun zip-octets-p (octets)
  "True when OCTETS begin as a zip archive does (a local file header)."
  (and (>= (length octets) 4)
       (= (aref octets 0) #x50) (= (aref octets 1) #x4b)
       (= (aref octets 2) 3) (= (aref octets 3) 4)))

(defun zip-entries (octets)
  "The entries of the zip archive OCTETS as a list of (name . bytes) in
the central directory's order, inflated, each one's CRC and size
checked; directory entries (a name ending in /) left out."
  (let* ((octets (octet-vector octets))
         (n (length octets))
         (eocd (loop for i from (- n 22) downto (max 0 (- n 22 65535))
                     when (= (le32 octets i) #x06054b50) return i)))
    (unless eocd (error "Not a zip archive: no end-of-central-directory record."))
    (let ((count (le16 octets (+ eocd 10)))
          (at (le32 octets (+ eocd 16)))
          (entries '()))
      (dotimes (k count (nreverse entries))
        (unless (and (<= (+ at 46) n) (= (le32 octets at) #x02014b50))
          (error "Bad central directory entry ~d at ~d." k at))
        (let* ((method (le16 octets (+ at 10)))
               (crc (le32 octets (+ at 16)))
               (packed (le32 octets (+ at 20)))
               (raw (le32 octets (+ at 24)))
               (name-length (le16 octets (+ at 28)))
               (extra-length (le16 octets (+ at 30)))
               (comment-length (le16 octets (+ at 32)))
               (local (le32 octets (+ at 42)))
               (name (octets-string (subseq octets (+ at 46) (+ at 46 name-length)))))
          (unless (and (<= (+ local 30) n) (= (le32 octets local) #x04034b50))
            (error "Bad local header for ~a." name))
          (let* ((start (+ local 30 (le16 octets (+ local 26)) (le16 octets (+ local 28))))
                 (end (+ start packed)))
            (unless (<= end n) (error "~a runs past the end of the archive." name))
            (let* ((data (subseq octets start end))
                   (bytes (case (if (zerop raw) 0 method)
                            ;; an empty entry is empty however it was packed
                            ;; (salza2 deflates nothing to nothing, which is
                            ;; not a deflate stream)
                            (0 (if (zerop raw) (make-array 0 :element-type '(unsigned-byte 8)) data))
                            ;; chipz's raw-deflate reader wants a byte of
                            ;; look-ahead past the final block, or it reports
                            ;; a premature end on a stream that is complete
                            ;; (zlib's trailer supplies it; a zip entry has
                            ;; nothing after the data).  Four zero bytes do.
                            (8 (octet-vector (chipz:decompress nil 'chipz:deflate (padded data 4))))
                            (t (error "~a uses compression method ~d, not stored or deflated." name method)))))
              (unless (= (length bytes) raw)
                (error "~a inflated to ~d bytes, not ~d." name (length bytes) raw))
              (unless (= (crc32 bytes 0 (length bytes)) crc)
                (error "~a fails its CRC." name))
              (unless (and (plusp name-length) (char= (char name (1- name-length)) #\/))
                (push (cons name bytes) entries))))
          (incf at (+ 46 name-length extra-length comment-length)))))))

(defun zip-octets (entries)
  "A zip archive of ENTRIES in order, each (name bytes) or (name bytes
:method :stored | :deflate), stored unless said otherwise; names are
written as UTF-8 and flagged so; every entry is dated 1980-01-01."
  (let ((parts '()) (directory '()) (offset 0))
    (dolist (entry entries)
      (destructuring-bind (name bytes &key (method :stored)) entry
        (let* ((name-octets (string-octets name))
               (bytes (octet-vector bytes))
               (method (if (zerop (length bytes)) :stored method))
               (crc (crc32 bytes 0 (length bytes)))
               (data (ecase method
                       (:stored bytes)
                       (:deflate (octet-vector (salza2:compress-data bytes 'salza2:deflate-compressor)))))
               (code (ecase method (:stored 0) (:deflate 8)))
               (local (make-array (+ 30 (length name-octets)) :element-type '(unsigned-byte 8) :initial-element 0))
               (central (make-array (+ 46 (length name-octets)) :element-type '(unsigned-byte 8) :initial-element 0)))
          (put-le32 local 0 #x04034b50)
          (put-le16 local 4 20)              ; version needed: 2.0
          (put-le16 local 6 #x0800)          ; general purpose: UTF-8 names
          (put-le16 local 8 code)
          (put-le16 local 10 0)              ; time 00:00:00
          (put-le16 local 12 #x21)           ; date 1980-01-01
          (put-le32 local 14 crc)
          (put-le32 local 18 (length data))
          (put-le32 local 22 (length bytes))
          (put-le16 local 26 (length name-octets))
          (put-le16 local 28 0)              ; no extra field
          (replace local name-octets :start1 30)
          (put-le32 central 0 #x02014b50)
          (put-le16 central 4 20)            ; made by: 2.0, MS-DOS attributes
          (put-le16 central 6 20)
          (put-le16 central 8 #x0800)
          (put-le16 central 10 code)
          (put-le16 central 12 0)
          (put-le16 central 14 #x21)
          (put-le32 central 16 crc)
          (put-le32 central 20 (length data))
          (put-le32 central 24 (length bytes))
          (put-le16 central 28 (length name-octets))
          ;; extra, comment, disk, internal and external attributes: 0
          (put-le32 central 42 offset)
          (replace central name-octets :start1 46)
          (push local parts)
          (push data parts)
          (push central directory)
          (incf offset (+ (length local) (length data))))))
    (let* ((directory (nreverse directory))
           (size (reduce #'+ directory :key #'length))
           (eocd (make-array 22 :element-type '(unsigned-byte 8) :initial-element 0)))
      (put-le32 eocd 0 #x06054b50)
      (put-le16 eocd 8 (length directory))
      (put-le16 eocd 10 (length directory))
      (put-le32 eocd 12 size)
      (put-le32 eocd 16 offset)
      (concat-octets (append (nreverse parts) directory (list eocd))))))
