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
;; OpenRaster, the layered interchange file of Krita, GIMP, MyPaint and
;; Scribus (openraster.org, version 0.0.6).  An .ora file is a zip
;; archive: `mimetype' first and stored, holding image/openraster;
;; `stack.xml', the layer stack, first child on top; one PNG per layer
;; under data/; `mergedimage.png', the flattened picture, so a viewer
;; that knows nothing of layers still shows something; and
;; Thumbnails/thumbnail.png, at most 256 pixels a side.
;;
;; Here the file is an `ora-image' of `ora-layer's, top first as the
;; stack lists them, each holding its PNG as octets and its place,
;; opacity, visibility and composite-op; nested stacks (a group in
;; Krita or GIMP) are flattened into the list on reading, the group's
;; opacity multiplied in and its visibility applied.  Attributes of a
;; layer element beyond the standard ones are kept as strings and
;; written back, which is how an application's own extensions ride
;; along (ora-document.lisp uses that for Stratum's adjustment layers);
;; any other file in the archive is kept in `files' the same way.
;; `read-ora' and `ora-octets' are the two ends; nothing here decodes a
;; layer's pixels until asked (ora-layer-rgba).
;;
;; With zip.lisp this is the spin-out candidate "openraster": it uses
;; nothing of Stratum but crc32, read-png and png-octets (png.lisp)
;; and the two string helpers of file.lisp.
;;

(defstruct (ora-layer (:constructor make-ora-layer
                          (&key name src (x 0) (y 0) (opacity 1d0) (visible? t)
                                (composite-op "svg:src-over") png attributes)))
  "One layer: NAME, SRC (its path in the archive, assigned when written
if nil), X and Y (its top-left on the image), OPACITY 0 to 1, VISIBLE?,
COMPOSITE-OP (an svg: blend name), PNG (its file as octets) and
ATTRIBUTES, an alist of the layer element's other attributes as strings."
  name src (x 0) (y 0) (opacity 1d0) (visible? t) (composite-op "svg:src-over") png attributes)

(defstruct (ora-image (:constructor make-ora-image
                          (&key width height layers merged thumbnail files
                                (xres 72) (yres 72) version namespaces)))
  "An OpenRaster image: WIDTH x HEIGHT, LAYERS top first, MERGED and
THUMBNAIL as PNG octets (or nil; the thumbnail is made from the merged
image when written), FILES an alist of (path . octets) for anything
else in the archive, NAMESPACES an alist of (prefix . uri) declared on
the root for extension attributes."
  width height layers merged thumbnail files (xres 72) (yres 72) version namespaces)

(defparameter *ora-layer-attributes*
  '("src" "name" "x" "y" "opacity" "visibility" "composite-op")
  "The layer attributes the struct holds in its own slots; the rest go to ATTRIBUTES.")

(defparameter *ora-standard-files*
  '("mimetype" "stack.xml" "mergedimage.png" "Thumbnails/thumbnail.png"))

;;; Numbers in stack.xml: an optional sign, digits and a fraction, read
;;; without the Lisp reader.

(defun parse-decimal (string &optional (default 0))
  "STRING as a rational, or DEFAULT when STRING is nil or not a plain
decimal number."
  (if (null string)
      default
      (let ((s (string-trim " " string)) (i 0) (sign 1) (value 0) (scale 1) (digits 0))
        (when (and (plusp (length s)) (find (char s 0) "+-"))
          (when (char= (char s 0) #\-) (setf sign -1))
          (incf i))
        (loop while (and (< i (length s)) (digit-char-p (char s i)))
              do (setf value (+ (* value 10) (digit-char-p (char s i))))
                 (incf i) (incf digits))
        (when (and (< i (length s)) (char= (char s i) #\.))
          (incf i)
          (loop while (and (< i (length s)) (digit-char-p (char s i)))
                do (setf value (+ (* value 10) (digit-char-p (char s i)))
                         scale (* scale 10))
                   (incf i) (incf digits)))
        (if (and (plusp digits) (= i (length s)))
            (* sign (/ value scale))
            default))))

;;; Just enough XML for stack.xml: elements, attributes, comments, the
;;; declaration, the five named entities and numeric references.  Text
;;; content is dropped (the stack has none that matters).  An element is
;;; (name attributes . children), attributes an alist of strings.

(defun xml-unescape (string)
  (if (not (find #\& string))
      string
      (with-output-to-string (out)
        (let ((i 0) (n (length string)))
          (loop while (< i n)
                do (let ((c (char string i)))
                     (if (char/= c #\&)
                         (progn (write-char c out) (incf i))
                         (let ((semi (position #\; string :start i)))
                           (unless semi (error "XML: a bare & in ~s." string))
                           (let ((ref (subseq string (1+ i) semi)))
                             (write-char
                              (cond ((string= ref "amp") #\&)
                                    ((string= ref "lt") #\<)
                                    ((string= ref "gt") #\>)
                                    ((string= ref "quot") #\")
                                    ((string= ref "apos") #\')
                                    ((and (> (length ref) 1) (char= (char ref 0) #\#))
                                     (code-char (if (char-equal (char ref 1) #\x)
                                                    (parse-integer ref :start 2 :radix 16)
                                                    (parse-integer ref :start 1))))
                                    (t (error "XML: unknown entity &~a;." ref)))
                              out))
                           (setf i (1+ semi))))))))))

(defun xml-escape (string)
  (with-output-to-string (out)
    (loop for c across (string string)
          do (case c
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char c out))))))

(defun xml-parse (string)
  "The root element of the XML document STRING as (name attributes . children)."
  (let ((i 0) (n (length string)))
    (labels ((peek () (and (< i n) (char string i)))
             (skip-ws ()
               (loop while (and (< i n) (member (char string i) '(#\Space #\Tab #\Newline #\Return)))
                     do (incf i)))
             (starts (prefix)
               (and (<= (+ i (length prefix)) n)
                    (string= prefix string :start2 i :end2 (+ i (length prefix)))))
             (skip-past (terminator)
               (let ((p (search terminator string :start2 i)))
                 (unless p (error "XML: no ~a closes what opened at ~d." terminator i))
                 (setf i (+ p (length terminator)))))
             (name-char-p (c) (or (alphanumericp c) (find c "_-.:")))
             (read-name ()
               (let ((start i))
                 (loop while (and (< i n) (name-char-p (char string i))) do (incf i))
                 (when (= start i) (error "XML: a name was expected at ~d." i))
                 (subseq string start i)))
             (read-attributes ()
               (let ((attrs '()))
                 (loop
                   (skip-ws)
                   (let ((c (peek)))
                     (cond ((null c) (error "XML: a tag is never closed."))
                           ((or (char= c #\>) (char= c #\/)) (return (nreverse attrs)))
                           (t (let ((name (read-name)))
                                (skip-ws)
                                (unless (eql (peek) #\=) (error "XML: = expected after ~a." name))
                                (incf i)
                                (skip-ws)
                                (let ((q (peek)))
                                  (unless (member q '(#\" #\'))
                                    (error "XML: a quoted value was expected for ~a." name))
                                  (incf i)
                                  (let ((end (position q string :start i)))
                                    (unless end (error "XML: the value of ~a is never closed." name))
                                    (push (cons name (xml-unescape (subseq string i end))) attrs)
                                    (setf i (1+ end)))))))))))
             (read-element ()
               (incf i)
               (let ((name (read-name)) (children '()))
                 (let ((attrs (read-attributes)))
                   (skip-ws)
                   (cond ((starts "/>") (incf i 2) (list* name attrs nil))
                         ((eql (peek) #\>)
                          (incf i)
                          (loop
                            (let ((child (read-content)))
                              (cond ((eq child :end)
                                     (incf i 2)
                                     (let ((close (read-name)))
                                       (unless (string= close name)
                                         (error "XML: </~a> closes <~a>." close name)))
                                     (skip-ws)
                                     (unless (eql (peek) #\>) (error "XML: > expected after </~a>." name))
                                     (incf i)
                                     (return (list* name attrs (nreverse children))))
                                    ((eq child :eof) (error "XML: <~a> is never closed." name))
                                    (child (push child children))))))
                         (t (error "XML: <~a> is malformed." name))))))
             (read-content ()
               (loop
                 (when (>= i n) (return :eof))
                 (cond ((starts "</") (return :end))
                       ((starts "<!--") (skip-past "-->"))
                       ((starts "<![CDATA[") (skip-past "]]>"))
                       ((starts "<!") (skip-past ">"))
                       ((starts "<?") (skip-past "?>"))
                       ((eql (peek) #\<) (return (read-element)))
                       (t (incf i))))))
      (loop
        (let ((c (read-content)))
          (cond ((eq c :eof) (error "XML: no root element."))
                ((eq c :end) (error "XML: a closing tag with nothing open."))
                (c (return c))))))))

(defun xml-attribute (element name)
  (cdr (assoc name (second element) :test #'string=)))

;;; Reading.

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun ora-layers-of (element entries &key (opacity 1) (visible? t))
  "The ora-layers under the stack ELEMENT, top first, nested stacks
flattened in with their opacity multiplied and their visibility applied."
  (loop for child in (cddr element)
        append (let ((tag (first child)))
                 (flet ((attr (name) (xml-attribute child name)))
                   (cond ((string= tag "stack")
                          (ora-layers-of child entries
                                         :opacity (* opacity (parse-decimal (attr "opacity") 1))
                                         :visible? (and visible? (not (equal (attr "visibility") "hidden")))))
                         ((string= tag "layer")
                          (let* ((src (attr "src"))
                                 (png (and src (cdr (assoc src entries :test #'string=)))))
                            (unless src (error "OpenRaster: a layer without src."))
                            (unless png (error "OpenRaster: ~a is not in the archive." src))
                            (list (make-ora-layer
                                   :name (attr "name") :src src
                                   :x (round (parse-decimal (attr "x") 0))
                                   :y (round (parse-decimal (attr "y") 0))
                                   :opacity (float (* opacity (parse-decimal (attr "opacity") 1)) 1d0)
                                   :visible? (and visible? (not (equal (attr "visibility") "hidden")))
                                   :composite-op (or (attr "composite-op") "svg:src-over")
                                   :png png
                                   :attributes (remove-if (lambda (a)
                                                            (member (car a) *ora-layer-attributes* :test #'string=))
                                                          (second child))))))
                         (t nil))))))

(defun read-ora (input)
  "INPUT, the octets of an .ora file or a path to one, as an ora-image."
  (let* ((octets (if (or (stringp input) (pathnamep input)) (file-octets input) (octet-vector input)))
         (entries (zip-entries octets))
         (mimetype (cdr (assoc "mimetype" entries :test #'string=)))
         (stack (cdr (assoc "stack.xml" entries :test #'string=))))
    (when (and mimetype (string/= (octets-string mimetype) "image/openraster"))
      (error "Not an OpenRaster file: its mimetype is ~s." (octets-string mimetype)))
    (unless stack (error "Not an OpenRaster file: no stack.xml."))
    (let ((root (xml-parse (octets-string stack))))
      (unless (string= (first root) "image")
        (error "OpenRaster: stack.xml's root is <~a>, not <image>." (first root)))
      (flet ((attr (name) (xml-attribute root name)))
        (let ((w (round (parse-decimal (attr "w") 0)))
              (h (round (parse-decimal (attr "h") 0))))
          (unless (and (plusp w) (plusp h)) (error "OpenRaster: the image has no size."))
          (let ((layers (ora-layers-of root entries)))
            (make-ora-image
             :width w :height h
             :xres (round (parse-decimal (attr "xres") 72))
             :yres (round (parse-decimal (attr "yres") 72))
             :version (attr "version")
             :layers layers
             :merged (cdr (assoc "mergedimage.png" entries :test #'string=))
             :thumbnail (cdr (assoc "Thumbnails/thumbnail.png" entries :test #'string=))
             :files (remove-if (lambda (e)
                                 (or (member (car e) *ora-standard-files* :test #'string=)
                                     (member (car e) layers :key #'ora-layer-src :test #'string=)))
                               entries)
             :namespaces (loop for (name . value) in (second root)
                               when (and (> (length name) 6) (string= "xmlns:" name :end2 6))
                                 collect (cons (subseq name 6) value)))))))))

(defun ora-layer-rgba (layer)
  "(values rgba width height) of LAYER's PNG."
  (read-png (ora-layer-png layer)))

(defun ora-file (image path)
  "The octets of the extra file at PATH in IMAGE, or nil."
  (cdr (assoc path (ora-image-files image) :test #'string=)))

;;; Writing.

(defun box-downscale (pixels w h nw nh)
  "PIXELS (W x H RGBA8) reduced to NW x NH, each result the average of
its source box, color weighted by alpha."
  (let ((out (make-array (* 4 nw nh) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (y nh out)
      (let ((y0 (floor (* y h) nh))
            (y1 (max (1+ (floor (* y h) nh)) (floor (* (1+ y) h) nh))))
        (dotimes (x nw)
          (let ((x0 (floor (* x w) nw))
                (x1 (max (1+ (floor (* x w) nw)) (floor (* (1+ x) w) nw)))
                (r 0) (g 0) (b 0) (a 0) (count 0))
            (loop for sy from y0 below y1
                  do (loop for sx from x0 below x1
                           do (let* ((p (* 4 (+ sx (* w sy))))
                                     (pa (aref pixels (+ p 3))))
                                (incf r (* pa (aref pixels p)))
                                (incf g (* pa (aref pixels (+ p 1))))
                                (incf b (* pa (aref pixels (+ p 2))))
                                (incf a pa)
                                (incf count))))
            (let ((o (* 4 (+ x (* nw y)))))
              (when (plusp a)
                (setf (aref out o) (round r a)
                      (aref out (+ o 1)) (round g a)
                      (aref out (+ o 2)) (round b a)))
              (setf (aref out (+ o 3)) (round a count)))))))))

(defun ora-thumbnail-png (merged)
  "A thumbnail PNG of the merged PNG MERGED: at most 256 a side, the
aspect kept, never enlarged."
  (multiple-value-bind (pixels w h) (read-png merged)
    (if (and (<= w 256) (<= h 256))
        (png-octets pixels w h)
        (let* ((scale (/ 256 (max w h)))
               (nw (max 1 (round (* w scale))))
               (nh (max 1 (round (* h scale)))))
          (png-octets (box-downscale pixels w h nw nh) nw nh)))))

(defun ora-stack-xml (image)
  "IMAGE's stack.xml as a string; every layer must have its src by now."
  (with-output-to-string (out)
    (format out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format out "<image version=\"0.0.6\" w=\"~d\" h=\"~d\" xres=\"~d\" yres=\"~d\"~{ xmlns:~a=\"~a\"~}>~%"
            (ora-image-width image) (ora-image-height image)
            (ora-image-xres image) (ora-image-yres image)
            (loop for (prefix . uri) in (ora-image-namespaces image)
                  collect (xml-escape prefix) collect (xml-escape uri)))
    (format out "  <stack>~%")
    (dolist (layer (ora-image-layers image))
      (format out "    <layer src=\"~a\" name=\"~a\" x=\"~d\" y=\"~d\" opacity=\"~,4f\" visibility=\"~a\" composite-op=\"~a\"~{ ~a=\"~a\"~}/>~%"
              (xml-escape (ora-layer-src layer))
              (xml-escape (or (ora-layer-name layer) ""))
              (ora-layer-x layer) (ora-layer-y layer)
              (float (ora-layer-opacity layer) 1.0)
              (if (ora-layer-visible? layer) "visible" "hidden")
              (xml-escape (ora-layer-composite-op layer))
              (loop for (name . value) in (ora-layer-attributes layer)
                    collect (xml-escape name) collect (xml-escape value))))
    (format out "  </stack>~%</image>~%")))

(defun ora-octets (image)
  "IMAGE as the bytes of an .ora file: mimetype first and stored, then
stack.xml, the layers' PNGs (under data/layerN.png for a layer with no
src), the extra files, mergedimage.png and the thumbnail (made from
the merged image when IMAGE has none).  PNGs are stored, not deflated
again; the rest is deflated."
  (unless (ora-image-merged image)
    (error "OpenRaster: mergedimage.png is required; give the image its merged picture."))
  (loop for layer in (ora-image-layers image)
        for k from 1
        unless (ora-layer-src layer)
          do (setf (ora-layer-src layer) (format nil "data/layer~d.png" k)))
  (zip-octets
   (append
    (list (list "mimetype" (string-octets "image/openraster") :method :stored)
          (list "stack.xml" (string-octets (ora-stack-xml image)) :method :deflate))
    (loop for layer in (ora-image-layers image)
          collect (list (ora-layer-src layer) (ora-layer-png layer) :method :stored))
    (loop for (path . octets) in (ora-image-files image)
          collect (list path octets :method (if (png-octets-p octets) :stored :deflate)))
    (list (list "mergedimage.png" (ora-image-merged image) :method :stored)
          (list "Thumbnails/thumbnail.png"
                (or (ora-image-thumbnail image) (ora-thumbnail-png (ora-image-merged image)))
                :method :stored)))))

(defun write-ora-file (image path)
  "IMAGE written as the .ora file at PATH; PATH."
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (ora-octets image) out))
  path)
