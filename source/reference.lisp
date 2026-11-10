;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; Reference pictures: pictures a visitor attaches to the prompt box
;; beside the one being edited -- a face, a product, a style to follow.
;; They are the session's, like its clipboard, and stay attached until
;; removed, so a second prompt can speak of them too.  The model that
;; plans is shown them after the document's picture (prompt.lisp), the
;; image generator is shown them when a form asks (generate.lisp), and
;; place-reference puts one in the document as a layer.  Only PNG octets
;; are kept of each, at three sizes: what the generator and a layer get,
;; what the planner and the prompt log get, and the page's thumbnail.
;;

(defparameter *max-references* 4
  "How many reference pictures a session holds at once.")

(defparameter *reference-size* 1024
  "The longer side, in pixels, a reference picture is kept at; a larger
one is reduced on its way in.")

(defparameter *reference-view-size* 640
  "The longer side of a reference picture as the model that plans is
shown it and as the prompt log keeps it.")

(defparameter *reference-thumb-size* 96
  "The longer side of a reference picture's thumbnail on the page.")

(defstruct (reference (:conc-name ref-))
  id name width height
  png     ; as kept: what the generator is shown and a layer is made of
  view    ; at most *reference-view-size*: the planner's, and the log's
  thumb)  ; the page's

(defun data-url (png)
  (format nil "data:image/png;base64,~a" (sexp-agent:base64-encode png)))

(defun session-references ()
  "The current session's reference pictures, the oldest first."
  (sess-references *session*))

(defun reduced-rgba (pixels w h size)
  "(values pixels w h): PIXELS, RGBA W x H, with its longer side at most
SIZE; itself when it already is."
  (if (<= (max w h) size)
      (values pixels w h)
      (let* ((scale (/ size (max w h)))
             (nw (max 1 (round (* w scale))))
             (nh (max 1 (round (* h scale)))))
        (values (box-downscale pixels w h nw nh) nw nh))))

(defun reference-name (name)
  "NAME as a reference picture's: what of it prints, sixty characters at
most, or \"reference\"."
  (let ((clean (string-trim '(#\Space)
                            (substitute-if #\Space
                                           (lambda (c) (or (not (graphic-char-p c)) (char= c #\")))
                                           (if (stringp name) name "")))))
    (cond ((zerop (length clean)) "reference")
          ((> (length clean) 60) (subseq clean 0 60))
          (t clean))))

(defun ensure-reference-room ()
  "An error in words when the session holds all the reference pictures
it may."
  (when (>= (length (session-references)) *max-references*)
    (error "A prompt takes ~d reference picture~:p at most. Remove one first." *max-references*)))

(defun add-reference! (pixels w h &optional name)
  "PIXELS, RGBA W x H, attached to the current session as a reference
picture called NAME, after those already there.  The reference."
  (ensure-reference-room)
  (multiple-value-bind (kept kw kh) (reduced-rgba pixels w h *reference-size*)
    (multiple-value-bind (view vw vh) (reduced-rgba kept kw kh *reference-view-size*)
      (multiple-value-bind (thumb tw th) (reduced-rgba view vw vh *reference-thumb-size*)
        (let* ((png (png-octets kept kw kh))
               (reference (make-reference
                           :id (string-downcase (format nil "~36r" (random (expt 36 8) (make-random-state t))))
                           :name (reference-name name) :width kw :height kh
                           :png png
                           :view (if (eq view kept) png (png-octets view vw vh))
                           :thumb (png-octets thumb tw th))))
          (setf (sess-references *session*) (append (session-references) (list reference)))
          reference)))))

(defun remove-reference! (id)
  "The reference picture with ID gone from the current session; an error
in words when there is none."
  (let ((reference (or (find id (session-references) :key #'ref-id :test #'equal)
                       (error "That reference picture is not attached any more."))))
    (setf (sess-references *session*) (remove reference (session-references)))
    reference))

(defun clear-references! ()
  "Every reference picture gone from the current session."
  (setf (sess-references *session*) '()))

(defun reference-numbered (n)
  "Reference picture N of the current session, counted from 1 in the
order they were attached; an error in words when there is no such one."
  (let ((references (session-references)))
    (cond ((null references)
           (error "No reference picture is attached."))
          ((and (integerp n) (<= 1 n (length references)))
           (nth (1- n) references))
          (t (error "Reference pictures are numbered 1 to ~d, and ~s is not one of them."
                    (length references) n)))))

(defun chosen-references (which)
  "The reference pictures WHICH names: all of them with t, one by its
number, several by a list of numbers; none with nil."
  (cond ((null which) '())
        ((eq which t)
         (or (session-references) (error "No reference picture is attached.")))
        ((listp which) (mapcar #'reference-numbered which))
        (t (list (reference-numbered which)))))

(defun references-state (references &optional (shown? t))
  "What the model that plans is told of REFERENCES, the reference
pictures going with a prompt, or nil when there are none.  SHOWN? is
whether they are in the message for it to see."
  (when references
    (let ((count (length references)))
      (format nil "~a attached to this prompt~a: ~{~a~^; ~}.  (place-reference n) puts one in the document as a layer, its own pixels; generate-picture and generate-layer show them to the image generator with :references."
              (if (= count 1) "1 reference picture is" (format nil "~d reference pictures are" count))
              (cond ((not shown?) ", though this server's planner cannot be shown them")
                    ((= count 1) " and follows the document's picture in this message")
                    (t " and follow the document's picture in this message, in this order"))
              (loop for r in references
                    for n from 1
                    collect (format nil "~d, \"~a\", ~d x ~d" n (ref-name r) (ref-width r) (ref-height r)))))))

;;; The page's doors.

(defun references-json ()
  (json-string
   (json-object "max" *max-references*
                ;; a vector, so that none is [] and not null
                "references" (coerce (loop for r in (session-references)
                                           for n from 1
                                           collect (json-object "id" (ref-id r) "n" n "name" (ref-name r)
                                                                "width" (ref-width r) "height" (ref-height r)
                                                                "thumb" (data-url (ref-thumb r))))
                                     'vector))))

(defun handle-references (req)
  "GET /stratum/api/references: the reference pictures of the session
asking.  POST {remove: id} takes one away, {clear: true} all of them.
Either answers the list again."
  (when (eq (net.aserve:request-method req) :post)
    (let ((body (yason:parse (gwl:request-body-text req))))
      (unless (hash-table-p body)
        (error "Say which reference picture to remove."))
      (cond ((eq (gethash "clear" body) t) (clear-references!))
            ((stringp (gethash "remove" body)) (remove-reference! (gethash "remove" body)))
            (t (error "Say which reference picture to remove.")))))
  (references-json))

(defun handle-add-reference (req)
  "POST /stratum/api/add-reference?name=: the image in the body (a PNG
or JPEG file, or raw RGBA8 with ?w=&h=) attached to the session asking
as a reference picture; answers the list."
  (ensure-reference-room)
  (multiple-value-bind (pixels w h) (request-image req)
    (add-reference! pixels w h (net.aserve:request-query-value "name" req)))
  (references-json))

(defun publish-references! (server host)
  "The reference pictures' doors, behind the invite gate like every door
under /stratum."
  (publish-door server host "/stratum/api/references" "application/json" #'handle-references)
  (publish-door server host "/stratum/api/add-reference" "application/json" #'handle-add-reference
                :upload? t))

(pushnew 'publish-references! *stratum-publishers*)

;;; The Lisp API.

(defun stratum-api:references ()
  "The reference pictures attached to the prompt box, in the order they
were attached: ((:index 1 :name ... :width ... :height ...) ...), or nil."
  (loop for r in (session-references)
        for n from 1
        collect (list :index n :name (ref-name r) :width (ref-width r) :height (ref-height r))))

(defun stratum-api:place-reference (n &key name above)
  "Reference picture N (references lists them) as a new raster layer
NAME above layer ABOVE, or on top: centered, and reduced to fit inside
the document when it is larger.  The layer's layer-info."
  (let* ((doc (stratum-api::doc))
         (reference (reference-numbered n))
         (dw (the-object doc doc-width))
         (dh (the-object doc doc-height)))
    (multiple-value-bind (pixels w h) (read-png (ref-png reference))
      (let ((scale (min 1 (/ dw w) (/ dh h))))
        (when (< scale 1)
          (let ((nw (max 1 (floor (* w scale))))
                (nh (max 1 (floor (* h scale)))))
            (setf pixels (box-downscale pixels w h nw nh) w nw h nh))))
      (stratum-api:layer-info
       (the-object doc (add-layer! (list :kind :raster :name (or name (ref-name reference))
                                         :pixels (doc-pixels doc (fit-rgba pixels w h dw dh)))
                                   (and above (stratum-api::index (stratum-api::layer-of above)))))))))

;;; For Lisp callers (not operations of the prompt box: attaching is the
;;; page's).

(defun stratum-api:add-reference (source &key name)
  "SOURCE, a PNG file's path or its octets, attached as a reference
picture, NAME or the file's name.  The list as references gives it."
  (multiple-value-bind (pixels w h)
      (read-png (if (typep source '(or string pathname)) (stratum-api::file-octets source) source))
    (add-reference! pixels w h (or name (and (typep source '(or string pathname)) (pathname-name source)))))
  (stratum-api:references))

(defun stratum-api:remove-reference (n)
  "Reference picture N detached; the list as references gives it."
  (remove-reference! (ref-id (reference-numbered n)))
  (stratum-api:references))

(defun stratum-api:clear-references ()
  "Every reference picture detached."
  (clear-references!)
  nil)
