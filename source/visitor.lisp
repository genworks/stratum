;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; What is kept of a visitor, in their own hands: File > My Uploads lists
;; the pictures the activity log (activity.lisp) kept of them, with a
;; small picture of each, and deletes some, all, or everything kept of
;; them, their prompts in the prompt log (prompt.lisp) included.  Only an
;; invited visitor has any of this: with the gate open there is no one to
;; say whose pictures are whose.  The operator's own hand on the same
;; records is here too: forget-visitor! and purge-prompt-log!.  And
;; Help > Send a Note: what a visitor wants the operator to know, in
;; their own words, kept with the rest of their record.
;;

(defparameter *upload-preview-size* 160
  "The longer side, in pixels, of an upload's picture in My Uploads.")

(defun turn-who (record)
  "The name a prompt record begins with: log-turn! writes (:who NAME
...) first, so only that much is read, not the record's forms and
results (300 of them read whole took five seconds)."
  (ignore-errors
   (with-open-file (in record :external-format :utf-8)
     (with-standard-io-syntax
       (let ((*read-eval* nil) (*package* (find-package :keyword)))
         (and (eql (peek-char t in) #\()
              (read-char in)
              (eq (read in) :who)
              (let ((who (read in))) (and (stringp who) who))))))))

(defun visitor-turns (who)
  "The prompt log's directories of WHO's prompts."
  (when (and who *prompt-log-directory*)
    (remove-if-not
     (lambda (turn)
       (let ((record (probe-file (merge-pathnames "turn.sexp" turn))))
         (and record (equal who (turn-who record)))))
     (uiop:subdirectories *prompt-log-directory*))))

(defun visitor-uploads (who)
  "WHO's kept pictures, newest first, each (:file :time :door :bytes):
:file as the activity log names it, under the log's directory, and only
while the file is there."
  (when (and who (activity-directory))
    (let ((uploads '()))
      (dolist (line (activity-lines who) uploads)
        (let ((file (getf line :upload)))
          (when (and file (probe-file (merge-pathnames file (activity-directory))))
            (push (list :file file :time (getf line :time)
                        :door (subseq (getf line :door) (1+ (or (position #\/ (getf line :door) :from-end t) -1)))
                        :bytes (getf line :bytes))
                  uploads)))))))

(defun visitor-upload-path (who file)
  "The path of FILE, one of WHO's kept pictures by its listed name, or
an error: nothing else under the log can be named here."
  (unless (find file (visitor-uploads who) :key (lambda (u) (getf u :file)) :test #'equal)
    (error "That is not one of your pictures."))
  (merge-pathnames file (activity-directory)))

(defun upload-preview (path)
  "A small PNG of the picture at PATH, a JPEG or a PNG; nil for anything
else, or what will not decode."
  (ignore-errors
   (let* ((octets (with-open-file (in path :element-type '(unsigned-byte 8))
                    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                      (read-sequence v in)
                      v)))
          (type (upload-type octets)))
     (multiple-value-bind (pixels w h)
         (cond ((equal type "jpg")
                (multiple-value-bind (pixels w h) (decode-jpeg octets)
                  (oriented-rgba pixels w h (jpeg-orientation octets))))
               ((equal type "png") (read-png octets)))
       (when pixels
         (let* ((scale (min 1 (/ *upload-preview-size* (max w h))))
                (tw (max 1 (round (* w scale))))
                (th (max 1 (round (* h scale)))))
           (png-octets (rgba-thumb pixels w h tw th) tw th)))))))

(defun forget-visitor! (who)
  "Everything kept of WHO, the name on an invite: their activity file,
the pictures they opened, and their prompts in the prompt log.  What
was removed, as (:activity t-or-nil :uploads n :prompts n)."
  (let ((uploads 0) (prompts 0) (activity nil))
    (when (activity-directory)
      (setf activity (and (probe-file (merge-pathnames (format nil "~a.log" (visitor-file-name who))
                                                       (activity-directory)))
                          t)
            ;; with the log's lock, so no line is being written as its file goes
            uploads (getf (visitor-log:delete-visitor! *activity* who) :uploads)))
    (dolist (turn (visitor-turns who))
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname turn) :validate t)
      (incf prompts))
    (list :activity activity :uploads uploads :prompts prompts)))

(defun turn-stamp (time)
  "TIME as the prompt log's directory names begin, yyyymmdd-hhmmss in
UTC: a universal time, or a string of the digits however they are
punctuated, as much of the time as is said (\"2026-11-09\",
\"20261109-2330\", \"2026-11-09T23:30:00Z\")."
  (etypecase time
    (integer (multiple-value-bind (second minute hour day month year) (decode-universal-time time 0)
               (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)))
    (string (let ((digits (remove-if-not #'digit-char-p time)))
              (unless (<= 4 (length digits) 14)
                (error "~s is not a time: a year at least, to the second at most." time))
              (if (> (length digits) 8)
                  (format nil "~a-~a" (subseq digits 0 8) (subseq digits 8))
                  digits)))))

(defun purge-prompt-log! (&key (who nil who?) from before keep dry-run)
  "Prompts deleted from the prompt log, the operator's choice of them:
those of WHO, the name on an invite (nil: those with no name, asked
through an open gate); those from FROM on, and those before BEFORE,
times in UTC as turn-stamp reads them; and of those chosen, all but the
newest KEEP.  One of the four at least must be said: nothing is chosen
by default.  With DRY-RUN nothing is deleted.  What went, as (:deleted
n :oldest name :newest name :left n), or :would-delete with DRY-RUN."
  (unless (or who? from before keep)
    (error "Say which prompts: :who, :from, :before or :keep."))
  (check-type keep (or null (integer 0)))
  (let* ((from (and from (turn-stamp from)))
         (before (and before (turn-stamp before)))
         (all (and *prompt-log-directory* (uiop:subdirectories *prompt-log-directory*)))
         (chosen (sort (loop for turn in all
                             for name = (car (last (pathname-directory turn)))
                             when (and (or (null from) (string<= from name))
                                       (or (null before) (string< name before))
                                       (or (not who?)
                                           (equal who (turn-who (merge-pathnames "turn.sexp" turn)))))
                               collect (cons name turn))
                       #'string< :key #'car))
         (going (if keep (butlast chosen keep) chosen)))
    (unless dry-run
      (dolist (turn going)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname (cdr turn)) :validate t)))
    (list (if dry-run :would-delete :deleted) (length going)
          :oldest (car (first going)) :newest (car (first (last going)))
          :left (- (length all) (length going)))))

(defun my-uploads-json (who)
  (json-string
   (json-object "who" who
                "kept" (json-bool (and who (activity-directory)))
                ;; a vector, so that none is [] and not null
                "uploads" (map 'vector (lambda (u)
                                         (json-object "file" (getf u :file) "time" (getf u :time)
                                                      "door" (getf u :door) "bytes" (or (getf u :bytes) 0)))
                               (visitor-uploads who))
                "prompts" (length (visitor-turns who)))))

(defun handle-my-uploads (req)
  "GET /stratum/api/my-uploads: what is kept of the visitor asking.
POST {delete: [file ...]} deletes those of their pictures; {everything:
true} deletes everything kept of them.  Either answers the list again."
  (let ((who (invitee req)))
    (when (eq (net.aserve:request-method req) :post)
      (unless who
        (error "Only an invited visitor has pictures kept here."))
      (let ((body (yason:parse (gwl:request-body-text req))))
        (unless (hash-table-p body)
          (error "Say which pictures to delete."))
        (cond ((eq (gethash "everything" body) t) (forget-visitor! who))
              ((listp (gethash "delete" body))
               (let ((paths (mapcar (lambda (file)
                                      (unless (stringp file) (error "That is not one of your pictures."))
                                      (visitor-upload-path who file))
                                    (gethash "delete" body))))
                 (mapc #'delete-file paths)))
              (t (error "Say which pictures to delete.")))))
    (my-uploads-json who)))

(defun handle-my-upload (req)
  "GET /stratum/api/my-upload?file=: a small PNG of one of the asking
visitor's pictures; an empty answer when it cannot be shown."
  (let ((who (invitee req)))
    (unless who (error "Only an invited visitor has pictures kept here."))
    (or (upload-preview (visitor-upload-path who (net.aserve:request-query-value "file" req)))
        (make-array 0 :element-type '(unsigned-byte 8)))))

(defparameter *note-characters* 2000
  "The most a note's words may run to.")

(defparameter *note-picture-size* 1280
  "The longer side, in pixels, of the picture of the document a note
may carry.")

(defun handle-note (req)
  "POST /stratum/api/note?doc= {text, picture}: the visitor's note, kept
as a line of their record in the activity log -- the time, the document's
name, the words as :note -- and with PICTURE true a picture of the
document as it is now kept beside it, as an opened picture is.  Answers
{kept, picture}; kept is false when this server keeps no activity log,
so that the page can say the note went nowhere."
  (let* ((body (yason:parse (gwl:request-body-text req)))
         (text (and (hash-table-p body) (gethash "text" body)))
         (words (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (unless (and words (plusp (length words)))
      (error "Write something in the note first."))
    (when (> (length words) *note-characters*)
      (error "A note takes up to ~d characters; this one has ~d." *note-characters* (length words)))
    (let* ((kept (and (activity-directory) t))
           (png (and kept (eq (gethash "picture" body) t)
                     (document-picture-png (request-doc req) *note-picture-size*))))
      (when kept
        (visitor-log:note! *activity* *gate* req :upload png :more (list :note words)))
      (json-string (json-object "kept" (json-bool kept) "picture" (json-bool png))))))

(defun publish-visitor! (server host)
  "The My Uploads doors and the note door, behind the invite gate like
every door under /stratum."
  (publish-door server host "/stratum/api/my-uploads" "application/json" #'handle-my-uploads)
  (publish-door server host "/stratum/api/my-upload" "image/png" #'handle-my-upload)
  ;; not publish-door: the note writes its own line, words and picture
  ;; together, where publish-door would add the line every door gets
  (net.aserve:publish :path "/stratum/api/note" :server server :host host
                      :content-type "application/json"
                      :function (lambda (req ent)
                                  (when (door-admits? req ent *max-request-bytes*)
                                    (serve-door req ent "application/json" #'handle-note)))))

(pushnew 'publish-visitor! *stratum-publishers*)
