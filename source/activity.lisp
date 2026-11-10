;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; The activity log: what each visitor did, by the name on their invite.
;; The prompt log (prompt.lisp) keeps each prompt whole; this keeps
;; everything else as well, one line a request to a file of the
;; visitor's own -- each visit to the page, each change made by hand
;; (strokes, filters, layers, selections, the Generate dialog), each
;; download -- and each picture they open or place, as they sent it.
;; Off by default (configure-activity-log!): keeping a visitor's
;; pictures is the operator's decision, and something the visitors
;; should be told.  No invite token is written anywhere.
;;

(defvar *activity-directory* nil
  "Where the activity log goes (configure-activity-log!), or nil for none,
the default.")

(defparameter *activity-body-characters* 4000
  "How much of a request's body a line keeps: a stroke's points can run
to many times this.")

(defparameter *activity-reads* '("/stratum" "/stratum/" "/stratum/api/export.png"
                                 "/stratum/api/save" "/stratum/api/save.ora")
  "The GET doors worth a line: a visit to the page and a download.  The
other GETs are the page fetching what it shows, many to each change.")

(defvar *activity-lock* (bt:make-lock "stratum activity")
  "Held while a line is written, so lines from two requests do not mix.")

(defun configure-activity-log! (&key (directory nil directory?))
  "Keep the activity log under DIRECTORY, made if need be and closed to
everyone but its owner, or none with nil.  The directory."
  (when directory?
    (let ((path (and directory (uiop:ensure-directory-pathname directory))))
      (when path
        (ensure-directories-exist path)
        #+unix (uiop:run-program (list "chmod" "700" (uiop:native-namestring path))
                                 :ignore-error-status t))
      (setf *activity-directory* path)))
  *activity-directory*)

(defun invitee (req)
  "The name on REQ's invite; nil with the gate open."
  (second (find-invite (invite-gate:request-token *gate* req))))

(defun visitor-file-name (who)
  "WHO, an invite's name or nil, as a file name: letters, digits, - and _
as they are, anything else _, and \"open-gate\" for nil."
  (if who
      (map 'string (lambda (c) (if (or (alphanumericp c) (member c '(#\- #\_))) c #\_)) who)
      "open-gate"))

(defun utc-stamp (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour day month year) (decode-universal-time time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour minute second)))

(defun query-without-invite (req)
  "REQ's query string with any invite token taken out."
  (let ((query (net.uri:uri-query (net.aserve:request-uri req))))
    (and query
         (let ((kept (remove-if (lambda (pair) (string-equal "invite=" pair :end2 (min 7 (length pair))))
                                (uiop:split-string query :separator "&"))))
           (and kept (format nil "~{~a~^&~}" kept))))))

(defun read-body-text (req)
  "The body a door already read, as text, or nil: never a read of its own,
which could wait on a body nobody meant to read."
  (let ((raw (net.aserve::request-request-body req)))
    (when (and (stringp raw) (plusp (length raw)))
      (let ((text (gwl:request-body-text req)))
        (if (> (length text) *activity-body-characters*)
            (format nil "~a ... (~d characters in all)" (subseq text 0 *activity-body-characters*)
                    (length text))
            text)))))

(defun upload-type (octets)
  "The file extension for an upload's OCTETS, by its first bytes."
  (flet ((starts (&rest bytes)
           (and (>= (length octets) (length bytes))
                (every #'= bytes (subseq octets 0 (length bytes))))))
    (cond ((starts #xff #xd8) "jpg")
          ((starts 137 80 78 71) "png")
          ((starts 80 75 3 4) "ora")
          ((starts 83 84 82 65 84 85 77 49) "stratum")
          ;; the page's own packing; the line's query says how to read it
          (t "bin"))))

(defun print-activity-line (line out)
  "LINE, a plist, written to OUT as the activity log keeps one."
  (with-standard-io-syntax
    (let ((*print-readably* nil) (*package* (find-package :keyword)) (*print-case* :downcase))
      (prin1 line out)
      (terpri out))))

(defun note-activity! (req &key upload)
  "A line for REQ in its visitor's file, with UPLOAD, the octets of an
opened or placed picture, kept beside it.  Nothing when the log is off
or REQ is the page fetching what it shows.  A log that cannot be
written must not cost anyone their request."
  (when (and *activity-directory*
             (or upload
                 (not (eq (net.aserve:request-method req) :get))
                 (member (net.uri:uri-path (net.aserve:request-uri req)) *activity-reads*
                         :test #'equal)))
    (handler-case
        (let* ((who (invitee req))
               (name (visitor-file-name who))
               (time (get-universal-time))
               (door (net.uri:uri-path (net.aserve:request-uri req)))
               (kept (and upload
                          (format nil "~a/~a-~a-~36r.~a" name (remove #\: (utc-stamp time))
                                  (subseq door (1+ (or (position #\/ door :from-end t) -1)))
                                  (random (expt 36 4) (make-random-state t))
                                  (upload-type upload))))
               (line (list :time (utc-stamp time) :who who
                           :method (net.aserve:request-method req) :door door
                           :query (query-without-invite req)
                           :body (and (not upload) (read-body-text req))
                           :upload kept :bytes (and upload (length upload)))))
          (bt:with-lock-held (*activity-lock*)
            (when kept
              (let ((path (merge-pathnames kept *activity-directory*)))
                (ensure-directories-exist path)
                (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)
                  (write-sequence upload out))))
            (with-open-file (out (merge-pathnames (format nil "~a.log" name) *activity-directory*)
                                 :direction :output :if-exists :append :if-does-not-exist :create
                                 :external-format :utf-8)
              (print-activity-line (loop for (key value) on line by #'cddr when value append (list key value))
                                   out))))
      (serious-condition (e)
        (let ((*print-readably* nil))
          (format *error-output* "~&[stratum] activity log: ~a~%" e))))))

(defun activity-lines (who)
  "The lines of WHO's activity file, oldest first, read with nothing
evaluated."
  (let ((file (and *activity-directory*
                   (probe-file (merge-pathnames (format nil "~a.log" (visitor-file-name who))
                                                *activity-directory*)))))
    (when file
      (with-open-file (in file :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*read-eval* nil) (*package* (find-package :keyword)))
            (loop for line = (read in nil in)
                  until (eq line in)
                  collect line)))))))

(defun activity-line-mark (line)
  "A short word for LINE's content, the same while the line is: whoever
names a line by where it stands in its file says this too, and so cannot
take another for it once the file has changed."
  (format nil "~36r" (sxhash (with-output-to-string (out) (print-activity-line line out)))))

(defun delete-activity-line! (who index mark)
  "Line INDEX of WHO's activity file, counted from 0, taken out, and
the picture kept with it deleted; an error unless the line there has
MARK (activity-line-mark), as it will not once the file has been
rewritten.  The file is written anew beside itself and put in its place
whole; the last line gone, the file goes.  The line."
  (bt:with-lock-held (*activity-lock*)
    (let* ((lines (activity-lines who))
           (line (and (integerp index) (<= 0 index) (nth index lines))))
      (unless (and line (equal mark (activity-line-mark line)))
        (error "That line is not where it was: the record has changed.  Refresh and try again."))
      (let ((file (merge-pathnames (format nil "~a.log" (visitor-file-name who)) *activity-directory*))
            (kept (append (subseq lines 0 index) (nthcdr (1+ index) lines)))
            (upload (getf line :upload)))
        (if kept
            (let ((anew (merge-pathnames (format nil "~a.rewrite" (visitor-file-name who))
                                         *activity-directory*)))
              (with-open-file (out anew :direction :output :if-exists :supersede :external-format :utf-8)
                (dolist (line kept) (print-activity-line line out)))
              (uiop:rename-file-overwriting-target anew file))
            (delete-file file))
        (when (and upload (notany (lambda (line) (equal upload (getf line :upload))) kept))
          (let ((picture (probe-file (merge-pathnames upload *activity-directory*))))
            (when picture (delete-file picture))))
        line))))

(defun activity-report ()
  "Each visitor in the activity log: (name :requests n :uploads n :first
time :last time :doors ((door . count) ...)), most recent first."
  (when *activity-directory*
    (sort (loop for file in (directory (merge-pathnames "*.log" *activity-directory*))
                for lines = (activity-lines (pathname-name file))
                when lines
                  collect (let ((doors '()))
                            (dolist (line lines)
                              (let ((cell (assoc (getf line :door) doors :test #'equal)))
                                (if cell (incf (cdr cell)) (push (cons (getf line :door) 1) doors))))
                            (list (or (getf (first lines) :who) (pathname-name file))
                                  :requests (length lines)
                                  :uploads (count-if (lambda (l) (getf l :upload)) lines)
                                  :first (getf (first lines) :time)
                                  :last (getf (car (last lines)) :time)
                                  :doors (sort doors #'> :key #'cdr))))
          #'string> :key (lambda (entry) (getf (cdr entry) :last)))))
