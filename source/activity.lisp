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
;; The log itself is Gendl's visitor-log (gwl/source/visitor-log.lisp),
;; one for Stratum, as invite.lisp holds Stratum's gate: what is here
;; says which requests are worth a line and what an upload's first
;; bytes name, and gives the log's functions Stratum's own names.  A
;; log this file wrote before it stood on visitor-log reads as it did:
;; the lines and the files beside them are the same.
;;

(defvar *activity*
  (visitor-log:make-visitor-log
   ;; The GET doors worth a line: a visit to the page and a download.
   ;; The other GETs are the page fetching what it shows, many to each
   ;; change.
   :reads '("/stratum" "/stratum/" "/stratum/api/export.png"
            "/stratum/api/save" "/stratum/api/save.ora")
   ;; How much of a request's body a line keeps: a stroke's points can
   ;; run to many times this.
   :body-characters 4000
   ;; An OpenRaster file is a zip; Stratum's own begins STRATUM1.  What
   ;; is neither these nor a JPEG or a PNG is the page's own packing,
   ;; "bin": the line's query says how to read it.
   :upload-types '(("ora" 80 75 3 4)
                   ("stratum" 83 84 82 65 84 85 77 49)))
  "Stratum's activity log, off until configure-activity-log! gives it a
directory.")

(defun configure-activity-log! (&key (directory nil directory?))
  "Keep the activity log under DIRECTORY, made if need be and closed to
everyone but its owner, or none with nil.  The directory."
  (when directory?
    (visitor-log:open-log! *activity* directory))
  (activity-directory))

(defun activity-directory ()
  "Where the activity log goes, or nil when none is kept."
  (visitor-log:visitor-log-directory *activity*))

(defun invitee (req)
  "The name on REQ's invite; nil with the gate open."
  (visitor-log:invitee *gate* req))

(defun visitor-file-name (who)
  "WHO, an invite's name or nil, as a file name: letters, digits, - and _
as they are, anything else _, and \"open-gate\" for nil."
  (visitor-log:visitor-file-name who))

(defun utc-stamp (&optional (time (get-universal-time)))
  (visitor-log:utc-stamp time))

(defun query-part (query name)
  "The value of NAME in QUERY, a query string as a line keeps it, or nil."
  (visitor-log:query-part query name))

(defun upload-type (octets)
  "The file extension for an upload's OCTETS, by its first bytes."
  (visitor-log:upload-type *activity* octets))

(defun note-activity! (req &key upload)
  "A line for REQ in its visitor's file, with UPLOAD, the octets of an
opened or placed picture, kept beside it.  Nothing when the log is off
or REQ is the page fetching what it shows.  A log that cannot be
written must not cost anyone their request."
  (visitor-log:note! *activity* *gate* req :upload upload))

(defun activity-lines (who)
  "The lines of WHO's activity file, oldest first, read with nothing
evaluated."
  (visitor-log:lines *activity* who))

(defun activity-line-mark (line)
  "A short word for LINE's content, the same while the line is: whoever
names a line by where it stands in its file says this too, and so cannot
take another for it once the file has changed."
  (visitor-log:line-mark line))

(defun delete-activity-line! (who index mark)
  "Line INDEX of WHO's activity file, counted from 0, taken out, and
the picture kept with it deleted; an error unless the line there has
MARK (activity-line-mark), as it will not once the file has been
rewritten.  The line."
  (visitor-log:delete-line! *activity* who index mark))

(defun activity-report ()
  "Each visitor in the activity log: (name :requests n :uploads n :first
time :last time :doors ((door . count) ...)), most recent first."
  (sort (loop for key in (visitor-log:visitor-keys *activity*)
              for lines = (activity-lines key)
              when lines
                collect (let ((doors '()))
                          (dolist (line lines)
                            (let ((cell (assoc (getf line :door) doors :test #'equal)))
                              (if cell (incf (cdr cell)) (push (cons (getf line :door) 1) doors))))
                          (list (or (getf (first lines) :who) key)
                                :requests (length lines)
                                :uploads (count-if (lambda (l) (getf l :upload)) lines)
                                :first (getf (first lines) :time)
                                :last (getf (car (last lines)) :time)
                                :doors (sort doors #'> :key #'cdr))))
        #'string> :key (lambda (entry) (getf (cdr entry) :last))))
