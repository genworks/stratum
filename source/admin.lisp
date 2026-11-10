;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; The operator's page, /stratum/admin: who has visited, the sessions
;; they worked in, what they did by hand and what they asked for, each
;; prompt with its forms and how they went, and the sessions in memory
;; with their documents' layers.  It reads what is already kept -- the
;; gate's roster and counts (invite.lisp), the activity log
;; (activity.lisp), the prompt log (prompt.lisp) -- and the sessions
;; table; it keeps nothing of its own.  The one thing it changes is what
;; it is asked to delete from those two logs: a line, a prompt, or
;; everything kept of a visitor.  Only a name on *admins* gets any of it;
;; anyone else, invited or not, is told there is no such page.  No invite
;; token is in anything it answers.  The page's look and the pieces its
;; script is built from are Gendl's visitor-log's, shared with other
;; applications' admin pages; web/admin.js holds what is Stratum's.
;;

(defvar *admins* nil
  "Who may see the admin page: a list of invite names; t for anyone the
gate admits, for a development room with no gate; or nil for no one, the
default.")

(defparameter *admin-events* 300
  "How many of a visitor's lines and prompts the admin page is sent
unless it asks for more.")

(defun admin? (req)
  "Whether REQ's person may see the admin page."
  (or (eq *admins* t)
      (and (consp *admins*)
           (member (invitee req) *admins* :test #'equal)
           t)))

(defun query-part (query name)
  "The value of NAME in QUERY, a query string as the activity log keeps
it, or nil."
  (when query
    (let ((prefix (format nil "~a=" name)))
      (dolist (pair (uiop:split-string query :separator "&"))
        (when (and (> (length pair) (length prefix))
                   (string= prefix pair :end2 (length prefix)))
          (let ((value (subseq pair (length prefix))))
            (return (or (ignore-errors (net.aserve:uridecode-string value)) value))))))))

(defun line-kind (line)
  "What a line of the activity log was: :visit, the page asked for;
:download; :upload, a picture opened or placed; :asked, a prompt sent;
:change, anything else done by hand."
  (let ((door (getf line :door)))
    (cond ((getf line :upload) :upload)
          ((member door '("/stratum" "/stratum/") :test #'equal) :visit)
          ((eq (getf line :method) :get) :download)
          ((equal door "/stratum/api/prompt") :asked)
          (t :change))))

;;; The prompt log, read back.

(defun turn-name (turn)
  "The name of TURN, a prompt's directory in the prompt log."
  (car (last (pathname-directory turn))))

(defun turn-time (turn)
  "When the prompt of TURN ended, as the activity log writes times; nil
for a directory not named as log-turn! names them."
  (let ((name (turn-name turn)))
    (and (stringp name) (> (length name) 15) (char= (char name 8) #\-)
         (every #'digit-char-p (subseq name 0 8))
         (every #'digit-char-p (subseq name 9 15))
         (format nil "~a-~a-~aT~a:~a:~aZ" (subseq name 0 4) (subseq name 4 6) (subseq name 6 8)
                 (subseq name 9 11) (subseq name 11 13) (subseq name 13 15)))))

(defun turn-head (turn)
  "(:who name :session id :document name) of the prompt in TURN, by the
first words of its record alone (see turn-who)."
  (ignore-errors
   (with-open-file (in (merge-pathnames "turn.sexp" turn) :external-format :utf-8)
     (with-standard-io-syntax
       (let ((*read-eval* nil) (*package* (find-package :common-lisp)) (head '()))
         (when (eql (peek-char t in) #\()
           (read-char in)
           (loop
             (when (eql (peek-char t in) #\)) (return))
             (let ((key (read in)))
               (unless (member key '(:who :session :document)) (return))
               (setf head (list* (read in) key head))))
           (nreverse head)))))))

(defun read-turn (turn)
  "The whole record of the prompt in TURN, or nil when it will not
read.  In the package log-turn! wrote it in, with nothing evaluated."
  (ignore-errors
   (with-open-file (in (merge-pathnames "turn.sexp" turn) :external-format :utf-8)
     (with-standard-io-syntax
       (let ((*read-eval* nil) (*package* (find-package :common-lisp)))
         (let ((record (read in)))
           (and (listp record) (evenp (length record)) record)))))))

(defun logged-turns ()
  "Every prompt in the prompt log, oldest first, each (:turn name :time
time :who name :session id :document name)."
  (when *prompt-log-directory*
    (sort (loop for turn in (uiop:subdirectories *prompt-log-directory*)
                when (probe-file (merge-pathnames "turn.sexp" turn))
                  collect (list* :turn (turn-name turn) :time (turn-time turn) (turn-head turn)))
          #'string< :key (lambda (turn) (getf turn :turn)))))

(defun turn-directory-named (name)
  "The directory of the prompt NAME, or nil: found among the log's own,
so nothing else can be named."
  (and (stringp name) *prompt-log-directory*
       (find name (uiop:subdirectories *prompt-log-directory*) :key #'turn-name :test #'equal)))

(defun turn-pictures (dir)
  "The names of the pictures kept in DIR, a prompt's directory, in the
order the model met them: before, reference-1 ..., look-1 ..., after."
  (let ((names (loop for file in (uiop:directory-files dir)
                     when (equal (pathname-type file) "png") collect (pathname-name file))))
    (flet ((numbered (prefix)
             (sort (remove-if-not (lambda (name) (eql 0 (search prefix name))) names)
                   #'< :key (lambda (name)
                              (or (parse-integer name :start (length prefix) :junk-allowed t) 0)))))
      (append (and (member "before" names :test #'equal) (list "before"))
              (numbered "reference-")
              (numbered "look-")
              (and (member "after" names :test #'equal) (list "after"))))))

(defun read-octets-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun turn-picture (turn name)
  "The PNG octets of picture NAME of the prompt TURN, both as the admin
page was told them."
  (let ((dir (turn-directory-named turn)))
    (unless (and dir (member name (turn-pictures dir) :test #'equal))
      (error "There is no such picture."))
    (read-octets-file (merge-pathnames (format nil "~a.png" name) dir))))

;;; What is kept, summed up.

(defun plain-text (value)
  "VALUE for the page: a string as it is, nil as nil, anything else
printed."
  (cond ((null value) nil)
        ((stringp value) value)
        (t (let ((*print-readably* nil) (*print-case* :downcase)) (princ-to-string value)))))

(defun plain-number (value)
  (and (realp value) (if (integerp value) value (float value 1d0))))

(defun session-rollup (lines turns)
  "The sessions that LINES, a visitor's activity, and TURNS, their
prompts, name: each (id :first time :last time :requests n :prompts n
:documents (name ...)), the most recent first."
  (let ((sessions (make-hash-table :test 'equal)))
    (flet ((note (id time document count)
             (when (and (stringp id) (plusp (length id)))
               (let ((entry (or (gethash id sessions)
                                (setf (gethash id sessions)
                                      (list :first time :last time :requests 0 :prompts 0 :documents '())))))
                 (when (and time (or (null (getf entry :first)) (string< time (getf entry :first))))
                   (setf (getf entry :first) time))
                 (when (and time (or (null (getf entry :last)) (string> time (getf entry :last))))
                   (setf (getf entry :last) time))
                 (incf (getf entry count))
                 (when (and (stringp document) (not (member document (getf entry :documents) :test #'equal)))
                   (setf (getf entry :documents) (append (getf entry :documents) (list document))))
                 (setf (gethash id sessions) entry)))))
      (dolist (line lines)
        (note (query-part (getf line :query) "s") (getf line :time)
              (query-part (getf line :query) "doc") :requests))
      (dolist (turn turns)
        (note (getf turn :session) (getf turn :time) (getf turn :document) :prompts)))
    (sort (loop for id being the hash-keys of sessions using (hash-value entry)
                collect (cons id entry))
          #'string> :key (lambda (entry) (or (getf (cdr entry) :last) "")))))

(defun layer-summary (layer)
  (let ((kind (the-object layer kind)))
    (json-object "name" (the-object layer layer-name)
                 "kind" (string-downcase kind)
                 "blend" (when (eql kind :raster) (string-downcase (the-object layer blend-mode)))
                 "opacity" (float (the-object layer opacity) 1d0)
                 "visible" (json-bool (the-object layer visible?))
                 "mask" (json-bool (the-object layer has-mask?)))))

(defun document-summary (doc)
  "DOC and its layers, the bottom one first, for the admin page."
  (json-object "name" (the-object doc doc-name)
               "width" (the-object doc doc-width)
               "height" (the-object doc doc-height)
               "mode" (string-downcase (the-object doc color-mode))
               "undo" (length (the-object doc undo-entries))
               "prompts" (length (the-object doc prompt-log))
               "layers" (map 'vector #'layer-summary (the-object doc layer-list))))

(defun live-sessions ()
  "The sessions in memory, the shared one first under the id nil, then
the most recently touched: each (id touched documents), documents a list
of summaries, or :busy when a request has the session's lock just now.
The lock is tried and never waited for, and looking touches no session,
so none is kept alive by being looked at."
  (let ((sessions (bt:with-lock-held (*sessions-lock*)
                    (let ((all '()))
                      (maphash (lambda (id session) (push (cons id session) all)) *sessions*)
                      (cons (cons nil *shared-session*)
                            (sort all #'> :key (lambda (entry) (sess-touched (cdr entry)))))))))
    (loop for (id . session) in sessions
          collect (list id (sess-touched session)
                        (if (bt:acquire-lock (sess-lock session) nil)
                            (unwind-protect
                                 (with-session (session)
                                   (let ((documents '()))
                                     (maphash (lambda (name doc)
                                                (push (cons name
                                                            (handler-case (document-summary doc)
                                                              (serious-condition (e)
                                                                (json-object "name" name
                                                                             "error" (princ-to-string e)))))
                                                      documents))
                                              (sess-table session))
                                     (mapcar #'cdr (sort documents #'string< :key #'car))))
                              (bt:release-lock (sess-lock session)))
                            :busy)))))

(defun admin-summary ()
  "Everyone the roster, the activity log or the prompt log names, summed
up, and the sessions in memory: a JSON object for the admin page."
  (let* ((turns (logged-turns))
         (visitors (make-hash-table :test 'equal))
         (order '())
         (whose (make-hash-table :test 'equal)))
    (flet ((note (key indicator value &key unless-set)
             (unless (nth-value 1 (gethash key visitors)) (push key order))
             (unless (and unless-set (getf (gethash key visitors) indicator))
               (setf (getf (gethash key visitors) indicator) value))))
      (when (invite-gate?)
        (dolist (invite (invites-report))
          (let ((key (visitor-file-name (getf invite :name))))
            (note key :name (getf invite :name))
            (note key :invite invite))))
      (when *activity-directory*
        (dolist (file (directory (merge-pathnames "*.log" *activity-directory*)))
          (let* ((key (pathname-name file))
                 (lines (activity-lines key)))
            (note key :lines lines)
            (note key :name (getf (first lines) :who) :unless-set t))))
      (dolist (turn turns)
        (let ((key (visitor-file-name (getf turn :who))))
          (note key :turns (cons turn (getf (gethash key visitors) :turns)))
          (note key :name (getf turn :who) :unless-set t))))
    (let ((rows
            (loop for key in (reverse order)
                  for entry = (gethash key visitors)
                  for lines = (getf entry :lines)
                  for mine = (reverse (getf entry :turns))
                  for invite = (getf entry :invite)
                  for sessions = (session-rollup lines mine)
                  for times = (remove nil (append (mapcar (lambda (line) (getf line :time)) lines)
                                                  (mapcar (lambda (turn) (getf turn :time)) mine)))
                  do (dolist (session sessions)
                       (setf (gethash (car session) whose) (or (getf entry :name) key)))
                  collect (flet ((kind (kind) (count kind lines :key #'line-kind)))
                            (json-object
                             "key" key
                             "name" (getf entry :name)
                             "invited" (json-bool invite)
                             "issued" (plain-text (getf invite :issued))
                             "uses" (and invite (getf invite :uses))
                             "seen" (and (integerp (getf invite :last)) (plusp (getf invite :last))
                                         (utc-stamp (getf invite :last)))
                             "visits" (kind :visit)
                             "changes" (kind :change)
                             "uploads" (kind :upload)
                             "downloads" (kind :download)
                             "asked" (kind :asked)
                             "prompts" (length mine)
                             "first" (and times (reduce (lambda (a b) (if (string< a b) a b)) times))
                             "last" (and times (reduce (lambda (a b) (if (string> a b) a b)) times))
                             "sessions" (map 'vector
                                             (lambda (session)
                                               (json-object "id" (car session)
                                                            "first" (getf (cdr session) :first)
                                                            "last" (getf (cdr session) :last)
                                                            "requests" (getf (cdr session) :requests)
                                                            "prompts" (getf (cdr session) :prompts)
                                                            "documents" (coerce (getf (cdr session) :documents)
                                                                                'vector)))
                                             sessions))))))
      (json-object
       "now" (utc-stamp)
       "server" (json-object "lisp" (format nil "~a ~a" (lisp-implementation-type)
                                            (lisp-implementation-version))
                             "heapUsed" (heap-used-fraction)
                             "gate" (json-bool (invite-gate?))
                             "activityLog" (json-bool *activity-directory*)
                             "promptLog" (json-bool *prompt-log-directory*)
                             "promptsKept" (length turns)
                             "promptLimit" *prompt-log-turns*
                             "idleSeconds" *session-idle-seconds*)
       ;; the most recently active first; then those with nothing
       ;; recorded, by when their link was last used (an admin who only
       ;; looks is among these)
       "visitors" (coerce (stable-sort rows #'string>
                                       :key (lambda (row)
                                              (format nil "~a ~a" (or (gethash "last" row) "")
                                                      (or (gethash "seen" row) ""))))
                          'vector)
       "sessions" (map 'vector
                       (lambda (session)
                         (destructuring-bind (id touched documents) session
                           (json-object "id" id
                                        "who" (and id (gethash id whose))
                                        "touched" (utc-stamp touched)
                                        "busy" (json-bool (eq documents :busy))
                                        "documents" (if (eq documents :busy)
                                                        (vector)
                                                        (coerce documents 'vector)))))
                       (live-sessions))))))

;;; One visitor, line by line.

(defun line-json (line)
  "A line of the activity log for the admin page."
  (let ((door (or (getf line :door) ""))
        (query (getf line :query)))
    (json-object "time" (getf line :time)
                 "kind" (string-downcase (line-kind line))
                 "door" (subseq door (1+ (or (position #\/ door :from-end t) -1)))
                 "session" (query-part query "s")
                 "document" (query-part query "doc")
                 "query" query
                 "body" (plain-text (getf line :body))
                 "file" (getf line :upload)
                 "bytes" (plain-number (getf line :bytes))
                 ;; what the delete door is told to name this line
                 "line" (getf line :index)
                 "mark" (getf line :mark))))

(defun turn-json (turn)
  "The prompt of TURN, an entry of logged-turns, with the whole of its
record: the words, who planned the forms (\"no planner\" for the Generate
dialog's, which no model wrote), each form with how it went, the reply,
and the names of its pictures."
  (let* ((dir (turn-directory-named (getf turn :turn)))
         (record (and dir (read-turn dir)))
         (outcome (let ((outcome (getf record :outcome))) (and (listp outcome) outcome))))
    (json-object "time" (getf turn :time)
                 "kind" "prompt"
                 "turn" (getf turn :turn)
                 "session" (plain-text (getf turn :session))
                 "document" (plain-text (getf turn :document))
                 "prompt" (plain-text (getf record :prompt))
                 "model" (plain-text (getf record :model))
                 "byHand" (json-bool (equal (getf record :model) "no planner"))
                 "effort" (plain-text (getf record :effort))
                 "seconds" (plain-number (getf record :seconds))
                 "rounds" (plain-number (getf record :rounds))
                 "failure" (plain-text (getf record :failure))
                 "reply" (plain-text (getf outcome :reply))
                 "ok" (json-bool (getf outcome :ok))
                 "declined" (json-bool (getf outcome :declined))
                 "stopped" (json-bool (getf outcome :stopped))
                 "results" (map 'vector
                                (lambda (result)
                                  (json-object "form" (plain-text (getf result :form))
                                               "ok" (json-bool (getf result :ok))
                                               "value" (plain-text (getf result :value))
                                               "error" (plain-text (getf result :error))))
                                (let ((results (getf outcome :results)))
                                  (and (listp results) (remove-if-not #'listp results))))
                 "state" (plain-text (getf record :state))
                 "looks" (map 'vector #'plain-text (let ((looks (getf record :looks))) (and (listp looks) looks)))
                 "pictures" (coerce (and dir (turn-pictures dir)) 'vector))))

(defun visitor-events (key &optional (limit *admin-events*))
  "The last LIMIT things kept of the visitor whose file is KEY, oldest
first: the lines of their activity file and their prompts in the prompt
log, by time.  (values events how-many-in-all)."
  (let* ((lines (loop for line in (activity-lines key)
                      for index from 0
                      collect (list* :index index :mark (activity-line-mark line) line)))
         (turns (remove-if-not (lambda (turn) (equal key (visitor-file-name (getf turn :who))))
                               (logged-turns)))
         (events (stable-sort (append (mapcar (lambda (line) (cons (or (getf line :time) "") line)) lines)
                                      (mapcar (lambda (turn) (cons (or (getf turn :time) "") turn)) turns))
                              #'string< :key #'car)))
    (values (mapcar (lambda (event)
                      (if (getf (cdr event) :turn) (turn-json (cdr event)) (line-json (cdr event))))
                    (last events (max 1 limit)))
            (length events))))

(defun kept-upload-path (key file)
  "The path of FILE, a picture kept of the visitor whose file is KEY, by
its listed name, or an error."
  (unless (and (stringp key) (stringp file)
               (find file (visitor-uploads key) :key (lambda (upload) (getf upload :file)) :test #'equal))
    (error "There is no such picture."))
  (merge-pathnames file *activity-directory*))

;;; What is kept, deleted.

(defun visitor-key (key)
  "KEY when it is a visitor's file name as the summary gives them, or an
error: nothing else is looked for under the logs."
  (unless (and (stringp key) (plusp (length key)) (equal key (visitor-file-name key)))
    (error "Say which visitor: who, as the summary names them."))
  key)

(defun kept-turns (key)
  "The prompt log's directories of the prompts the page shows under the
visitor whose file is KEY."
  (when *prompt-log-directory*
    (remove-if-not (lambda (turn)
                     (and (probe-file (merge-pathnames "turn.sexp" turn))
                          (equal key (visitor-file-name (getf (turn-head turn) :who)))))
                   (uiop:subdirectories *prompt-log-directory*))))

(defun delete-turn! (key name)
  "The prompt NAME deleted from the prompt log, its pictures with it;
an error unless it is one of the visitor's whose file is KEY."
  (let ((turn (find name (kept-turns key) :key #'turn-name :test #'equal)))
    (unless turn
      (error "That visitor has no such prompt."))
    (uiop:delete-directory-tree (uiop:ensure-directory-pathname turn) :validate t)
    name))

(defun delete-kept! (key)
  "Everything the page shows of the visitor whose file is KEY, deleted:
their activity file, the pictures they opened, and their prompts in the
prompt log.  Their invite stays, and so does a session of theirs in
memory.  What went, as (:lines n :uploads n :prompts n)."
  (let ((lines 0) (uploads 0) (prompts 0))
    (when *activity-directory*
      ;; with the log's lock, so no line is being written as its file goes
      (bt:with-lock-held (*activity-lock*)
        (let ((file (probe-file (merge-pathnames (format nil "~a.log" key) *activity-directory*)))
              (pictures (probe-file (merge-pathnames (format nil "~a/" key) *activity-directory*))))
          (when file
            (setf lines (length (activity-lines key)))
            (delete-file file))
          (when pictures
            (setf uploads (length (uiop:directory-files pictures)))
            (uiop:delete-directory-tree (uiop:ensure-directory-pathname pictures) :validate t)))))
    (dolist (turn (kept-turns key))
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname turn) :validate t)
      (incf prompts))
    (list :lines lines :uploads uploads :prompts prompts)))

;;; The doors.

(defun handle-admin-summary (req)
  "GET /stratum/api/admin: {now, server, visitors, sessions}."
  (declare (ignore req))
  (json-string (admin-summary)))

(defun handle-admin-visitor (req)
  "GET /stratum/api/admin/visitor?who=KEY&limit=N: {who, total, events},
the last N things kept of one visitor, oldest first."
  (let ((key (net.aserve:request-query-value "who" req :post nil))
        (limit (or (ignore-errors
                    (parse-integer (net.aserve:request-query-value "limit" req :post nil)))
                   *admin-events*)))
    (multiple-value-bind (events total) (visitor-events (visitor-key key) (max 1 (min limit 20000)))
      (json-string (json-object "who" key "total" total "events" (coerce events 'vector))))))

(defun handle-admin-picture (req)
  "GET /stratum/api/admin/picture?turn=NAME&name=before: a picture kept
with a prompt."
  (turn-picture (net.aserve:request-query-value "turn" req :post nil)
                (net.aserve:request-query-value "name" req :post nil)))

(defun handle-admin-upload (req)
  "GET /stratum/api/admin/upload?who=KEY&file=NAME&size=N: a PNG of a
picture a visitor opened, N pixels on its longer side; an empty answer
when it cannot be shown."
  (let ((size (or (ignore-errors (parse-integer (net.aserve:request-query-value "size" req :post nil)))
                  *upload-preview-size*)))
    (or (let ((*upload-preview-size* (max 40 (min size 1280))))
          (upload-preview (kept-upload-path (net.aserve:request-query-value "who" req :post nil)
                                            (net.aserve:request-query-value "file" req :post nil))))
        (make-array 0 :element-type '(unsigned-byte 8)))))

(defun handle-admin-delete (req)
  "POST /stratum/api/admin/delete, a JSON body naming a visitor as the
summary does and what of theirs is to go: {who: KEY, everything: true},
everything kept of them; {who: KEY, line: N, mark: WORD}, one line of
their activity file, as the visitor door numbered and marked it, and the
picture kept with it; {who: KEY, turn: NAME}, one of their prompts and
its pictures.  {deleted: {lines, uploads, prompts}}.  Nothing but a POST
of JSON is taken, which a form on someone else's page cannot send."
  (unless (and (eq (net.aserve:request-method req) :post)
               (search "application/json" (or (net.aserve:header-slot-value req :content-type) "")
                       :test #'char-equal))
    (error "Deleting takes a POST of JSON."))
  (let* ((body (ignore-errors (yason:parse (gwl:request-body-text req))))
         (key (visitor-key (and (hash-table-p body) (gethash "who" body))))
         (gone (cond ((eq (gethash "everything" body) t) (delete-kept! key))
                     ((gethash "turn" body)
                      (delete-turn! key (gethash "turn" body))
                      (list :lines 0 :uploads 0 :prompts 1))
                     ((gethash "line" body)
                      (let ((line (delete-activity-line! key (gethash "line" body) (gethash "mark" body))))
                        (list :lines 1 :uploads (if (getf line :upload) 1 0) :prompts 0)))
                     (t (error "Say what is to go: everything, a line or a turn.")))))
    (json-string (json-object "deleted" (json-object "lines" (getf gone :lines)
                                                     "uploads" (getf gone :uploads)
                                                     "prompts" (getf gone :prompts))))))

(defparameter *admin-css* "
.ad-session.ad-none { border-color: #4a5058; }
.ad-pictures { display: flex; flex-wrap: wrap; gap: 8px; margin: 5px 0; }
.ad-pictures figure { margin: 0; }
.ad-pictures img { display: block; max-width: 180px; max-height: 140px; border: 1px solid #3a3e45;
  background: #17181b; border-radius: 3px; }
.ad-pictures figcaption { font-size: 11px; color: #8a9099; text-align: center; }
@media (max-width: 640px) {
  .ad-pictures img { max-width: 44vw; }
}
"
  "What the admin page shows that no other application's does, pictures,
after the look every such page has (visitor-log:*admin-css*).")

(defun admin-html ()
  "The page: visitor-log's, its script of helpers ahead of ours."
  (visitor-log:admin-page
   "Stratum admin"
   (list "/stratum/admin-kit.js"
         (format nil "/stratum/admin.js?v=~a"
                 (file-write-date (merge-pathnames "web/admin.js" *stratum-dir*))))
   :css *admin-css*))

(defun serve-admin (req ent content-type fn)
  "FN, a function of REQ answering a string or octets, for someone on
*admins*; anyone else is told what the prefix door tells them of a path
nobody published.  No session's lock is taken, and no line is written in
the activity log."
  (if (not (admin? req))
      (reply-text req ent net.aserve:*response-not-found* "Not found." :drain *refused-body-bytes*)
      (let ((result nil) (failure nil))
        (handler-case (setf result (funcall fn req))
          (serious-condition (e) (setf failure (princ-to-string e))))
        (if failure
            (reply-text req ent net.aserve:*response-internal-server-error* failure)
            (net.aserve:with-http-response (req ent :content-type content-type)
              (no-store! req)
              (net.aserve:with-http-body (req ent)
                (let ((s (net.aserve:request-reply-stream req)))
                  (if (stringp result)
                      (write-string result s)
                      (write-sequence result s)))))))))

(defun publish-admin! (server host)
  "The admin page and its doors: behind the invite gate like every door
under /stratum, and then for *admins* alone."
  (flet ((pub (path content-type fn)
           (net.aserve:publish :path path :server server :host host
                               :content-type content-type
                               :function (lambda (req ent)
                                           (when (door-admits? req ent *max-request-bytes*)
                                             (serve-admin req ent content-type fn))))))
    (pub "/stratum/admin" "text/html"
         (lambda (req) (declare (ignore req)) (admin-html)))
    (pub "/stratum/admin-kit.js" "application/javascript"
         (lambda (req) (declare (ignore req)) visitor-log:*admin-script*))
    (pub "/stratum/admin.js" "application/javascript"
         (lambda (req) (declare (ignore req))
           (uiop:read-file-string (merge-pathnames "web/admin.js" *stratum-dir*))))
    (pub "/stratum/api/admin" "application/json" #'handle-admin-summary)
    (pub "/stratum/api/admin/visitor" "application/json" #'handle-admin-visitor)
    (pub "/stratum/api/admin/picture" "image/png" #'handle-admin-picture)
    (pub "/stratum/api/admin/upload" "image/png" #'handle-admin-upload)
    (pub "/stratum/api/admin/delete" "application/json" #'handle-admin-delete)))

(pushnew 'publish-admin! *stratum-publishers*)
