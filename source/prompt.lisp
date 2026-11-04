;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; The prompt box: an edit described in words, done through the Lisp
;; API (api.lisp) by a language model.  POST /stratum/api/prompt
;; {prompt} sends the model docs/api.md, the document described in
;; words with a small picture of it, and the request; the model
;; answers with forms; sexp-agent (Gendl's gwl/source/sexp-agent.lisp)
;; runs them through its sandbox -- only *prompt-operators*, only
;; literal arguments -- on the page's document, and the page redraws.
;; The model is reached at *agent*'s messages-url: a key-holding gate
;; in production (configure-agent! from the room's services-init), or
;; the API with a key file on a trusted development ship.
;;
;; The lock is held while the state is read and while the forms run,
;; never during the model's call, which takes seconds.
;;

(defparameter *api-reference-file* (merge-pathnames "docs/api.md" *stratum-dir*)
  "Where the API reference, the model's instructions, is read from.")

(defun api-reference ()
  "The text of docs/api.md."
  (if (probe-file *api-reference-file*)
      (uiop:read-file-string *api-reference-file*)
      (format nil "The reference document ~a is missing." *api-reference-file*)))

(defvar *agent*
  (sexp-agent:make-agent :name "Stratum"
                         :package :stratum-api
                         :operators stratum-api:*prompt-operators*
                         :reference #'api-reference
                         :max-tokens 8000)
  "The prompt box's agent.")

(defun configure-agent! (&key (messages-url nil url?) (api-key-file nil key?)
                              (model nil model?) (effort nil effort?)
                              (session-header nil header?))
  "Where the agent reaches its model and how: MESSAGES-URL (a gate, or
the API), API-KEY-FILE (nil behind a gate), MODEL, EFFORT (low, medium,
high) and the SESSION-HEADER a gate reads.  The agent as it stands."
  (when url? (setf (sexp-agent:agent-messages-url *agent*) messages-url))
  (when key? (setf (sexp-agent:agent-api-key-file *agent*) api-key-file))
  (when model? (setf (sexp-agent:agent-model *agent*) model))
  (when effort? (setf (sexp-agent:agent-effort *agent*) effort))
  (when header? (setf (sexp-agent:agent-session-header *agent*) session-header))
  *agent*)

(defun run-forms (forms &optional (document stratum-api:*document*))
  "FORMS, a string or a list, run through the agent's sandbox on
DOCUMENT (the current one) under the lock: what sexp-agent:run-forms
answers.  The same door the prompt box's forms go through."
  (bt:with-lock-held (*lock*)
    (let ((stratum-api:*document* document))
      (sexp-agent:run-forms *agent* forms))))

(defparameter *prompt-picture-size* 640
  "The longer side, in pixels, of the picture of the document the
model is shown with each prompt.")

(defun document-picture-png (doc &optional (size *prompt-picture-size*))
  "The document flattened and scaled so its longer side is SIZE, as PNG
octets."
  (let* ((w (the-object doc doc-width))
         (h (the-object doc doc-height))
         (scale (min 1 (/ size (max w h))))
         (tw (max 1 (round (* w scale))))
         (th (max 1 (round (* h scale)))))
    (png-octets (rgba-thumb (the-object doc (render-cpu)) w h tw th 4) tw th)))

(defparameter *prompt-history-turns* 6
  "How many earlier turns on a document go with each prompt.")

(defun printed (value)
  "VALUE as the page shows it, in the API package, at most 400 characters."
  (let ((text (let ((*package* (find-package :stratum-api))
                    (*print-case* :downcase)
                    (*print-pretty* nil)
                    (*print-readably* nil))
                (prin1-to-string value))))
    (if (> (length text) 400)
        (format nil "~a ..." (subseq text 0 400))
        text)))

(defun prompt-document (doc prompt &key session)
  "PROMPT done to DOC: the document described, pictured and sent with
the prompt to the model; its forms run under the lock on DOC; the
turn kept on the document.  A plist for the page: :reply :forms
:results :ok :usage :width :height."
  (let (state picture history)
    (bt:with-lock-held (*lock*)
      (let ((stratum-api:*document* doc))
        (setf state (stratum-api:describe-document)))
      (setf picture (document-picture-png doc)
            history (reverse (the-object doc prompt-log))))
    (let ((answer (sexp-agent:ask *agent* prompt
                                  :state-text state
                                  :image-png picture
                                  :session-id (and session (format nil "stratum-~a" session))
                                  :history history
                                  :runner (lambda (code) (run-forms code doc)))))
      (bt:with-lock-held (*lock*)
        ;; a form that remade the document (crop, a resize) registered
        ;; the new one under the name; the turn goes on that one
        (let ((doc (or (gethash (the-object doc doc-name) *documents*) doc)))
          (the-object doc (set-slot! :prompt-log
                                     (subseq (cons (cons prompt (getf answer :assistant))
                                                   (the-object doc prompt-log))
                                             0 (min (1+ (length (the-object doc prompt-log)))
                                                    *prompt-history-turns*))))
          (list :reply (getf answer :reply)
                :forms (getf answer :forms)
                :results (mapcar (lambda (r)
                                   (list :form (getf r :form) :ok (getf r :ok)
                                         :value (and (getf r :ok) (printed (getf r :value)))
                                         :error (getf r :error)))
                                 (getf answer :results))
                :ok (getf answer :ok)
                :usage (getf answer :usage)
                :width (the-object doc doc-width)
                :height (the-object doc doc-height)))))))

(defun prompt-json (result)
  (json-string
   (json-object "reply" (getf result :reply)
                "forms" (coerce (getf result :forms) 'vector)
                "results" (coerce (mapcar (lambda (r)
                                            (json-object "form" (getf r :form)
                                                         "ok" (json-bool (getf r :ok))
                                                         "value" (getf r :value)
                                                         "error" (getf r :error)))
                                          (getf result :results))
                                  'vector)
                "ok" (json-bool (getf result :ok))
                "usage" (let ((usage (getf result :usage)))
                          (json-object "input" (getf usage :input 0) "output" (getf usage :output 0)))
                "width" (getf result :width)
                "height" (getf result :height))))

;;; The door.  A model's call takes seconds, sometimes a minute, and a
;;; proxy in front of the server (Cloudflare) cuts a request at about a
;;; hundred; so the door starts the work on a thread of its own and
;;; answers a job id at once, and the page asks after the job until it
;;; is done.

(defvar *prompt-jobs* (make-hash-table :test 'equal)
  "Job id -> (:status :working | :done | :failed, :result JSON text or
:error text, :started universal time).")

(defvar *prompt-jobs-lock* (bt:make-lock "stratum prompt jobs"))

(defparameter *prompt-job-seconds* 900
  "A finished job is forgotten this long after it started.")

(defun sweep-prompt-jobs! ()
  (let ((now (get-universal-time)) (gone '()))
    (maphash (lambda (id job)
               (when (and (not (eql (getf job :status) :working))
                          (> (- now (getf job :started)) *prompt-job-seconds*))
                 (push id gone)))
             *prompt-jobs*)
    (dolist (id gone) (remhash id *prompt-jobs*))))

(defun start-prompt-job! (documents doc prompt session)
  "The prompt started on its own thread; the job's id."
  (let ((id (format nil "~36r~36r" (get-universal-time) (random (expt 36 6) (make-random-state t)))))
    (bt:with-lock-held (*prompt-jobs-lock*)
      (sweep-prompt-jobs!)
      (setf (gethash id *prompt-jobs*) (list :status :working :started (get-universal-time))))
    (bt:make-thread
     (lambda ()
       (let ((outcome (handler-case
                          (let ((*documents* documents))
                            (list :status :done :result (prompt-json (prompt-document doc prompt :session session))))
                        (serious-condition (e)
                          (list :status :failed :error (princ-to-string e))))))
         (bt:with-lock-held (*prompt-jobs-lock*)
           (setf (gethash id *prompt-jobs*)
                 (append outcome (list :started (getf (gethash id *prompt-jobs*) :started)))))))
     :name (format nil "stratum prompt ~a" id))
    id))

(defun prompt-job-json (id)
  "What the page hears about job ID: {status: working}, {status: done,
...the result}, {status: failed, error}, or {status: unknown}."
  (let ((job (bt:with-lock-held (*prompt-jobs-lock*) (gethash id *prompt-jobs*))))
    (case (getf job :status)
      (:working (json-string (json-object "status" "working")))
      (:done (let ((result (yason:parse (getf job :result))))
               (setf (gethash "status" result) "done")
               (json-string result)))
      (:failed (json-string (json-object "status" "failed" "error" (getf job :error))))
      (t (json-string (json-object "status" "unknown"))))))

(defun handle-prompt (req ent)
  "POST /stratum/api/prompt?doc=&s= {prompt}: the prompt started on the
session's document, answering {job}; GET ...?job=ID: the job's state,
and its result once done (prompt-job-json).  A 500 carries what went
wrong, as the other doors answer."
  (let ((result nil) (failure nil))
    (handler-case
        (let ((job (net.aserve:request-query-value "job" req)))
          (if job
              (setf result (prompt-job-json job))
              (let* ((body (yason:parse (gwl:request-body-text req)))
                     (prompt (and (hash-table-p body) (gethash "prompt" body)))
                     (session (net.aserve:request-query-value "s" req))
                     (documents nil)
                     (doc nil))
                (unless (and (stringp prompt) (plusp (length (string-trim '(#\Space #\Newline #\Tab) prompt))))
                  (error "The prompt is empty."))
                (bt:with-lock-held (*lock*)
                  (setf documents (request-session-table req))
                  (let ((*documents* documents)) (setf doc (request-doc req))))
                (setf result (json-string (json-object "job" (start-prompt-job! documents doc prompt session)))))))
      (serious-condition (e) (setf failure (princ-to-string e))))
    (if failure
        (net.aserve:with-http-response (req ent :content-type "text/plain"
                                                :response net.aserve:*response-internal-server-error*)
          (net.aserve:with-http-body (req ent)
            (write-string failure (net.aserve:request-reply-stream req))))
        (net.aserve:with-http-response (req ent :content-type "application/json")
          (net.aserve:with-http-body (req ent)
            (write-string result (net.aserve:request-reply-stream req)))))))

(defun publish-prompt! (server host)
  "The prompt door and the reference, behind the invite gate like
every door under /stratum."
  (net.aserve:publish :path "/stratum/api/prompt" :server server :host host
                      :content-type "application/json"
                      :function (lambda (req ent)
                                  (if (invite-allowed? req)
                                      (progn (ensure-invite-cookie! req)
                                             (handle-prompt req ent))
                                      (refuse req ent))))
  (publish-door server host "/stratum/api/reference" "text/markdown; charset=utf-8"
                (lambda (req) (declare (ignore req)) (api-reference))))

(pushnew 'publish-prompt! *stratum-publishers*)
