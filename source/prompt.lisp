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
;; One of the operations is (look x y w h): a picture of part of the
;; document at full resolution, which goes back to the model, and the
;; model answers again.  So it looks closely before it outlines
;; anything and checks what it selected or changed, a few times in one
;; prompt (*prompt-looks*).
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

(defun agent-looks? ()
  "Whether this image's sexp-agent sends a picture back to the model
and lets it answer again: Gendl's of 2026-11-05 and after."
  (let ((symbol (find-symbol "PICTURES-SHOWN" :sexp-agent))) ; since the first of :looks
    (and symbol (fboundp symbol) t)))

(defun prompt-operators ()
  "The operations the agent admits: *prompt-operators*, without look in
an image whose sexp-agent cannot show the model what look made.  There
the model's (look ...) is refused as no operation of Stratum, and it
goes on from the picture it was sent, as it did before there was one."
  (if (agent-looks?)
      stratum-api:*prompt-operators*
      (remove 'stratum-api:look stratum-api:*prompt-operators*)))

(defvar *agent*
  (sexp-agent:make-agent :name "Stratum"
                         :package :stratum-api
                         :operators (prompt-operators)
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

(defun run-form (form)
  "One form through the sandbox: its result, whatever became of it."
  (handler-case (first (sexp-agent:run-forms *agent* (list form)))
    ;; an image whose compiled sexp-agent catches errors alone
    (serious-condition (e)
      (list :form form :ok nil
            :error (if (typep e 'storage-condition)
                       "There is not enough memory for this operation."
                       (princ-to-string e))))))

(defun run-forms (forms &optional (document stratum-api:*document*))
  "FORMS, a string or a list, run through the agent's sandbox on
DOCUMENT (the current one), each under the lock but for the slow ones
that take it themselves (*long-operators*): what sexp-agent:run-forms
answers.  The same door the prompt box's forms go through."
  (let ((forms (if (stringp forms)
                   (handler-case (sexp-agent:read-forms *agent* forms)
                     (error (e)
                       (return-from run-forms
                         (values (list (list :form forms :ok nil
                                             :error (format nil "The forms could not be read: ~a" e)))
                                 nil))))
                   forms))
        (results '()))
    (let ((stratum-api:*document* document))
      ;; one at a time: heap exhausted is no ERROR, so a sandbox that
      ;; catches errors alone would lose every result with it, and leave
      ;; the page not knowing what the forms before it had done
      (dolist (form forms)
        (let ((result (if (and (consp form) (member (car form) *long-operators*))
                          (run-form form)
                          (bt:with-lock-held (*lock*)
                            (collect-if-full!)
                            (run-form form)))))
          (push result results)
          (unless (getf result :ok)
            ;; what the failed form left behind goes before the next request
            (full-gc)
            (return)))))
    (let ((results (nreverse results)))
      (values results (every (lambda (r) (getf r :ok)) results)))))

;;; What the model sees.  A picture is a rectangle of the document,
;;; flattened as the page shows it, at a scale that is a whole number
;;; either way -- reduced K to 1 when the rectangle is larger than the
;;; picture may be, enlarged 1 to M when it is very small -- so that a
;;; point in the picture is a document pixel by one multiplication,
;;; which the caption states.  The request carries one of the whole
;;; document; (look x y w h) in the model's forms brings one of a part
;;; of it, at full resolution when it fits, and with it the model's
;;; next turn (sexp-agent's :looks): it looks closer before it outlines
;;; anything, and checks the selection or the result after.

(defparameter *prompt-picture-size* 1280
  "The longer side, in pixels, a picture shown to the model may have.
Its images are read at up to 2576 pixels a side, about a token for
each 28 x 28 pixels: 1280 x 853 is some 1400 tokens.")

(defparameter *prompt-picture-least* 400
  "A rectangle smaller than this on its longer side is shown enlarged,
by a whole factor up to eight.")

(defparameter *prompt-looks* 5
  "How many times the model may look, and answer again, in one prompt:
at a thing closely, at a selection of it, at a second thing and its
selection, and once more at the result.")

(defparameter *reading-operators*
  '(stratum-api:documents stratum-api:describe-document stratum-api:document-info
    stratum-api:layers stratum-api:layer-info stratum-api:selection stratum-api:look
    stratum-api:drawing-sources stratum-api:undo stratum-api:redo)
  "The operations that leave nothing for a prompt's own undo to take
back.  Undo and redo are among them: 'undo that' is not itself a change
to undo, and the lists stand as those forms left them.")

(defun changing-result? (result)
  "Whether RESULT, of run-forms, is of a form that may have changed the
document, passed or not."
  (let ((form (getf result :form)))
    (and (consp form) (not (member (car form) *reading-operators*)))))

(defstruct (picture (:constructor make-picture (png note width height &optional small)))
  "What (look) answers: PNG octets WIDTH x HEIGHT and NOTE, the words
that place the picture in the document; SMALL, the same picture for the
prompt log when there is a log and PNG is larger than the log's size."
  png note width height small)

(defun picture-log-png (picture)
  "PICTURE's PNG octets for the prompt log."
  (or (picture-small picture) (picture-png picture)))

(defmethod print-object ((picture picture) stream)
  (print-unreadable-object (picture stream)
    (format stream "picture ~a x ~a: ~a" (picture-width picture) (picture-height picture)
            (picture-note picture))))

(defvar *prompt-log-directory* nil
  "Where each prompt's record goes (configure-prompt-log!): a directory
of its own per prompt, with small pictures of what the model saw.  Nil
keeps none, the default: the pictures are the visitor's, so keeping them
is the operator's decision and something the visitors should be told.")

(defparameter *prompt-log-turns* 300
  "How many prompts the log keeps; the oldest go first.")

(defparameter *prompt-log-picture-size* 640
  "The longer side, in pixels, of a picture in the log.")

(defun scale-rect (pixels width channels x y w h k m)
  "The W x H rectangle at X, Y of PIXELS (WIDTH wide, CHANNELS bytes a
pixel), reduced by the whole factor K (each pixel the mean of K x K,
fewer at the far edges) or enlarged by M (each pixel M x M): (values
octets picture-width picture-height)."
  (declare (type (simple-array (unsigned-byte 8) (*)) pixels)
           (type fixnum width channels x y w h k m))
  (let* ((pw (if (> k 1) (ceiling w k) (* w m)))
         (ph (if (> k 1) (ceiling h k) (* h m)))
         (out (make-array (* pw ph channels) :element-type '(unsigned-byte 8))))
    (declare (type fixnum pw ph))
    (dotimes (py ph)
      (dotimes (px pw)
        (let* ((x0 (+ x (if (> k 1) (* px k) (floor px m))))
               (y0 (+ y (if (> k 1) (* py k) (floor py m))))
               (x1 (min (+ x w) (+ x0 k)))
               (y1 (min (+ y h) (+ y0 k)))
               (count (* (- x1 x0) (- y1 y0)))
               (o (* channels (+ px (* py pw)))))
          (declare (type fixnum x0 y0 x1 y1 count o))
          (dotimes (c channels)
            (let ((sum 0))
              (declare (type fixnum sum))
              (loop for sy of-type fixnum from y0 below y1
                    do (loop for sx of-type fixnum from x0 below x1
                             do (incf sum (aref pixels (+ c (* channels (+ sx (* sy width))))))))
              (setf (aref out (+ o c)) (round sum count)))))))
    (values out pw ph)))

(defun show-selection! (rgba mask pw ph)
  "RGBA (PW x PH) dimmed where MASK, the selection at the same size,
does not cover, and the selection's edge drawn in magenta: how a
picture shows the model what is selected."
  (declare (type (simple-array (unsigned-byte 8) (*)) rgba mask) (type fixnum pw ph))
  (flet ((in? (px py) (and (<= 0 px) (< px pw) (<= 0 py) (< py ph)
                           (>= (aref mask (+ px (* py pw))) 128))))
    (dotimes (py ph rgba)
      (dotimes (px pw)
        (let ((o (* 4 (+ px (* py pw)))))
          (if (and (in? px py)
                   ;; two pixels of edge: one is hard to see
                   (not (and (in? (1- px) py) (in? (1+ px) py) (in? px (1- py)) (in? px (1+ py))
                             (in? (- px 2) py) (in? (+ px 2) py) (in? px (- py 2)) (in? px (+ py 2))))
                   ;; the picture's own border is no edge of the selection
                   (< 1 px (- pw 2)) (< 1 py (- ph 2)))
              (setf (aref rgba o) 255 (aref rgba (+ o 1)) 0 (aref rgba (+ o 2)) 255 (aref rgba (+ o 3)) 255)
              (let ((keep (+ 140 (floor (* 115 (aref mask (+ px (* py pw)))) 255))))
                (dotimes (c 3)
                  (setf (aref rgba (+ o c)) (floor (* keep (aref rgba (+ o c))) 255))))))))))

(defun small-png (rgba pw ph)
  "RGBA, a picture PW x PH, as PNG octets at the log's size, when there
is a log and it is larger than that."
  (when (and *prompt-log-directory* (> (max pw ph) *prompt-log-picture-size*))
    (multiple-value-bind (small sw sh)
        (scale-rect rgba pw 4 0 0 pw ph (ceiling (max pw ph) *prompt-log-picture-size*) 1)
      (png-octets small sw sh))))

(defun document-picture (doc &key x y w h (size *prompt-picture-size*) (selection t))
  "A picture of the W x H rectangle of DOC at X, Y (all of it when
unsaid; clipped to the document), its longer side at most SIZE.  With
SELECTION and something selected, the selection shows."
  (let* ((dw (the-object doc doc-width))
         (dh (the-object doc doc-height))
         (x0 (max 0 (min dw (round (or x 0)))))
         (y0 (max 0 (min dh (round (or y 0)))))
         (x1 (max 0 (min dw (round (+ (or x 0) (or w dw))))))
         (y1 (max 0 (min dh (round (+ (or y 0) (or h dh))))))
         (rw (- x1 x0))
         (rh (- y1 y0)))
    (unless (and (plusp rw) (plusp rh))
      (error "The rectangle x ~a y ~a w ~a h ~a is not inside the document, which is ~a x ~a." x y w h dw dh))
    ;; the render is a layer's worth of bytes: make room before, not
    ;; after it has failed
    (ensure-room (* 4 dw dh))
    (let* ((k (max 1 (ceiling (max rw rh) size)))
           (m (if (= k 1) (max 1 (min 8 (floor *prompt-picture-least* (max rw rh)))) 1))
           (mask (and selection (the-object doc selection) (getf (the-object doc selection) :mask))))
      (multiple-value-bind (rgba pw ph) (scale-rect (the-object doc (render-cpu)) dw 4 x0 y0 rw rh k m)
        (when mask
          (show-selection! rgba (scale-rect mask dw 1 x0 y0 rw rh k m) pw ph))
        (make-picture
         (png-octets rgba pw ph)
         (format nil "the document's rectangle x ~d y ~d w ~d h ~d, shown ~d x ~d pixels~a.  A point at picture pixel (px, py) is document pixel (~a, ~a).~a"
                 x0 y0 rw rh pw ph
                 (cond ((> k 1) (format nil ", reduced ~d to 1" k))
                       ((> m 1) (format nil ", enlarged 1 to ~d" m))
                       (t ", one picture pixel a document pixel"))
                 (picture-axis x0 "px" k m) (picture-axis y0 "py" k m)
                 (cond ((not selection) "")
                       (mask "  The selection is shown: the picture is dimmed outside it and its edge is drawn in magenta.")
                       (t "  Nothing is selected.")))
         pw ph
         (small-png rgba pw ph))))))

(defun picture-axis (origin name k m)
  "One coordinate of the caption's mapping: '3300 + px', '5 px',
'3300 + px / 4'."
  (let ((term (cond ((> k 1) (format nil "~d ~a" k name))
                    ((> m 1) (format nil "~a / ~d" name m))
                    (t name))))
    (if (zerop origin) term (format nil "~d + ~a" origin term))))

(defun document-picture-png (doc &optional (size *prompt-picture-size*))
  "The document flattened and reduced so its longer side is at most
SIZE, as PNG octets."
  (picture-png (document-picture doc :size size :selection nil)))

(defun stratum-api:look (&rest where)
  "(look) or (look x y w h), either with :selection nil after: a
picture of the W x H rectangle of the document at X, Y, or of all of
it, flattened as the page shows it, at full resolution when the
rectangle fits a picture and reduced by a whole factor when it does
not, with the selection showing unless :selection is nil.  To the
prompt box's model the picture comes back with its next turn."
  (let ((rect (loop while (realp (first where)) collect (pop where))))
    (unless (and (member (length rect) '(0 4))
                 (or (null where) (and (eq (first where) :selection) (= (length where) 2))))
      (error "(look) shows the whole document and (look x y w h) a rectangle of it, four numbers; :selection nil after either leaves the selection out."))
    (destructuring-bind (&optional x y w h) rect
      (document-picture (stratum-api::doc) :x x :y y :w w :h h
                                           :selection (getf where :selection t)))))

(defparameter *prompt-history-turns* 6
  "How many earlier turns on a document go with each prompt.")

(defvar *prompt-progress* (lambda (note) (declare (ignore note)))
  "Called with a few words after each round of the model's forms; the
prompt door binds it to put them on the job, for the page.")

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

(defun configure-prompt-log! (&key (directory nil directory?) (turns nil turns?))
  "Keep a record of each prompt under DIRECTORY, made if need be and
closed to everyone but its owner, or none with nil; at most TURNS of
them.  The directory."
  (when turns? (setf *prompt-log-turns* turns))
  (when directory?
    (let ((path (and directory (uiop:ensure-directory-pathname directory))))
      (when path
        (ensure-directories-exist path)
        #+unix (uiop:run-program (list "chmod" "700" (uiop:native-namestring path))
                                 :ignore-error-status t))
      (setf *prompt-log-directory* path)))
  *prompt-log-directory*)

(defvar *turn-count* 0
  "Prompts logged since the server started: two in one second keep
their order.")

(defun turn-directory ()
  "A new directory for one prompt, named so that the names sort by time."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (merge-pathnames (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~6,'0d-~36r/"
                             year month day hour minute second (incf *turn-count*)
                             (random (expt 36 4) (make-random-state t)))
                     *prompt-log-directory*)))

(defun write-octets-file (path octets)
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence octets out)))

(defun prune-prompt-log! ()
  "The oldest prompts gone, past *prompt-log-turns*."
  (let ((turns (sort (mapcar #'uiop:native-namestring (uiop:subdirectories *prompt-log-directory*))
                     #'string>)))
    (dolist (old (nthcdr *prompt-log-turns* turns))
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname old) :validate t))))

(defun after-picture (doc)
  "The document at the size of the log's pictures, or nil with no log."
  (when *prompt-log-directory*
    (collect-if-full!)
    (document-picture doc :size *prompt-log-picture-size*)))

(defun log-turn! (turn before looks after)
  "TURN, a plist, and its pictures -- BEFORE, what the model was shown,
LOOKS, what it asked to see, in order, AFTER, how it ended -- into a
directory of their own: turn.sexp, before.png, look-1.png ..., after.png.
No token goes in: a person is the name on their invite.  A log that
cannot be written must not cost anyone their prompt."
  (when *prompt-log-directory*
    (handler-case
        (let ((dir (turn-directory)))
          (ensure-directories-exist dir)
          (with-open-file (out (merge-pathnames "turn.sexp" dir) :direction :output
                                                                 :external-format :utf-8 :if-exists :supersede)
            (let ((*print-pretty* t) (*print-right-margin* 100) (*print-readably* nil)
                  (*package* (find-package :common-lisp)) (*print-case* :downcase))
              (prin1 (append turn (list :looks (mapcar #'picture-note looks))) out)
              (terpri out)))
          (write-octets-file (merge-pathnames "before.png" dir) (picture-log-png before))
          (loop for look in looks
                for n from 1
                do (write-octets-file (merge-pathnames (format nil "look-~d.png" n) dir)
                                      (picture-log-png look)))
          (when after (write-octets-file (merge-pathnames "after.png" dir) (picture-log-png after)))
          (prune-prompt-log!))
      (serious-condition (e)
        (let ((*print-readably* nil))
          (format *error-output* "~&[stratum] prompt log: ~a~%" e))))))

(defun generation-forms (settings prompt)
  "The forms for the Generate dialog's SETTINGS, a table from the page:
PROMPT painted with no model between it and the generator."
  (let* ((source (gethash "source" settings))
         (name (let ((n (gethash "name" settings))) (if (and (stringp n) (plusp (length n))) n "Generated")))
         (seed (let ((s (gethash "seed" settings))) (if (integerp s) s -1)))
         (reference? (gethash "reference" settings))
         (strength (let ((s (gethash "strength" settings))) (if (realp s) (max 0.05d0 (min 1d0 (float s 1d0))) 0.35d0)))
         (changed? (not (eq (gethash "keepAll" settings) t))))
    (cond ((equal source "new")
           (format nil "(generate-layer ~s :name ~s :seed ~d)" prompt name seed))
          ((equal source "picture")
           (format nil "(generate-picture ~s :name ~s :seed ~d :strength ~,2f :reference ~:[nil~;t~])"
                   prompt name seed strength (eq reference? t)))
          ((equal source "selection")
           (format nil "(generate-fill ~s :name ~s :seed ~d :keep ~:[:all~;:changed~])"
                   prompt name seed changed?))
          (t (error "Generate from ~s is not something this server knows." source)))))

(defun prompt-document (doc prompt &key session who forms)
  "PROMPT done to DOC: the document described, pictured and sent with
the prompt to the model; its forms run under the lock on DOC; the
turn kept on the document, and what its forms did one undo, however
it ended (brush.lisp), unless they remade the document, which like
Image > Crop has no undo.  With FORMS, a string, those run and no model
is asked: everything else is the same.  WHO is the invitee, for the
prompt log.  A
plist for the page: :reply :forms :results :ok :declined :stopped :changed :usage :width
:height."
  (let ((name (the-object doc doc-name))
        (began (get-internal-real-time))
        (ran 0)
        (changed nil)
        (looks '())
        journal state picture history answer outcome failure)
    (bt:with-lock-held (*lock*)
      (let ((stratum-api:*document* doc))
        (setf state (stratum-api:describe-document)))
      (setf picture (document-picture doc)
            history (reverse (the-object doc prompt-log))
            journal (begin-prompt-journal! doc)))
    (unwind-protect
         (handler-bind ((serious-condition (lambda (e) (setf failure (princ-to-string e)))))
           (let ((runner
                   ;; a form that remade the document (crop, a resize)
                   ;; registered the new one under the name: each round
                   ;; runs on the one there now
                   (lambda (code)
                     (multiple-value-bind (results passed)
                         (run-forms code (or (gethash name *documents*) doc))
                       (incf ran (length results))
                       (when (some #'changing-result? results) (setf changed t))
                       (funcall *prompt-progress* (format nil "~d form~:p run" ran))
                       (values results passed)))))
             (setf answer
                   (if forms
                       (multiple-value-bind (results passed) (funcall runner forms)
                         (let ((results (mapcar (lambda (r)
                                                  (list* :form (sexp-agent:form-string *agent* (getf r :form)) r))
                                                results)))
                           (list :reply (if passed "Generated with no planner." "")
                                 :forms (mapcar (lambda (r) (getf r :form)) results)
                                 :results results
                                 :ok passed
                                 :rounds 0
                                 :assistant forms)))
                       (sexp-agent:ask *agent* prompt
                                       :state-text (format nil "~a~%The picture with this message is ~a~%This server ~:[has no image generator: generate-fill, generate-layer and generate-picture will fail~;has an image generator~]."
                                                           state (picture-note picture) *generator*)
                                       :image-png (picture-png picture)
                                       :session-id (and session (format nil "stratum-~a" session))
                                       :history history
                                       :runner runner
                                       :looks *prompt-looks*
                                       :picture-of (lambda (value)
                                                     (when (picture-p value)
                                                       (push value looks)
                                                       (values (picture-png value) (picture-note value))))
                                       ;; a Gendl whose sexp-agent has no looks yet
                                       ;; runs the forms and stops there
                                       :allow-other-keys t))))
           (bt:with-lock-held (*lock*)
             (let ((doc (or (gethash name *documents*) doc)))
               (the-object doc (set-slot! :prompt-log
                                          (subseq (cons (cons prompt (getf answer :assistant))
                                                        (the-object doc prompt-log))
                                                  0 (min (1+ (length (the-object doc prompt-log)))
                                                         *prompt-history-turns*))))
               (setf outcome
                     (list :reply (getf answer :reply)
                           :forms (getf answer :forms)
                           :results (mapcar (lambda (r)
                                              (list :form (getf r :form) :ok (getf r :ok)
                                                    :value (and (getf r :ok) (printed (getf r :value)))
                                                    :error (getf r :error)))
                                            (getf answer :results))
                           :ok (getf answer :ok)
                           :declined (getf answer :declined)
                           :stopped (and (getf answer :stopped) t)
                           :changed (prompt-changes doc journal)
                           :usage (getf answer :usage)
                           :width (the-object doc doc-width)
                           :height (the-object doc doc-height)))))
           outcome)
      (let ((after nil))
        (bt:with-lock-held (*lock*)
          (end-prompt-journal! doc journal
                               (and changed (eq doc (or (gethash name *documents*) doc))))
          (setf after (after-picture (or (gethash name *documents*) doc))))
        (log-turn! (list :who who :session session :document name
                         :prompt prompt :state state
                         :model (if forms "no planner" (sexp-agent:agent-model *agent*))
                         :effort (unless forms (sexp-agent:agent-effort *agent*))
                         :seconds (round (elapsed-ms began) 1000)
                         :rounds (getf answer :rounds)
                         :failure (or failure (getf answer :stopped))
                         :outcome outcome)
                   picture (reverse looks) after)))))

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
                "declined" (json-bool (getf result :declined))
                "stopped" (json-bool (getf result :stopped))
                "changed" (coerce (mapcar (lambda (c)
                                            (json-object "from" (getf c :from) "stamp" (getf c :stamp)
                                                         "x" (getf c :x) "y" (getf c :y)
                                                         "w" (getf c :w) "h" (getf c :h)))
                                          (getf result :changed))
                                  'vector)
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

(defun start-prompt-job! (documents doc prompt session &optional who forms)
  "The prompt started on its own thread, FORMS in place of a model's
when given; the job's id."
  (let ((id (format nil "~36r~36r" (get-universal-time) (random (expt 36 6) (make-random-state t)))))
    (bt:with-lock-held (*prompt-jobs-lock*)
      (sweep-prompt-jobs!)
      (setf (gethash id *prompt-jobs*) (list :status :working :started (get-universal-time))))
    (bt:make-thread
     (lambda ()
       (let ((outcome (handler-case
                          (let ((*documents* documents)
                                (*prompt-progress*
                                  (lambda (note)
                                    (bt:with-lock-held (*prompt-jobs-lock*)
                                      (let ((job (gethash id *prompt-jobs*)))
                                        (when (eql (getf job :status) :working)
                                          (setf (gethash id *prompt-jobs*)
                                                (list :status :working :started (getf job :started)
                                                      :note note))))))))
                            (list :status :done :result (prompt-json (prompt-document doc prompt :session session :who who :forms forms))))
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
      (:working (json-string (json-object "status" "working" "note" (getf job :note))))
      (:done (let ((result (yason:parse (getf job :result))))
               (setf (gethash "status" result) "done")
               (json-string result)))
      (:failed (json-string (json-object "status" "failed" "error" (getf job :error))))
      (t (json-string (json-object "status" "unknown"))))))

(defun invitee (req)
  "The name on REQ's invite, for the prompt log; nil with the gate open."
  (second (find-invite (invite-gate:request-token *gate* req))))

(defun handle-prompt (req ent)
  "POST /stratum/api/prompt?doc=&s= {prompt}: the prompt started on the
session's document, answering {job}; with {generate: {...}} as well
(generation-forms) it goes to the image generator with no model asked;
GET ...?job=ID: the job's state,
and its result once done (prompt-job-json).  A 500 carries what went
wrong, as the other doors answer."
  (let ((result nil) (failure nil))
    (handler-case
        (let ((job (net.aserve:request-query-value "job" req)))
          (if job
              (setf result (prompt-job-json job))
              (let* ((body (yason:parse (gwl:request-body-text req)))
                     (prompt (and (hash-table-p body) (gethash "prompt" body)))
                     (settings (and (hash-table-p body) (gethash "generate" body)))
                     (session (net.aserve:request-query-value "s" req))
                     (documents nil)
                     (doc nil))
                (unless (and (stringp prompt) (plusp (length (string-trim '(#\Space #\Newline #\Tab) prompt))))
                  (error "The prompt is empty."))
                (bt:with-lock-held (*lock*)
                  (setf documents (request-session-table req))
                  (let ((*documents* documents)) (setf doc (request-doc req))))
                (setf result (json-string (json-object "job" (start-prompt-job! documents doc prompt session
                                                                                   (invitee req)
                                                                                   (and (hash-table-p settings)
                                                                                        (generation-forms settings prompt)))))))))
      (serious-condition (e) (setf failure (princ-to-string e))))
    (if failure
        (reply-text req ent net.aserve:*response-internal-server-error* failure)
        (net.aserve:with-http-response (req ent :content-type "application/json")
          (no-store! req)
          (net.aserve:with-http-body (req ent)
            (write-string result (net.aserve:request-reply-stream req)))))))

(defun publish-prompt! (server host)
  "The prompt door and the reference, behind the invite gate like
every door under /stratum."
  (net.aserve:publish :path "/stratum/api/prompt" :server server :host host
                      :content-type "application/json"
                      :function (lambda (req ent)
                                  (when (door-admits? req ent *max-request-bytes*)
                                    (handle-prompt req ent))))
  (publish-door server host "/stratum/api/reference" "text/markdown; charset=utf-8"
                (lambda (req) (declare (ignore req)) (api-reference))))

(pushnew 'publish-prompt! *stratum-publishers*)
