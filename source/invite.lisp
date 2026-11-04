;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; The private beta's door: Stratum's gate, an invite-gate (Gendl's
;; gwl/source/invite-gate.lisp) under /stratum.  An invite is a token
;; minted for one person; their link is <public-base>/stratum/invite/
;; <token>, which sets a cookie and sends the browser to /stratum/.
;; Every door under /stratum/ admits a request carrying a live token
;; and refuses the rest.  With no invites file there is no gate at all:
;; development rooms and the test suites run as they always did.
;;
;; dev.lisp and ci/run-tests.lisp load invite-gate before Stratum when
;; the image does not carry it yet.
;;
;; The operator's forms are exported: (stratum:configure-gate! ...),
;; (stratum:mint-invite! "Name"), (stratum:revoke-invite! token),
;; (stratum:invites-report), and (stratum:gate) for invite-gate's own
;; functions, so one shape serves every application on a server.
;;

(defvar *gate* (invite-gate:make-gate :name "Stratum" :prefix "/stratum" :cookie "stratum_invite")
  "Stratum's invite gate; its invites file is the beta's roster.")

(defun configure-gate! (&key (invites-file nil file?) (public-base nil base?))
  "Set the roster file (nil opens the gate) and where invite links
point; the gate as it stands."
  (when file? (setf (invite-gate:gate-invites-file *gate*) invites-file))
  (when base? (setf (invite-gate:gate-public-base *gate*) public-base))
  *gate*)

(defun gate ()
  "Stratum's gate, for invite-gate's own functions: (invite-gate:mint-invite! (stratum:gate) name)."
  *gate*)

(defun invite-gate? () (invite-gate:gate-on? *gate*))
(defun find-invite (token) (invite-gate:find-invite *gate* token))
(defun mint-invite! (name) (invite-gate:mint-invite! *gate* name))
(defun revoke-invite! (token) (invite-gate:revoke-invite! *gate* token))
(defun invites-report () (invite-gate:invites-report *gate*))
(defun invite-allowed? (req) (invite-gate:invite-allowed? *gate* req))
(defun ensure-invite-cookie! (req) (invite-gate:ensure-invite-cookie! *gate* req))
(defun close-after-reply! (req)
  "Called inside with-http-response: the connection closes after this
reply, so the request's body is not read first.  A kept-alive reply
reads the whole body as a string of four bytes a character, which a
large POST turns into a heap exhausted outside any handler, its worker
held in the debugger for good.  Here as well as in invite-gate: the
images carry a compiled invite-gate until they are rebuilt."
  (setf (net.aserve::request-reply-strategy req)
        (remove :keep-alive (net.aserve::request-reply-strategy req))))

(defparameter *refused-body-bytes* 1000000
  "How much body a visitor with no invite may have read and dropped, so
an ordinary small POST gets the page.  A larger one is closed on.")

(defun request-content-length (req)
  "The length REQ declares, or nil."
  (let ((declared (net.aserve:header-slot-value req :content-length)))
    (and declared (ignore-errors (parse-integer declared :junk-allowed t)))))

(defun drain-body! (req most)
  "Read and drop REQ's body when nothing has read it and it declares at
most MOST bytes.  Closing a connection with bytes unread resets it, and
the reset can reach the client ahead of the reply, so a refusal would
arrive as a bare network error."
  (let ((declared (request-content-length req)))
    (when (and declared (<= 0 declared most)
               (null (net.aserve::request-request-body req)))
      (handler-case
          (let ((scratch (make-array 65536 :element-type '(unsigned-byte 8)))
                (sock (net.aserve:request-socket req))
                (left declared))
            (setf (net.aserve::request-request-body req) "")
            (when (net.aserve::request-has-continue-expectation req)
              (net.aserve::send-100-continue req))
            (loop while (plusp left)
                  do (let ((got (glisp:with-deadline (10)
                                  (read-sequence scratch sock :end (min left (length scratch))))))
                       (when (zerop got) (return))
                       (decf left got))))
        (serious-condition () nil)))))

(defun refuse (req ent)
  "A 403 carrying the gate's private-beta page; a large body unread."
  (drain-body! req *refused-body-bytes*)
  (net.aserve:with-http-response (req ent :content-type "text/html"
                                          :response net.aserve:*response-forbidden*)
    (close-after-reply! req)
    (net.aserve:with-http-body (req ent)
      (write-string (invite-gate:refusal-page *gate*) (net.aserve:request-reply-stream req)))))
