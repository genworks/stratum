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
(defun refuse (req ent) (invite-gate:refuse *gate* req ent))
