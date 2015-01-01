;;; jabber-chatstate.el --- Chat state notification (XEP-0085) implementation

;; Author: Ami Fischman <ami@fischman.org>
;; (based entirely on jabber-events.el by Magnus Henoch <mange@freemail.hu>)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; TODO
;; - Currently only active/composing notifications are /sent/ though all 5
;;   notifications are handled on receipt.

(require 'cl)

(defgroup jabber-chatstates nil
  "Chat state notifications."
  :group 'jabber)

(defconst jabber-chatstates-xmlns "http://jabber.org/protocol/chatstates"
  "XML namespace for the chatstates feature.")

(defcustom jabber-chatstates-confirm t
  "Send notifications about chat states?"
  :group 'jabber-chatstates
  :type 'boolean)

(defvar jabber-chatstates-requested 'first-time
  "Whether or not chat states notification was requested.
This is one of the following:
first-time - send state in first stanza, then switch to nil
t - send states
nil - don't send states")
(make-variable-buffer-local 'jabber-chatstates-requested)

(defvar jabber-chatstates-last-state nil
  "The last seen chat state.")
(make-variable-buffer-local 'jabber-chatstates-last-state)

(defvar jabber-chatstates-message ""
  "Human-readable presentation of chat state information")
(make-variable-buffer-local 'jabber-chatstates-message)

;;; INCOMING
;;; Code for requesting chat state notifications from others and handling
;;; them.

(defun jabber-chatstates-update-message ()
  (setq jabber-chatstates-message
        (if (and jabber-chatstates-last-state
                 (not (eq 'active jabber-chatstates-last-state)))
            (format " (%s)" (symbol-name jabber-chatstates-last-state))
          "")))

(add-hook 'jabber-chat-send-hooks 'jabber-chatstates-when-sending)
(defun jabber-chatstates-when-sending (text id)
  (jabber-chatstates-update-message)
  (jabber-chatstates-stop-timer)
  (when (and jabber-chatstates-confirm jabber-chatstates-requested)
    (when (eq jabber-chatstates-requested 'first-time)
      ;; don't send more notifications until we know that the other
      ;; side wants them.
      (setq jabber-chatstates-requested nil))
    (setq jabber-chatstates-composing-sent nil)
    `((active ((xmlns . ,jabber-chatstates-xmlns))))))

;;; OUTGOING
;;; Code for handling requests for chat state notifications and providing
;;; them, modulo user preferences.

(defvar jabber-chatstates-composing-sent nil
  "Has composing notification been sent?
It can be sent and cancelled several times.")
(make-variable-buffer-local 'jabber-chatstates-composing-sent)

(defvar jabber-chatstates-paused-timer nil
  "Timer that counts down from 'composing state to 'paused.")
(make-variable-buffer-local 'jabber-chatstates-paused-timer)

(defun jabber-chatstates-stop-timer ()
  "Stop the 'paused timer."
  (when jabber-chatstates-paused-timer
    (cancel-timer jabber-chatstates-paused-timer)))

(defun jabber-chatstates-kick-timer ()
  "Start (or restart) the 'paused timer as approriate."
  (jabber-chatstates-stop-timer)
  (setq jabber-chatstates-paused-timer
        (run-with-timer 5 nil 'jabber-chatstates-send-paused)))

(defun jabber-chatstates-send-paused ()
  "Send an 'paused state notification."
  (when (and jabber-chatstates-requested jabber-chatting-with)
    (setq jabber-chatstates-composing-sent nil)
    (jabber-send-sexp
     jabber-buffer-connection
     `(message
       ((to . ,jabber-chatting-with)
        (type . "chat"))
       (paused ((xmlns . ,jabber-chatstates-xmlns)))))))

(defun jabber-chatstates-after-change ()
  (let* ((composing-now (not (= (point-max) jabber-point-insert)))
         (state (if composing-now 'composing 'active)))
    (when (and jabber-chatstates-confirm
               jabber-chatting-with
	       jabber-chatstates-requested
               (not (eq composing-now jabber-chatstates-composing-sent)))
      (jabber-send-sexp
       jabber-buffer-connection
       `(message
         ((to . ,jabber-chatting-with)
          (type . "chat"))
         (,state ((xmlns . ,jabber-chatstates-xmlns)))))
      (when (setq jabber-chatstates-composing-sent composing-now)
        (jabber-chatstates-kick-timer)))))

;;; COMMON

(defun jabber-handle-incoming-message-chatstates (jc xml-data)
  (when (get-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from)))
    (with-current-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))
      (cond
       ;; If we get an error message, we shouldn't report any
       ;; events, as the requests are mirrored from us.
       ((string= (jabber-xml-get-attribute xml-data 'type) "error")
        (remove-hook 'post-command-hook 'jabber-chatstates-after-change t)
        (setq jabber-chatstates-requested nil))

       (t
	(let ((state
	       (or
		(let ((node
		       (find jabber-chatstates-xmlns
			     (jabber-xml-node-children xml-data)
			     :key #'(lambda (x) (jabber-xml-get-attribute x 'xmlns))
			     :test #'string=)))
		  (jabber-xml-node-name node))
		(let ((node
		       ;; XXX: this is how we interoperate with
		       ;; Google Talk.  We should really use a
		       ;; namespace-aware XML parser.
		       (find jabber-chatstates-xmlns
			     (jabber-xml-node-children xml-data)
			     :key #'(lambda (x) (jabber-xml-get-attribute x 'xmlns:cha))
			     :test #'string=)))
		  (when node
		    ;; Strip the "cha:" prefix
		    (let ((name (symbol-name (jabber-xml-node-name node))))
		      (when (> (length name) 4)
			(intern (substring name 4)))))))))
	  ;; Set up hooks for composition notification
	  (when (and jabber-chatstates-confirm state)
	    (setq jabber-chatstates-requested t)
	    (add-hook 'post-command-hook 'jabber-chatstates-after-change nil t))

	  (setq jabber-chatstates-last-state state)
	  (jabber-chatstates-update-message)))))))

;; Add function last in chain, so a chat buffer is already created.
(add-to-list 'jabber-message-chain 'jabber-handle-incoming-message-chatstates t)

(jabber-disco-advertise-feature "http://jabber.org/protocol/chatstates")

(provide 'jabber-chatstates)
;; arch-tag: d879de90-51e1-11dc-909d-000a95c2fcd0
