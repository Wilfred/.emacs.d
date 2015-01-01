;;; jabber-rtt.el --- XEP-0301: In-Band Real Time Text

;; Copyright (C) 2013  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile (require 'cl))

;;;; Handling incoming events

;;;###autoload
(eval-after-load "jabber-disco"
  '(jabber-disco-advertise-feature "urn:xmpp:rtt:0"))

(defvar jabber-rtt-ewoc-node nil)
(make-variable-buffer-local 'jabber-rtt-ewoc-node)

(defvar jabber-rtt-last-seq nil)
(make-variable-buffer-local 'jabber-rtt-last-seq)

(defvar jabber-rtt-message nil)
(make-variable-buffer-local 'jabber-rtt-message)

(defvar jabber-rtt-pending-events nil)
(make-variable-buffer-local 'jabber-rtt-pending-events)

(defvar jabber-rtt-timer nil)
(make-variable-buffer-local 'jabber-rtt-timer)

;; Add function last in chain, so a chat buffer is already created.
;;;###autoload
(eval-after-load "jabber-core"
  '(add-to-list 'jabber-message-chain #'jabber-rtt-handle-message t))

;;;###autoload
(defun jabber-rtt-handle-message (jc xml-data)
  ;; We could support this for MUC as well, if useful.
  (when (and (not (jabber-muc-message-p xml-data))
	     (get-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))))
    (with-current-buffer (jabber-chat-get-buffer (jabber-xml-get-attribute xml-data 'from))
      (let* ((rtt (jabber-xml-path xml-data '(("urn:xmpp:rtt:0" . "rtt"))))
	     (body (jabber-xml-path xml-data '(body)))
	     (seq (when rtt (jabber-xml-get-attribute rtt 'seq)))
	     (event (when rtt (or (jabber-xml-get-attribute rtt 'event) "edit")))
	     (actions (when rtt (jabber-xml-node-children rtt)))
	     (inhibit-read-only t))
	(cond
	 ((or body (string= event "cancel"))
	  ;; A <body/> element supersedes real time text.
	  (jabber-rtt--reset))
	 ((member event '("new" "reset"))
	  (jabber-rtt--reset)
	  (setq jabber-rtt-ewoc-node
		(ewoc-enter-last jabber-chat-ewoc (list :notice "[typing...]"))
		jabber-rtt-last-seq (string-to-number seq)
		jabber-rtt-message ""
		jabber-rtt-pending-events nil)
	  (jabber-rtt--enqueue-actions actions))
	 ((string= event "edit")
	  ;; TODO: check whether this works properly in 32-bit Emacs
	  (cond
	   ((and jabber-rtt-last-seq
		 (equal (1+ jabber-rtt-last-seq)
			(string-to-number seq)))
	    ;; We are in sync.
	    (setq jabber-rtt-last-seq (string-to-number seq))
	    (jabber-rtt--enqueue-actions actions))
	   (t
	    ;; TODO: show warning when not in sync
	    (message "out of sync! %s vs %s"
		     seq jabber-rtt-last-seq))
	  ))
	 ;; TODO: handle event="init"
	 )))))

(defun jabber-rtt--reset ()
  (when jabber-rtt-ewoc-node
    (ewoc-delete jabber-chat-ewoc jabber-rtt-ewoc-node))
  (when (timerp jabber-rtt-timer)
    (cancel-timer jabber-rtt-timer))
  (setq jabber-rtt-ewoc-node nil
	jabber-rtt-last-seq nil
	jabber-rtt-message nil
	jabber-rtt-pending-events nil
	jabber-rtt-timer nil))

(defun jabber-rtt--enqueue-actions (new-actions)
  (setq jabber-rtt-pending-events
	;; Ensure that the queue never contains more than 700 ms worth
	;; of wait events.
	(jabber-rtt--fix-waits (append jabber-rtt-pending-events new-actions)))
  (unless jabber-rtt-timer
    (jabber-rtt--process-actions (current-buffer))))

(defun jabber-rtt--process-actions (buffer)
  (with-current-buffer buffer
    (setq jabber-rtt-timer nil)
    (catch 'wait
      (while jabber-rtt-pending-events
	(let ((action (pop jabber-rtt-pending-events)))
	  (case (jabber-xml-node-name action)
	    ((t)
	     ;; insert text
	     (let* ((p (jabber-xml-get-attribute action 'p))
		    (position (if p (string-to-number p) (length jabber-rtt-message))))
	       (setq position (max position 0))
	       (setq position (min position (length jabber-rtt-message)))
	       (setf (substring jabber-rtt-message position position)
		     (car (jabber-xml-node-children action)))

	       (ewoc-set-data jabber-rtt-ewoc-node (list :notice (concat "[typing...] " jabber-rtt-message)))
	       (let ((inhibit-read-only t))
		 (ewoc-invalidate jabber-chat-ewoc jabber-rtt-ewoc-node))))
	    ((e)
	     ;; erase text
	     (let* ((p (jabber-xml-get-attribute action 'p))
		    (position (if p (string-to-number p) (length jabber-rtt-message)))
		    (n (jabber-xml-get-attribute action 'n))
		    (number (if n (string-to-number n) 1)))
	       (setq position (max position 0))
	       (setq position (min position (length jabber-rtt-message)))
	       (setq number (max number 0))
	       (setq number (min number position))
	       ;; Now erase the NUMBER characters before POSITION.
	       (setf (substring jabber-rtt-message (- position number) position)
		     "")

	       (ewoc-set-data jabber-rtt-ewoc-node (list :notice (concat "[typing...] " jabber-rtt-message)))
	       (let ((inhibit-read-only t))
		 (ewoc-invalidate jabber-chat-ewoc jabber-rtt-ewoc-node))))
	    ((w)
	     (setq jabber-rtt-timer
		   (run-with-timer
		    (/ (string-to-number (jabber-xml-get-attribute action 'n)) 1000.0)
		    nil
		    #'jabber-rtt--process-actions
		    buffer))
	     (throw 'wait nil))))))))

(defun jabber-rtt--fix-waits (actions)
  ;; Ensure that the sum of all wait events is no more than 700 ms.
  (let ((sum 0))
    (dolist (action actions)
      (when (eq (jabber-xml-node-name action) 'w)
	(let ((n (jabber-xml-get-attribute action 'n)))
	  (setq n (string-to-number n))
	  (when (>= n 0)
	    (setq sum (+ sum n))))))

    (if (<= sum 700)
	actions
      (let ((scale (/ 700.0 sum)))
	(mapcar
	 (lambda (action)
	   (if (eq (jabber-xml-node-name action) 'w)
	       (let ((n (jabber-xml-get-attribute action 'n)))
		 (setq n (string-to-number n))
		 (setq n (max n 0))
		 `(w ((n . ,(number-to-string (* scale n)))) nil))
	     action))
	 actions)))))

;;;; Sending events

(defvar jabber-rtt-send-timer nil)
(make-variable-buffer-local 'jabber-rtt-send-timer)

(defvar jabber-rtt-send-seq nil)
(make-variable-buffer-local 'jabber-rtt-send-seq)

(defvar jabber-rtt-outgoing-events nil)
(make-variable-buffer-local 'jabber-rtt-outgoing-events)

(defvar jabber-rtt-send-last-timestamp nil)
(make-variable-buffer-local 'jabber-rtt-send-last-timestamp)

;;;###autoload
(define-minor-mode jabber-rtt-send-mode
  "Show text to recipient as it is being typed.
This lets the recipient see every change made to the message up
until it's sent.  The recipient's client needs to implement
XEP-0301, In-Band Real Time Text."
  nil " Real-Time" nil
  (if (null jabber-rtt-send-mode)
      (progn
	(remove-hook 'after-change-functions #'jabber-rtt--queue-update t)
	(remove-hook 'jabber-chat-send-hooks #'jabber-rtt--message-sent t)
	(jabber-rtt--cancel-send))
    (unless (derived-mode-p 'jabber-chat-mode)
      (error "Real Time Text only makes sense in chat buffers"))
    (when (timerp jabber-rtt-send-timer)
      (cancel-timer jabber-rtt-send-timer))
    (setq jabber-rtt-send-timer nil
	  jabber-rtt-send-seq nil
	  jabber-rtt-outgoing-events nil
	  jabber-rtt-send-last-timestamp nil)
    (jabber-rtt--send-current-text nil)
    (add-hook 'after-change-functions #'jabber-rtt--queue-update nil t)
    (add-hook 'jabber-chat-send-hooks #'jabber-rtt--message-sent nil t)))

(defun jabber-rtt--cancel-send ()
  (when (timerp jabber-rtt-send-timer)
    (cancel-timer jabber-rtt-send-timer))
  (setq jabber-rtt-send-seq (1+ jabber-rtt-send-seq))
  (jabber-send-sexp jabber-buffer-connection
		    `(message ((to . ,jabber-chatting-with)
			       (type . "chat"))
			      (rtt ((xmlns . "urn:xmpp:rtt:0")
				    (seq . ,(number-to-string jabber-rtt-send-seq))
				    (event . "cancel"))
				   nil)))
  (setq jabber-rtt-send-timer nil
	jabber-rtt-send-seq nil
	jabber-rtt-outgoing-events nil
	jabber-rtt-send-last-timestamp nil))

(defun jabber-rtt--send-current-text (resetp)
  (let ((text (buffer-substring-no-properties jabber-point-insert (point-max))))
    ;; This should give us enough room to avoid wrap-arounds, even
    ;; with just 28 bits...
    (setq jabber-rtt-send-seq (random 100000))
    (jabber-send-sexp jabber-buffer-connection
		      `(message ((to . ,jabber-chatting-with)
				 (type . "chat"))
				(rtt ((xmlns . "urn:xmpp:rtt:0")
				      (seq . ,(number-to-string jabber-rtt-send-seq))
				      (event . ,(if resetp "reset" "new")))
				     (t () ,text))))))

(defun jabber-rtt--queue-update (beg end pre-change-length)
  (unless (or (< beg jabber-point-insert)
	      (< end jabber-point-insert))
    (let ((timestamp (current-time)))
      (when jabber-rtt-send-last-timestamp
	(let* ((time-difference (time-subtract timestamp jabber-rtt-send-last-timestamp))
	       (interval (truncate (* 1000 (float-time time-difference)))))
	  (when (and (> interval 0)
		     ;; Don't send too long intervals - this should have
		     ;; been sent by our timer already.
		     (< interval 1000))
	    (push `(w ((n . ,(number-to-string interval))) nil)
		  jabber-rtt-outgoing-events))))
      (setq jabber-rtt-send-last-timestamp timestamp))

    (when (> pre-change-length 0)
      ;; Some text was deleted.  Let's check if we can use a shorter
      ;; tag:
      (let ((at-end (= end (point-max)))
	    (erase-one (= pre-change-length 1)))
	(push `(e (
		   ,@(unless at-end
		       `((p . ,(number-to-string
				(+ beg
				   (- jabber-point-insert)
				   pre-change-length)))))
		   ,@(unless erase-one
		       `((n . ,(number-to-string pre-change-length))))))
	      jabber-rtt-outgoing-events)))

    (when (/= beg end)
      ;; Some text was inserted.
      (let ((text (buffer-substring-no-properties beg end))
	    (at-end (= end (point-max))))
	(push `(t (
		   ,@(unless at-end
		       `((p . ,(number-to-string (- beg jabber-point-insert))))))
		  ,text)
	      jabber-rtt-outgoing-events)))

    (when (null jabber-rtt-send-timer)
      (setq jabber-rtt-send-timer
	    (run-with-timer 0.7 nil #'jabber-rtt--send-queued-events (current-buffer))))))

(defun jabber-rtt--send-queued-events (buffer)
  (with-current-buffer buffer
    (setq jabber-rtt-send-timer nil)
    (when jabber-rtt-outgoing-events
      (let ((event (if jabber-rtt-send-seq "edit" "new")))
	(setq jabber-rtt-send-seq
	      (if jabber-rtt-send-seq
		  (1+ jabber-rtt-send-seq)
		(random 100000)))
	(jabber-send-sexp jabber-buffer-connection
			  `(message ((to . ,jabber-chatting-with)
				     (type . "chat"))
				    (rtt ((xmlns . "urn:xmpp:rtt:0")
					  (seq . ,(number-to-string jabber-rtt-send-seq))
					  (event . ,event))
					 ,@(nreverse jabber-rtt-outgoing-events))))
	(setq jabber-rtt-outgoing-events nil)))))

(defun jabber-rtt--message-sent (_text _id)
  ;; We're sending a <body/> element; reset our state
  (when (timerp jabber-rtt-send-timer)
    (cancel-timer jabber-rtt-send-timer))
  (setq jabber-rtt-send-timer nil
	jabber-rtt-send-seq nil
	jabber-rtt-outgoing-events nil
	jabber-rtt-send-last-timestamp nil))

(provide 'jabber-rtt)
;;; jabber-rtt.el ends here
