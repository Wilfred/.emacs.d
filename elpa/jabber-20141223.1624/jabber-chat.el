;; jabber-chat.el - one-to-one chats

;; Copyright (C) 2005, 2007, 2008 - Magnus Henoch - mange@freemail.hu

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'jabber-core)
(require 'jabber-chatbuffer)
(require 'jabber-history)
(require 'jabber-menu)                  ;we need jabber-jid-chat-menu
(require 'ewoc)
(eval-when-compile (require 'cl))

(defgroup jabber-chat nil "chat display options"
  :group 'jabber)

(defcustom jabber-chat-buffer-format "*-jabber-chat-%n-*"
  "The format specification for the name of chat buffers.

These fields are available (all are about the person you are chatting
with):

%n   Nickname, or JID if no nickname set
%j   Bare JID (without resource)
%r   Resource"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-header-line-format
  '("" (jabber-chat-buffer-show-avatar
	(:eval 
	 (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
	   (jabber-propertize " "
			      'display (get buddy 'avatar)))))
    (:eval (jabber-jid-displayname jabber-chatting-with))
    "\t" (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
		  (propertize 
		   (or
		    (cdr (assoc (get buddy 'show) jabber-presence-strings))
		    (get buddy 'show))
		   'face
		   (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
		       'jabber-roster-user-online))))
    "\t" (:eval (jabber-fix-status (get (jabber-jid-symbol jabber-chatting-with) 'status)))
    "\t" jabber-events-message		;see jabber-events.el
    "\t" jabber-chatstates-message)		;see jabber-chatstates.el
  "The specification for the header line of chat buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)

(defcustom jabber-chat-buffer-show-avatar t
  "Show avatars in header line of chat buffer?
This variable might not take effect if you have changed
`jabber-chat-header-line-format'."
  :type 'boolean
  :group 'jabber-chat)

(defcustom jabber-chat-time-format "%H:%M"
  "The format specification for instant messages in the chat buffer.
See also `jabber-chat-delayed-time-format'.

See `format-time-string' for valid values."
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-delayed-time-format "%Y-%m-%d %H:%M"
  "The format specification for delayed messages in the chat buffer.
See also `jabber-chat-time-format'.

See `format-time-string' for valid values."
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-print-rare-time t
  "Non-nil means to print \"rare time\" indications in chat buffers.
The default settings tell every new hour."
  :type 'boolean
  :group 'jabber-chat)

(defcustom jabber-rare-time-format "%a %e %b %Y %H:00"
  "The format specification for the rare time information.
Rare time information will be printed whenever the current time,
formatted according to this string, is different to the last
rare time printed."
  :type 'string
  :group 'jabber-chat)

(defface jabber-rare-time-face
  '((t (:foreground "darkgreen" :underline t)))
  "face for displaying the rare time info"
  :group 'jabber-chat)

(defcustom jabber-chat-local-prompt-format "[%t] %n> "
  "The format specification for lines you type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
     or `jabber-chat-delayed-time-format'
%u   Username
%n   Nickname (obsolete, same as username)
%r   Resource
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-foreign-prompt-format "[%t] %n> "
  "The format specification for lines others type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
     or `jabber-chat-delayed-time-format'
%n   Nickname, or JID if no nickname set
%u   Username
%r   Resource
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-system-prompt-format "[%t] *** "
  "The format specification for lines from the system or that are special in the chat buffer."
  :type 'string
  :group 'jabber-chat)

(defface jabber-chat-prompt-local
  '((t (:foreground "blue" :weight bold)))
  "face for displaying the chat prompt for what you type in"
  :group 'jabber-chat)

(defface jabber-chat-prompt-foreign
  '((t (:foreground "red" :weight bold)))
  "face for displaying the chat prompt for what they send"
  :group 'jabber-chat)

(defface jabber-chat-prompt-system
  '((t (:foreground "green" :weight bold)))
  "face used for system and special messages"
  :group 'jabber-chat)

(defface jabber-chat-text-local '((t ()))
  "Face used for text you write"
  :group 'jabber-chat)

(defface jabber-chat-text-foreign '((t ()))
  "Face used for text others write"
  :group 'jabber-chat)

(defface jabber-chat-error
  '((t (:foreground "red" :weight bold)))
  "Face used for error messages"
  :group 'jabber-chat)

;;;###autoload
(defvar jabber-chatting-with nil
  "JID of the person you are chatting with")

(defvar jabber-chat-printers '(jabber-chat-print-subject
			       jabber-chat-print-body
			       jabber-chat-print-url
			       jabber-chat-goto-address)
  "List of functions that may be able to print part of a message.
Each function receives these arguments:

XML-DATA   The entire message stanza
WHO        :local or :foreign, for sent or received stanza, respectively
MODE       :insert or :printp.  For :insert, insert text at point.
           For :printp, return non-nil if function would insert text.")

(defvar jabber-body-printers '(jabber-chat-normal-body)
  "List of functions that may be able to print a body for a message.
Each function receives these arguments:

XML-DATA   The entire message stanza
WHO        :local, :foreign or :error
MODE       :insert or :printp.  For :insert, insert text at point.
           For :printp, return non-nil if function would insert text.

These functions are called in order, until one of them returns
non-nil.

Add a function to the beginning of this list if the tag it handles
replaces the contents of the <body/> tag.")

(defvar jabber-chat-send-hooks nil
  "List of functions called when a chat message is sent.
The arguments are the text to send, and the id attribute of the
message.

The functions should return a list of XML nodes they want to be
added to the outgoing message.")

(defvar jabber-chat-earliest-backlog nil
  "Float-time of earliest backlog entry inserted into buffer.
nil if no backlog has been inserted.")

;;;###autoload
(defun jabber-chat-get-buffer (chat-with)
  "Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-chat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname chat-with))
		(cons ?j (jabber-jid-user chat-with))
		(cons ?r (or (jabber-jid-resource chat-with) "")))))

(defun jabber-chat-create-buffer (jc chat-with)
  "Prepare a buffer for chatting with CHAT-WITH.
This function is idempotent."
  (with-current-buffer (get-buffer-create (jabber-chat-get-buffer chat-with))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode jc #'jabber-chat-pp)

      (make-local-variable 'jabber-chatting-with)
      (setq jabber-chatting-with chat-with)
      (setq jabber-send-function 'jabber-chat-send)
      (setq header-line-format jabber-chat-header-line-format)

      (make-local-variable 'jabber-chat-earliest-backlog)

      ;; insert backlog
      (when (null jabber-chat-earliest-backlog)
	(let ((backlog-entries (jabber-history-backlog chat-with)))
	  (if (null backlog-entries)
	      (setq jabber-chat-earliest-backlog (jabber-float-time))
	    (setq jabber-chat-earliest-backlog
		  (jabber-float-time (jabber-parse-time
				      (aref (car backlog-entries) 0))))
	    (mapc 'jabber-chat-insert-backlog-entry (nreverse backlog-entries))))))

    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)

    (current-buffer)))

(defun jabber-chat-insert-backlog-entry (msg)
  "Insert backlog entry MSG at beginning of buffer."
  ;; Rare timestamps are especially important in backlog.  We risk
  ;; having superfluous timestamps if we just add before each backlog
  ;; entry.
  (let* ((message-time (jabber-parse-time (aref msg 0)))
	 (fake-stanza `(message ((from . ,(aref msg 2)))
				(body nil ,(aref msg 4))
				(x ((xmlns . "jabber:x:delay")
				    (stamp . ,(jabber-encode-legacy-time message-time))))))
	 (node-data (list (if (string= (aref msg 1) "in") :foreign :local)
			  fake-stanza :delayed t)))

    ;; Insert after existing rare timestamp?
    (if (and jabber-print-rare-time
	     (ewoc-nth jabber-chat-ewoc 0)
	     (eq (car (ewoc-data (ewoc-nth jabber-chat-ewoc 0))) :rare-time)
	     (not (jabber-rare-time-needed message-time (cadr (ewoc-data (ewoc-nth jabber-chat-ewoc 0))))))
	(ewoc-enter-after jabber-chat-ewoc (ewoc-nth jabber-chat-ewoc 0) node-data)
      ;; Insert first.
      (ewoc-enter-first jabber-chat-ewoc node-data)
      (when jabber-print-rare-time
	(ewoc-enter-first jabber-chat-ewoc (list :rare-time message-time))))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Display more context" 'jabber-chat-display-more-backlog))

(defun jabber-chat-display-more-backlog (how-many)
  "Display more context. HOW-MANY is number of messages. Specify 0 to display all messages."
  (interactive "nHow many more messages (Specify 0 to display all)? ")
  (let* ((inhibit-read-only t)
	 (jabber-backlog-days nil)
	 (jabber-backlog-number (if (= how-many 0) t how-many))
	 (backlog-entries (jabber-history-backlog
			   jabber-chatting-with jabber-chat-earliest-backlog)))
    (when backlog-entries
      (setq jabber-chat-earliest-backlog 
	    (jabber-float-time (jabber-parse-time
				(aref (car backlog-entries) 0))))
      (save-excursion
	(goto-char (point-min))
	(mapc 'jabber-chat-insert-backlog-entry (nreverse backlog-entries))))))

(add-to-list 'jabber-message-chain 'jabber-process-chat)

(defun jabber-process-chat (jc xml-data)
  "If XML-DATA is a one-to-one chat message, handle it as such."
  ;; For now, everything that is not a public MUC message is
  ;; potentially a 1to1 chat message.
  (when (not (jabber-muc-message-p xml-data))
    ;; Note that we handle private MUC messages here.
    (let ((from (jabber-xml-get-attribute xml-data 'from))
	  (error-p (jabber-xml-get-children xml-data 'error))
	  (body-text (car (jabber-xml-node-children
			   (car (jabber-xml-get-children
				 xml-data 'body))))))
      ;; First check if we would output anything for this stanza.
      (when (or error-p
		(run-hook-with-args-until-success 'jabber-chat-printers xml-data :foreign :printp))
	;; If so, create chat buffer, if necessary...
	(with-current-buffer (if (jabber-muc-sender-p from)
				 (jabber-muc-private-create-buffer
				  jc
				  (jabber-jid-user from)
				  (jabber-jid-resource from))
			       (jabber-chat-create-buffer jc from))
	  ;; ...add the message to the ewoc...
	  (let ((node
		 (ewoc-enter-last jabber-chat-ewoc (list (if error-p :error :foreign) xml-data :time (current-time)))))
	    (jabber-maybe-print-rare-time node))

	  ;; ...and call alert hooks.
	  (dolist (hook '(jabber-message-hooks jabber-alert-message-hooks))
	    (run-hook-with-args hook
				from (current-buffer) body-text
				(funcall jabber-alert-message-function 
					 from (current-buffer) body-text))))))))

(defun jabber-chat-send (jc body)
  "Send BODY through connection JC, and display it in chat buffer."
  ;; Build the stanza...
  (let* ((id (apply 'format "emacs-msg-%d.%d.%d" (current-time)))
	 (stanza-to-send `(message 
			   ((to . ,jabber-chatting-with)
			    (type . "chat")
			    (id . ,id))
			   (body () ,body))))
    ;; ...add additional elements...
    ;; TODO: Once we require Emacs 24.1, use `run-hook-wrapped' instead.
    ;; That way we don't need to eliminate the "local hook" functionality
    ;; here.
    (dolist (hook jabber-chat-send-hooks)
      (if (eq hook t)
	  ;; Local hook referring to global...
	  (when (local-variable-p 'jabber-chat-send-hooks)
	    (dolist (global-hook (default-value 'jabber-chat-send-hooks))
	      (nconc stanza-to-send (funcall global-hook body id))))
      (nconc stanza-to-send (funcall hook body id))))
    ;; ...display it, if it would be displayed.
    (when (run-hook-with-args-until-success 'jabber-chat-printers stanza-to-send :local :printp)
      (jabber-maybe-print-rare-time
       (ewoc-enter-last jabber-chat-ewoc (list :local stanza-to-send :time (current-time)))))
    ;; ...and send it...
    (jabber-send-sexp jc stanza-to-send)))

(defun jabber-chat-pp (data)
  "Pretty-print a <message/> stanza.
\(car data) is either :local, :foreign, :error or :notice.
\(cadr data) is the <message/> stanza.
This function is used as an ewoc prettyprinter."
  (let* ((beg (point))
	 (original-timestamp (when (listp (cadr data))
			       (jabber-message-timestamp (cadr data))))
	 (internal-time
	  (plist-get (cddr data) :time))
	 (body (ignore-errors (car
	       (jabber-xml-node-children
		(car
		 (jabber-xml-get-children (cadr data) 'body))))))
	 (/me-p
	  (and (> (length body) 4)
	       (string= (substring body 0 4) "/me "))))

    ;; Print prompt...
    (let ((delayed (or original-timestamp (plist-get (cddr data) :delayed)))
	  (prompt-start (point)))
      (case (car data)
	(:local
	 (jabber-chat-self-prompt (or original-timestamp internal-time)
				  delayed
				  /me-p))
	(:foreign
	 (if (and (listp (cadr data))
		  (jabber-muc-private-message-p (cadr data)))
	     (jabber-muc-private-print-prompt (cadr data))
	   ;; For :error and :notice, this might be a string... beware
	   (jabber-chat-print-prompt (when (listp (cadr data)) (cadr data))
				     (or original-timestamp internal-time)
				     delayed
				     /me-p)))
	((:error :notice :subscription-request)
	 (jabber-chat-system-prompt (or original-timestamp internal-time)))
	(:muc-local
	 (jabber-muc-print-prompt (cadr data) t /me-p))
        (:muc-foreign
         (jabber-muc-print-prompt (cadr data) nil /me-p))
	((:muc-notice :muc-error)
	 (jabber-muc-system-prompt)))
      (put-text-property prompt-start (point) 'field 'jabber-prompt))
    
    ;; ...and body
    (case (car data)
      ((:local :foreign)
       (run-hook-with-args 'jabber-chat-printers (cadr data) (car data) :insert))
      ((:muc-local :muc-foreign)
       (let ((printers (append jabber-muc-printers jabber-chat-printers)))
	 (run-hook-with-args 'printers (cadr data) (car data) :insert)))
      ((:error :muc-error)
       (if (stringp (cadr data))
	    (insert (jabber-propertize (cadr data) 'face 'jabber-chat-error))
	 (jabber-chat-print-error (cadr data))))
      ((:notice :muc-notice)
       (insert (cadr data)))
      (:rare-time
       (insert (jabber-propertize (format-time-string jabber-rare-time-format (cadr data))
				  'face 'jabber-rare-time-face)))
      (:subscription-request
       (insert "This user requests subscription to your presence.\n")
       (when (and (stringp (cadr data)) (not (zerop (length (cadr data)))))
	 (insert "Message: " (cadr data) "\n"))
       (insert "Accept?\n\n")
       (flet ((button
	       (text action)
	       (if (fboundp 'insert-button)
		   (insert-button text 'action action)
		 ;; simple button replacement
		 (let ((keymap (make-keymap)))
		   (define-key keymap "\r" action)
		   (insert (jabber-propertize text 'keymap keymap 'face 'highlight))))
	       (insert "\t")))
	 (button "Mutual" 'jabber-subscription-accept-mutual)
	 (button "One-way" 'jabber-subscription-accept-one-way)
	 (button "Decline" 'jabber-subscription-decline))))

    (when jabber-chat-fill-long-lines
      (save-restriction
	(narrow-to-region beg (point))
	(jabber-chat-buffer-fill-long-lines)))

    (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point) 'front-sticky t)
    (put-text-property beg (point) 'rear-nonsticky t)))

(defun jabber-rare-time-needed (time1 time2)
  "Return non-nil if a timestamp should be printed between TIME1 and TIME2."
  (not (string= (format-time-string jabber-rare-time-format time1)
		(format-time-string jabber-rare-time-format time2))))

(defun jabber-maybe-print-rare-time (node)
  "Print rare time before NODE, if appropriate."
  (let* ((prev (ewoc-prev jabber-chat-ewoc node))
	 (data (ewoc-data node))
	 (prev-data (when prev (ewoc-data prev))))
    (flet ((entry-time (entry)
		       (or (when (listp (cadr entry))
			     (jabber-message-timestamp (cadr entry))
			     (plist-get (cddr entry) :time)))))
      (when (and jabber-print-rare-time
		 (or (null prev)
		     (jabber-rare-time-needed (entry-time prev-data)
					      (entry-time data))))
	(ewoc-enter-before jabber-chat-ewoc node
			   (list :rare-time (entry-time data)))))))

(defun jabber-chat-print-prompt (xml-data timestamp delayed dont-print-nick-p)
  "Print prompt for received message in XML-DATA.
TIMESTAMP is the timestamp to print, or nil to get it
from a jabber:x:delay element.
If DELAYED is true, print long timestamp
\(`jabber-chat-delayed-time-format' as opposed to
`jabber-chat-time-format').
If DONT-PRINT-NICK-P is true, don't include nickname."
  (let ((from (jabber-xml-get-attribute xml-data 'from))
	(timestamp (or timestamp (jabber-message-timestamp xml-data))))
    (insert (jabber-propertize 
	     (format-spec jabber-chat-foreign-prompt-format
			  (list
			   (cons ?t (format-time-string 
				     (if delayed
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n (if dont-print-nick-p "" (jabber-jid-displayname from)))
			   (cons ?u (or (jabber-jid-username from) from))
			   (cons ?r (jabber-jid-resource from))
			   (cons ?j (jabber-jid-user from))))
	     'face 'jabber-chat-prompt-foreign
	     'help-echo
	     (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " from)))))

(defun jabber-chat-system-prompt (timestamp)
  (insert (jabber-propertize 
	   (format-spec jabber-chat-foreign-prompt-format
			(list
			 (cons ?t (format-time-string jabber-chat-time-format
						      timestamp))
			 (cons ?n "")
			 (cons ?u "")
			 (cons ?r "")
			 (cons ?j "")))
	   'face 'jabber-chat-prompt-system
	   'help-echo
	   (concat (format-time-string "System message on %Y-%m-%d %H:%M:%S" timestamp)))))

(defun jabber-chat-self-prompt (timestamp delayed dont-print-nick-p)
  "Print prompt for sent message.
TIMESTAMP is the timestamp to print, or nil for now.
If DELAYED is true, print long timestamp
\(`jabber-chat-delayed-time-format' as opposed to
`jabber-chat-time-format').
If DONT-PRINT-NICK-P is true, don't include nickname."
  (let* ((state-data (fsm-get-state-data jabber-buffer-connection))
	 (username (plist-get state-data :username))
	 (server (plist-get state-data :server))
	 (resource (plist-get state-data :resource))
	 (nickname username))
    (insert (jabber-propertize 
	     (format-spec jabber-chat-local-prompt-format
			  (list
			   (cons ?t (format-time-string 
				     (if delayed
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n (if dont-print-nick-p "" nickname))
			   (cons ?u username)
			   (cons ?r resource)
			   (cons ?j (concat username "@" server))))
	     'face 'jabber-chat-prompt-local
	     'help-echo
	     (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from you")))))

(defun jabber-chat-print-error (xml-data)
  "Print error in given <message/> in a readable way."
  (let ((the-error (car (jabber-xml-get-children xml-data 'error))))
    (insert 
     (jabber-propertize
      (concat "Error: " (jabber-parse-error the-error))
      'face 'jabber-chat-error))))

(defun jabber-chat-print-subject (xml-data who mode)
  "Print subject of given <message/>, if any."
  (let ((subject (car
		  (jabber-xml-node-children
		   (car
		    (jabber-xml-get-children xml-data 'subject))))))
    (when (not (zerop (length subject)))
      (case mode
	(:printp
	 t)
	(:insert
	 (insert (jabber-propertize
		  "Subject: " 'face 'jabber-chat-prompt-system)
		 (jabber-propertize
		  subject
		  'face 'jabber-chat-text-foreign)
		 "\n"))))))

(defun jabber-chat-print-body (xml-data who mode)
  (run-hook-with-args-until-success 'jabber-body-printers xml-data who mode))

(defun jabber-chat-normal-body (xml-data who mode)
  "Print body for received message in XML-DATA."
  (let ((body (car
	       (jabber-xml-node-children
		(car
		 (jabber-xml-get-children xml-data 'body))))))
    (when body

      (when (eql mode :insert)
	(if (and (> (length body) 4)
		 (string= (substring body 0 4) "/me "))
	    (let ((action (substring body 4))
		  (nick (cond
			 ((eq who :local)
			  (plist-get (fsm-get-state-data jabber-buffer-connection) :username))
			 ((or (jabber-muc-message-p xml-data)
			      (jabber-muc-private-message-p xml-data))
			  (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
			 (t
			  (jabber-jid-displayname (jabber-xml-get-attribute xml-data 'from))))))
	      (insert (jabber-propertize
		       (concat nick
			       " "
			       action)
		       'face 'jabber-chat-prompt-system)))
	  (insert (jabber-propertize 
		   body
		   'face (case who
			   ((:foreign :muc-foreign) 'jabber-chat-text-foreign)
			   ((:local :muc-local) 'jabber-chat-text-local))))))
      t)))

(defun jabber-chat-print-url (xml-data who mode)
  "Print URLs provided in jabber:x:oob namespace."
  (let ((foundp nil))
    (dolist (x (jabber-xml-node-children xml-data))
      (when (and (listp x) (eq (jabber-xml-node-name x) 'x)
		 (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:oob"))
	(setq foundp t)
	
	(when (eql mode :insert)
	  (let ((url (car (jabber-xml-node-children
			   (car (jabber-xml-get-children x 'url)))))
		(desc (car (jabber-xml-node-children
			    (car (jabber-xml-get-children x 'desc))))))
	    (insert "\n"
                    (jabber-propertize
		     "URL: " 'face 'jabber-chat-prompt-system)
                    (format "%s <%s>" desc url))))))
    foundp))

(defun jabber-chat-goto-address (xml-data who mode)
  "Call `goto-address' on the newly written text."
  (when (eq mode :insert)
    (ignore-errors
      ;; `goto-address' is autoloaded, but `goto-address-fontify' is not.
      (require 'goto-addr)
      (let ((end (point))
	    (limit (max (- (point) 1000) (1+ (point-min)))))
	;; We only need to fontify the text written since the last
	;; prompt.  The prompt has a field property, so we can find it
	;; using `field-beginning'.
	(goto-address-fontify (field-beginning nil nil limit) end)))))

;; jabber-compose is autoloaded in jabber.el
(add-to-list 'jabber-jid-chat-menu
	     (cons "Compose message" 'jabber-compose))

(defun jabber-send-message (jc to subject body type)
  "send a message tag to the server"
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "subject: ")
		     (jabber-read-with-input-method "body: ")
		     (read-string "type: ")))
  (jabber-send-sexp jc
		    `(message ((to . ,to)
                               ,(if (> (length type) 0)
                                    `(type . ,type)))
                              ,(if (> (length subject) 0)
                                   `(subject () ,subject))
                              ,(if (> (length body) 0)
                                   `(body () ,body))))
  (if (and jabber-history-enabled (not (string= type "groupchat")))
      (jabber-history-log-message "out" nil to body (current-time))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Start chat" 'jabber-chat-with))

(defun jabber-chat-with (jc jid &optional other-window)
  "Open an empty chat window for chatting with JID.
With a prefix argument, open buffer in other window.
Returns the chat buffer."
  (interactive (let ((jid
		      (jabber-read-jid-completing "chat with:"))
		     (account
		      (jabber-read-account)))
		 (list 
		  account jid current-prefix-arg)))
  (let ((buffer (jabber-chat-create-buffer jc jid)))
    (if other-window
	(switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

(defun jabber-chat-with-jid-at-point (&optional other-window)
  "Start chat with JID at point.
Signal an error if there is no JID at point.
With a prefix argument, open buffer in other window."
  (interactive "P")
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid))
	(account (get-text-property (point)
				    'jabber-account)))
    (if (and jid-at-point account)
	(jabber-chat-with account jid-at-point other-window)
      (error "No contact at point"))))

(provide 'jabber-chat)

;; arch-tag: f423eb92-aa87-475b-b590-48c93ccba9be
