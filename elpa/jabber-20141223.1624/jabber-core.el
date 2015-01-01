;; jabber-core.el - core functions

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

;; SSL-Connection Parts:
;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni

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

(require 'cl)

(require 'jabber-util)
(require 'jabber-logon)
(require 'jabber-conn)
(require 'fsm)

(require 'jabber-sasl)
(require 'jabber-console)

(defvar jabber-connections nil
  "List of jabber-connection FSMs.")

(defvar *jabber-roster* nil
  "the roster list")

(defvar jabber-jid-obarray (make-vector 127 0)
  "obarray for keeping JIDs")

(defvar *jabber-disconnecting* nil
  "boolean - are we in the process of disconnecting by free will")

(defvar jabber-message-chain nil
  "Incoming messages are sent to these functions, in order.")

(defvar jabber-iq-chain nil
  "Incoming infoqueries are sent to these functions, in order.")

(defvar jabber-presence-chain nil
  "Incoming presence notifications are sent to these functions, in order.")

(defvar jabber-namespace-prefixes nil
  "XML namespace prefixes used for the current connection.")
(make-variable-buffer-local 'jabber-namespace-prefixes)

(defgroup jabber-core nil "customize core functionality"
  :group 'jabber)

(defcustom jabber-post-connect-hooks '(jabber-send-current-presence
				       jabber-muc-autojoin
				       jabber-whitespace-ping-start
				       jabber-vcard-avatars-find-current)
  "*Hooks run after successful connection and authentication.
The functions should accept one argument, the connection object."
  :type 'hook
  :options '(jabber-send-current-presence
	     jabber-muc-autojoin
	     jabber-whitespace-ping-start
	     jabber-keepalive-start
	     jabber-vcard-avatars-find-current
	     jabber-autoaway-start)
  :group 'jabber-core)

(defcustom jabber-pre-disconnect-hook nil
  "*Hooks run just before voluntary disconnection
This might be due to failed authentication."
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-lost-connection-hooks nil
  "*Hooks run after involuntary disconnection.
The functions are called with one argument: the connection object."
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-post-disconnect-hook nil
  "*Hooks run after disconnection"
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-auto-reconnect nil
  "Reconnect automatically after losing connection?
This will be of limited use unless you have the password library
installed, and have configured it to cache your password
indefinitely.  See `password-cache' and `password-cache-expiry'."
  :type 'boolean
  :group 'jabber-core)

(defcustom jabber-reconnect-delay 5
  "Seconds to wait before reconnecting"
  :type 'integer
  :group 'jabber-core)

(defcustom jabber-roster-buffer "*-jabber-roster-*"
  "The name of the roster buffer"
  :type 'string
  :group 'jabber-core)

(defvar jabber-process-buffer " *-jabber-process-*"
  "The name of the process buffer")

(defcustom jabber-use-sasl t
  "If non-nil, use SASL if possible.
SASL will still not be used if the library for it is missing or
if the server doesn't support it.

Disabling this shouldn't be necessary, but it may solve certain
problems."
  :type 'boolean
  :group 'jabber-core)

(defsubst jabber-have-sasl-p ()
  "Return non-nil if SASL functions are available."
  (featurep 'sasl))

(defvar jabber-account-history ()
  "Keeps track of previously used jabber accounts")

(defvar jabber-connection-type-history ()
  "Keeps track of previously used connection types")

;; jabber-connect and jabber-connect-all should load jabber.el, not
;; just jabber-core.el, when autoloaded.

;;;###autoload (autoload 'jabber-connect-all "jabber" "Connect to all configured Jabber accounts.\nSee `jabber-account-list'.\nIf no accounts are configured (or ARG supplied), call `jabber-connect' interactively." t)
(defun jabber-connect-all (&optional arg)
  "Connect to all configured Jabber accounts.
See `jabber-account-list'.
If no accounts are configured (or with prefix argument), call `jabber-connect' interactively.
With many prefix arguments, one less is passed to `jabber-connect'."
  (interactive "P")
  (let ((accounts
	 (remove-if (lambda (account)
		      (cdr (assq :disabled (cdr account))))
		    jabber-account-list)))
    (if (or (null accounts) arg)
	(let ((current-prefix-arg
	       (cond
		;; A number of C-u's; remove one, so to speak.
		((consp arg)
		 (if (> (car arg) 4)
		     (list (/ (car arg) 4))
		   nil))
		;; Otherwise, we just don't care.
		(t
		 arg))))
	  (call-interactively 'jabber-connect))
      ;; Only connect those accounts that are not yet connected.
      (let ((already-connected (mapcar #'jabber-connection-original-jid jabber-connections))
	    (connected-one nil))
	(dolist (account accounts)
	  (unless (member (jabber-jid-user (car account)) already-connected)
	    (let* ((jid (car account))
		   (alist (cdr account))
		   (password (cdr (assq :password alist)))
		   (network-server (cdr (assq :network-server alist)))
		   (port (cdr (assq :port alist)))
		   (connection-type (cdr (assq :connection-type alist))))
	      (jabber-connect
	       (jabber-jid-username jid)
	       (jabber-jid-server jid)
	       (jabber-jid-resource jid)
	       nil password network-server
	       port connection-type)
	      (setq connected-one t))))
	(unless connected-one
	  (message "All configured Jabber accounts are already connected"))))))

;;;###autoload (autoload 'jabber-connect "jabber" "Connect to the Jabber server and start a Jabber XML stream.\nWith prefix argument, register a new account.\nWith double prefix argument, specify more connection details." t)
(defun jabber-connect (username server resource &optional
				registerp password network-server
				port connection-type)
  "Connect to the Jabber server and start a Jabber XML stream.
With prefix argument, register a new account.
With double prefix argument, specify more connection details."
  (interactive
   (let* ((jid (completing-read "Enter your JID: " jabber-account-list nil nil nil 'jabber-account-history))
	  (entry (assoc jid jabber-account-list))
	  (alist (cdr entry))
	  password network-server port connection-type registerp)
     (flet ((nonempty
	     (s)
	     (unless (zerop (length s)) s)))
       (when entry
	 ;; If the user entered the JID of one of the preconfigured
	 ;; accounts, use that data.
	 (setq password (cdr (assq :password alist)))
	 (setq network-server (cdr (assq :network-server alist)))
	 (setq port (cdr (assq :port alist)))
	 (setq connection-type (cdr (assq :connection-type alist))))
       (when (equal current-prefix-arg '(16))
	 ;; Double prefix arg: ask about everything.
	 ;; (except password, which is asked about later anyway)
	 (setq password nil)
	 (setq network-server
	       (read-string (format "Network server: (default `%s') " network-server)
			    nil nil network-server))
	 (when (zerop (length network-server))
	   (setq network-server nil))
	 (setq port
	       (car
		(read-from-string
		 (read-string (format "Port: (default `%s') " port)
			      nil nil (if port (number-to-string port) "nil")))))
	 (setq connection-type
	       (car
		(read-from-string
		 (let ((default (symbol-name (or connection-type jabber-default-connection-type))))
		   (completing-read
		    (format "Connection type: (default `%s') " default)
		    (mapcar (lambda (type)
			      (cons (symbol-name (car type)) nil))
			    jabber-connect-methods)
		    nil t nil 'jabber-connection-type-history default)))))
	 (setq registerp (or jabber-silent-mode (yes-or-no-p "Register new account? "))))
       (when (equal current-prefix-arg '(4))
	 (setq registerp t))

       (list (jabber-jid-username jid)
	     (jabber-jid-server jid)
	     (jabber-jid-resource jid)
	     registerp password network-server port connection-type))))

  (require 'jabber)

  (if (member (list username
		    server)
	      (mapcar
	       (lambda (c)
		 (let ((data (fsm-get-state-data c)))
		   (list (plist-get data :username)
			 (plist-get data :server))))
	       jabber-connections))
      (message "Already connected to %s@%s"
	       username server)
    ;;(jabber-clear-roster)

    (push (start-jabber-connection username server resource
				   registerp password 
				   network-server port connection-type)
	  jabber-connections)))

(define-state-machine jabber-connection
  :start ((username server resource registerp password network-server port connection-type)
	  "Start a Jabber connection."
	  (let* ((connection-type
		  (or connection-type jabber-default-connection-type))
		 (send-function
		  (jabber-get-send-function connection-type)))

	    (list :connecting 
		  (list :send-function send-function
			;; Save the JID we originally connected with.
			:original-jid (concat username "@" server)
			:username username
			:server server
			:resource resource
			:password password
			:registerp registerp
			:connection-type connection-type
			:encrypted (eq connection-type 'ssl)
			:network-server network-server
			:port port)))))

(define-enter-state jabber-connection nil
  (fsm state-data)
  ;; `nil' is the error state.

  ;; Close the network connection.
  (let ((connection (plist-get state-data :connection)))
    (when (processp connection)
      (let ((process-buffer (process-buffer connection)))
	(delete-process connection)
	(when (and (bufferp process-buffer)
		   (not jabber-debug-keep-process-buffers))
	  (kill-buffer process-buffer)))))
  (setq state-data (plist-put state-data :connection nil))
  ;; Clear MUC data
  (jabber-muc-connection-closed (jabber-connection-bare-jid fsm))
  ;; Remove lost connections from the roster buffer.
  (jabber-display-roster)
  (let ((expected (plist-get state-data :disconnection-expected))
	(reason (plist-get state-data :disconnection-reason))
	(ever-session-established (plist-get state-data :ever-session-established)))
    (unless expected
      (run-hook-with-args 'jabber-lost-connection-hooks fsm)
      (message "%s@%s%s: connection lost: `%s'"
	       (plist-get state-data :username)
	       (plist-get state-data :server)
	       (if (plist-get state-data :resource)
		   (concat "/" (plist-get state-data :resource))
		 "")
	       reason))

    (if (and jabber-auto-reconnect (not expected) ever-session-established)
	;; Reconnect after a short delay?
	(list state-data jabber-reconnect-delay)
      ;; Else the connection is really dead.  Remove it from the list
      ;; of connections.
      (setq jabber-connections
	    (delq fsm jabber-connections))
      (when jabber-mode-line-mode
        (jabber-mode-line-presence-update))
      (jabber-display-roster)
      ;; And let the FSM sleep...
      (list state-data nil))))

(define-state jabber-connection nil
  (fsm state-data event callback)
  ;; In the `nil' state, the connection is dead.  We wait for a
  ;; :timeout message, meaning to reconnect, or :do-disconnect,
  ;; meaning to cancel reconnection.
  (case event
    (:timeout
     (list :connecting state-data))
    (:do-disconnect
     (setq jabber-connections
	    (delq fsm jabber-connections))
     (list nil state-data nil))))

(define-enter-state jabber-connection :connecting
  (fsm state-data)
  (let* ((connection-type (plist-get state-data :connection-type))
	 (connect-function (jabber-get-connect-function connection-type))
	 (server (plist-get state-data :server))
	 (network-server (plist-get state-data :network-server))
	 (port (plist-get state-data :port)))
    (funcall connect-function fsm server network-server port))
  (list state-data nil))

(define-state jabber-connection :connecting
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:connected
     (let ((connection (cadr event))
	   (registerp (plist-get state-data :registerp)))

       (setq state-data (plist-put state-data :connection connection))

       (when (processp connection)
	 ;; TLS connections leave data in the process buffer, which
	 ;; the XML parser will choke on.
	 (with-current-buffer (process-buffer connection)
	   (erase-buffer))

	 (set-process-filter connection (fsm-make-filter fsm))
	 (set-process-sentinel connection (fsm-make-sentinel fsm)))

       (list :connected state-data)))

    (:connection-failed
     (message "Jabber connection failed")
     (plist-put state-data :disconnection-reason
		(mapconcat #'identity (cadr event) "; "))
     (list nil state-data))

    (:do-disconnect
     ;; We don't have the connection object, so defer the disconnection.
     :defer)))

(defsubst jabber-fsm-handle-sentinel (state-data event)
  "Handle sentinel event for jabber fsm."
  ;; We do the same thing for every state, so avoid code duplication.
  (let* ((string (car (cddr event)))
	 ;; The event string sometimes (always?) has a trailing
	 ;; newline, that we don't care for.
	 (trimmed-string
	  (if (eq ?\n (aref string (1- (length string))))
	      (substring string 0 -1)
	    string))
	 (new-state-data
	  ;; If we already know the reason (e.g. a stream error), don't
	  ;; overwrite it.
	  (if (plist-get state-data :disconnection-reason)
	      state-data
	    (plist-put state-data :disconnection-reason trimmed-string))))
    (list nil new-state-data)))

(define-enter-state jabber-connection :connected
  (fsm state-data)

  (jabber-send-stream-header fsm)
  
  ;; Next thing happening is the server sending its own <stream:stream> start tag.
  
  (list state-data nil))

(define-state jabber-connection :connected
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :connected state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stream-start
     (let ((session-id (cadr event))
	   (stream-version (car (cddr event))))
       (setq state-data
	     (plist-put state-data :session-id session-id))
       ;; the stream feature is only sent if the initiating entity has
       ;; sent 1.0 in the stream header. if sasl is not supported then
       ;; we don't send 1.0 in the header and therefore we shouldn't wait
       ;; even if 1.0 is present in the receiving stream.
       (cond
	;; Wait for stream features?
	((and stream-version
	      (>= (string-to-number stream-version) 1.0)
	      jabber-use-sasl
	      (jabber-have-sasl-p))
	 ;; Stay in same state...
	 (list :connected state-data))
	;; Register account?
	((plist-get state-data :registerp)
	 ;; XXX: require encryption for registration?
	 (list :register-account state-data))
	;; Legacy authentication?
	(t
	 (list :legacy-auth state-data)))))

    (:stanza
     (let ((stanza (cadr event)))
       (cond
	;; At this stage, we only expect a stream:features stanza.
	((not (eq (jabber-xml-node-name stanza) 'features))
	 (list nil (plist-put state-data
			      :disconnection-reason
			      (format "Unexpected stanza %s" stanza))))
	((and (jabber-xml-get-children stanza 'starttls)
	      (eq (plist-get state-data :connection-type) 'starttls))
	 (list :starttls state-data))
	;; XXX: require encryption for registration?
	((plist-get state-data :registerp)
	 ;; We could check for the <register/> element in stream
	 ;; features, but as a client we would only lose by doing
	 ;; that.
	 (list :register-account state-data))
	(t
	 (list :sasl-auth (plist-put state-data :stream-features stanza))))))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))

(define-enter-state jabber-connection :starttls
  (fsm state-data)
  (jabber-starttls-initiate fsm)
  (list state-data nil))

(define-state jabber-connection :starttls
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :starttls state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (condition-case e
	 (progn
	   (jabber-starttls-process-input fsm (cadr event))
	   ;; Connection is encrypted.  Send a stream tag again.
	   (list :connected (plist-put state-data :encrypted t)))
       (error
	(let* ((msg (concat "STARTTLS negotiation failed: "
			    (error-message-string e)))
	       (new-state-data (plist-put state-data :disconnection-reason msg)))
	  (list nil new-state-data)))))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))

(define-enter-state jabber-connection :register-account
  (fsm state-data)
  (jabber-get-register fsm nil)
  (list state-data nil))

(define-state jabber-connection :register-account
  (fsm state-data event callback)
  ;; The connection will be closed in jabber-register
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :register-account state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (or
      (jabber-process-stream-error (cadr event) state-data)
      (progn
	(jabber-process-input fsm (cadr event))
	(list :register-account state-data))))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))

(define-enter-state jabber-connection :legacy-auth
  (fsm state-data)
  (jabber-get-auth fsm (plist-get state-data :server)
		   (plist-get state-data :session-id))
  (list state-data nil))

(define-state jabber-connection :legacy-auth
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :legacy-auth state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (or
      (jabber-process-stream-error (cadr event) state-data)
      (progn
	(jabber-process-input fsm (cadr event))
	(list :legacy-auth state-data))))

    (:authentication-success
     (jabber-cache-password (jabber-connection-bare-jid fsm) (cdr event))
     (list :session-established state-data))

    (:authentication-failure
     (jabber-uncache-password (jabber-connection-bare-jid fsm))
     ;; jabber-logon has already displayed a message
     (list nil (plist-put state-data
			  :disconnection-expected t)))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))

(define-enter-state jabber-connection :sasl-auth
  (fsm state-data)
  (let ((new-state-data
	 (plist-put state-data
		    :sasl-data
		    (jabber-sasl-start-auth 
		     fsm
		     (plist-get state-data
				:stream-features)))))
    (list new-state-data nil)))

(define-state jabber-connection :sasl-auth
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :sasl-auth state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (let ((new-sasl-data
	    (jabber-sasl-process-input 
	     fsm (cadr event) 
	     (plist-get state-data :sasl-data))))
       (list :sasl-auth (plist-put state-data :sasl-data new-sasl-data))))

    (:use-legacy-auth-instead
     (list :legacy-auth (plist-put state-data :sasl-data nil)))

    (:authentication-success
     (jabber-cache-password (jabber-connection-bare-jid fsm) (cdr event))
     (list :bind (plist-put state-data :sasl-data nil)))

    (:authentication-failure
     (jabber-uncache-password (jabber-connection-bare-jid fsm))
     ;; jabber-sasl has already displayed a message
     (list nil (plist-put state-data
			  :disconnection-expected t)))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))

(define-enter-state jabber-connection :bind
  (fsm state-data)
  (jabber-send-stream-header fsm)
  (list state-data nil))

(define-state jabber-connection :bind
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :bind state-data)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stream-start
     ;; we wait for stream features...
     (list :bind state-data))

    (:stanza
     (let ((stanza (cadr event)))
       (cond
	((eq (jabber-xml-node-name stanza) 'features)
	 ;; Record stream features, discarding earlier data:
	 (setq state-data (plist-put state-data :stream-features stanza))
	 (if (jabber-xml-get-children stanza 'bind)
	     (labels
		 ((handle-bind 
		   (jc xml-data success)
		   (fsm-send jc (list
				 (if success :bind-success :bind-failure)
				 xml-data))))
	       ;; So let's bind a resource.  We can either pick a resource ourselves,
	       ;; or have the server pick one for us.
	       (let ((resource (plist-get state-data :resource)))
		 (jabber-send-iq fsm nil "set"
				 `(bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind"))
					,@(when resource
					    `((resource () ,resource))))
				 #'handle-bind t
				 #'handle-bind nil))
	       (list :bind state-data))
	   (message "Server doesn't permit resource binding")
	   (list nil state-data)))
	(t
	 (or
	  (jabber-process-stream-error (cadr event) state-data)
	  (progn
	    (jabber-process-input fsm (cadr event))
	    (list :bind state-data)))))))

    (:bind-success
     (let ((jid (jabber-xml-path (cadr event) '(bind jid ""))))
       ;; Maybe this isn't the JID we asked for.
       (plist-put state-data :username (jabber-jid-username jid))
       (plist-put state-data :server (jabber-jid-server jid))
       (plist-put state-data :resource (jabber-jid-resource jid)))

     ;; If the server follows the older RFCs 3920 and 3921, it may
     ;; offer session initiation here.  If it follows RFCs 6120 and
     ;; 6121, it might not offer it, and we should just skip it.
     (if (jabber-xml-get-children (plist-get state-data :stream-features) 'session)
	 (labels
	     ((handle-session
	       (jc xml-data success)
	       (fsm-send jc (list
			     (if success :session-success :session-failure)
			     xml-data))))
	   (jabber-send-iq fsm nil "set"
			   '(session ((xmlns . "urn:ietf:params:xml:ns:xmpp-session")))
			   #'handle-session t
			   #'handle-session nil)
	   (list :bind state-data))
       ;; Session establishment not offered - assume not necessary.
       (list :session-established state-data)))

    (:session-success
     ;; We have a session
     (list :session-established state-data))

    (:bind-failure
     (message "Resource binding failed: %s" 
	      (jabber-parse-error
	       (jabber-iq-error (cadr event))))
     (list nil state-data))

    (:session-failure
     (message "Session establishing failed: %s"
	      (jabber-parse-error
	       (jabber-iq-error (cadr event))))
     (list nil state-data))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))

(define-enter-state jabber-connection :session-established
  (fsm state-data)
  (jabber-send-iq fsm nil
		  "get" 
		  '(query ((xmlns . "jabber:iq:roster")))
		  #'jabber-process-roster 'initial
		  #'jabber-initial-roster-failure nil)
  (list (plist-put state-data :ever-session-established t) nil))

(defvar jabber-pending-presence-timeout 0.5
  "Wait this long before doing presence packet batch processing.")

(define-state jabber-connection :session-established
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:filter
     (let ((process (cadr event))
	   (string (car (cddr event))))
       (jabber-pre-filter process string fsm)
       (list :session-established state-data :keep)))

    (:sentinel
     (jabber-fsm-handle-sentinel state-data event))

    (:stanza
     (or
      (jabber-process-stream-error (cadr event) state-data)
      (progn
	(jabber-process-input fsm (cadr event))
	(list :session-established state-data :keep))))

    (:roster-update
     ;; Batch up roster updates
     (let* ((jid-symbol-to-update (cdr event))
	    (pending-updates (plist-get state-data :roster-pending-updates)))
       ;; If there are pending updates, there is a timer running
       ;; already; just add the new symbol and wait.
       (if pending-updates
	   (progn
	     (unless (memq jid-symbol-to-update pending-updates)
	       (nconc pending-updates (list jid-symbol-to-update)))
	     (list :session-established state-data :keep))
	 ;; Otherwise, we need to create the list and start the timer.
	 (setq state-data 
	       (plist-put state-data
			  :roster-pending-updates 
			  (list jid-symbol-to-update)))
	 (list :session-established state-data jabber-pending-presence-timeout))))

    (:timeout
     ;; Update roster
     (let ((pending-updates (plist-get state-data :roster-pending-updates)))
       (setq state-data (plist-put state-data :roster-pending-updates nil))
       (jabber-roster-update fsm nil pending-updates nil)
       (list :session-established state-data)))

    (:send-if-connected
     ;; This is the only state in which we respond to such messages.
     ;; This is to make sure we don't send anything inappropriate
     ;; during authentication etc.
     (jabber-send-sexp fsm (cdr event))
     (list :session-established state-data :keep))

    (:do-disconnect
     (jabber-send-string fsm "</stream:stream>")
     (list nil (plist-put state-data
			  :disconnection-expected t)))))

(defun jabber-disconnect (&optional arg)
  "Disconnect from all Jabber servers. If ARG supplied, disconnect one account."
  (interactive "P")
  (if arg
      (jabber-disconnect-one (jabber-read-account))
    (unless *jabber-disconnecting*	; avoid reentry
      (let ((*jabber-disconnecting* t))
	(if (null jabber-connections)
	    (message "Already disconnected")
	  (run-hooks 'jabber-pre-disconnect-hook)
	  (dolist (c jabber-connections)
	    (jabber-disconnect-one c t))
	  (setq jabber-connections nil)

	  (jabber-disconnected)
	  (when (interactive-p)
	    (message "Disconnected from Jabber server(s)")))))))

(defun jabber-disconnect-one (jc &optional dont-redisplay)
  "Disconnect from one Jabber server.
If DONT-REDISPLAY is non-nil, don't update roster buffer."
  (interactive (list (jabber-read-account)))
  (fsm-send-sync jc :do-disconnect)
  (when (interactive-p)
    (message "Disconnected from %s"
	     (jabber-connection-jid jc)))
  (unless dont-redisplay
    (jabber-display-roster)))

(defun jabber-disconnected ()
  "Re-initialise jabber package variables.
Call this function after disconnection."
  (when (get-buffer jabber-roster-buffer)
    (with-current-buffer (get-buffer jabber-roster-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer))))

  (jabber-clear-roster)
  (run-hooks 'jabber-post-disconnect-hook))

(defun jabber-log-xml (fsm direction data)
  "Print DATA to XML console (and, optionally, in file).
If `jabber-debug-log-xml' is nil, do nothing.
FSM is the connection that is sending/receiving.
DIRECTION is a string, either \"sending\" or \"receive\".
DATA is any sexp."
  (when jabber-debug-log-xml
      (jabber-process-console fsm direction data)))

(defun jabber-pre-filter (process string fsm)
  (with-current-buffer (process-buffer process)
    ;; Append new data
    (goto-char (point-max))
    (insert string)

    (unless (boundp 'jabber-filtering)
      (let (jabber-filtering)
	(jabber-filter process fsm)))))

(defun jabber-filter (process fsm)
  "the filter function for the jabber process"
  (with-current-buffer (process-buffer process)
    ;; Start from the beginning
    (goto-char (point-min))
    (let (xml-data)
      (loop 
       do
       ;; Skip whitespace
       (unless (zerop (skip-chars-forward " \t\r\n"))
	 (delete-region (point-min) (point)))
       ;; Skip processing directive
       (when (looking-at "<\\?xml[^?]*\\?>")
	 (delete-region (match-beginning 0) (match-end 0)))

       ;; Stream end?
       (when (looking-at "</stream:stream>")
	 (return (fsm-send fsm :stream-end)))

       ;; Stream header?
       (when (looking-at "<stream:stream[^>]*\\(>\\)")
	 ;; Let's pretend that the stream header is a closed tag,
	 ;; and parse it as such.
	 (replace-match "/>" t t nil 1)
	 (let* ((ending-at (point))
		(stream-header (car (xml-parse-region (point-min) ending-at)))
		(session-id (jabber-xml-get-attribute stream-header 'id))
		(stream-version (jabber-xml-get-attribute stream-header 'version)))

	   ;; Need to keep any namespace attributes on the stream
	   ;; header, as they can affect any stanza in the
	   ;; stream...
	   (setq jabber-namespace-prefixes
		 (jabber-xml-merge-namespace-declarations
		  (jabber-xml-node-attributes stream-header)
		  nil))
	   (jabber-log-xml fsm "receive" stream-header)
	   (fsm-send fsm (list :stream-start session-id stream-version))
	   (delete-region (point-min) ending-at)))
       
       ;; Normal tag

       ;; XXX: do these checks make sense?  If so, reinstate them.
       ;;(if (active-minibuffer-window)
       ;;    (run-with-idle-timer 0.01 nil #'jabber-filter process string)

       ;; This check is needed for xml.el of Emacs 21, as it chokes on
       ;; empty attribute values.
       (save-excursion
	 (while (search-forward-regexp " \\w+=''" nil t)
           (replace-match "")))
       
       (setq xml-data (and (catch 'unfinished
			     (jabber-xml-skip-tag-forward)
			     (> (point) (point-min)))
			   (xml-parse-region (point-min) (point))))

       while xml-data
       do
       ;; If there's a problem with writing the XML log,
       ;; make sure the stanza is delivered, at least.
       (condition-case e
	   (jabber-log-xml fsm "receive" (car xml-data))
	 (error
	  (ding)
	  (message "Couldn't write XML log: %s" (error-message-string e))
	  (sit-for 2)))
       (delete-region (point-min) (point))

       (fsm-send fsm (list :stanza
			   (jabber-xml-resolve-namespace-prefixes
			    (car xml-data) nil jabber-namespace-prefixes)))
       ;; XXX: move this logic elsewhere
       ;; We explicitly don't catch errors in jabber-process-input,
       ;; to facilitate debugging.
       ;; (jabber-process-input (car xml-data))
       ))))

(defun jabber-process-input (jc xml-data)
  "process an incoming parsed tag"
  (let* ((tag (jabber-xml-node-name xml-data))
	 (functions (eval (cdr (assq tag '((iq . jabber-iq-chain)
					   (presence . jabber-presence-chain)
					   (message . jabber-message-chain)))))))
    (dolist (f functions)
      (condition-case e
	  (funcall f jc xml-data)
	((debug error)
	 (fsm-debug-output "Error %S while processing %S with function %s" e xml-data f))))))

(defun jabber-process-stream-error (xml-data state-data)
  "Process an incoming stream error.
Return nil if XML-DATA is not a stream:error stanza.
Return an fsm result list if it is."
  (when (and (eq (jabber-xml-node-name xml-data) 'error)
	     (equal (jabber-xml-get-xmlns xml-data) "http://etherx.jabber.org/streams"))
    (let ((condition (jabber-stream-error-condition xml-data))
	  (text (jabber-parse-stream-error xml-data)))
      (setq state-data (plist-put state-data :disconnection-reason 
				  (format "Stream error: %s" text)))
      ;; Special case: when the error is `conflict', we have been
      ;; forcibly disconnected by the same user.  Don't reconnect
      ;; automatically.
      (when (eq condition 'conflict)
	(setq state-data (plist-put state-data :disconnection-expected t)))
      (list nil state-data))))

;; XXX: This function should probably die.  The roster is stored
;; inside the connection plists, and the obarray shouldn't be so big
;; that we need to clean it.
(defun jabber-clear-roster ()
  "Clean up the roster."
  ;; This is made complicated by the fact that the JIDs are symbols with properties.
  (mapatoms #'(lambda (x)
		(unintern x jabber-jid-obarray))
	    jabber-jid-obarray)
  (setq *jabber-roster* nil))

(defun jabber-send-sexp (jc sexp)
  "Send the xml corresponding to SEXP to connection JC."
  (condition-case e
      (jabber-log-xml jc "sending" sexp)
    (error
     (ding)
     (message "Couldn't write XML log: %s" (error-message-string e))
     (sit-for 2)))
  (jabber-send-string jc (jabber-sexp2xml sexp)))

(defun jabber-send-sexp-if-connected (jc sexp)
  "Send the stanza SEXP only if JC has established a session."
  (fsm-send-sync jc (cons :send-if-connected sexp)))

(defun jabber-send-stream-header (jc)
  "Send stream header to connection JC."
  (let ((stream-header
	 (concat "<?xml version='1.0'?><stream:stream to='" 
		 (plist-get (fsm-get-state-data jc) :server)
		 "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'"
		 ;; Not supporting SASL is not XMPP compliant,
		 ;; so don't pretend we are.
		 (if (and (jabber-have-sasl-p) jabber-use-sasl)
		     " version='1.0'"
		   "")
		 ">
")))
    (jabber-log-xml jc "sending" stream-header)
    (jabber-send-string jc stream-header)))

(defun jabber-send-string (jc string)
  "Send STRING to the connection JC."
  (let* ((state-data (fsm-get-state-data jc))
	 (connection (plist-get state-data :connection))
	 (send-function (plist-get state-data :send-function)))
    (unless connection
      (error "%s has no connection" (jabber-connection-jid jc)))
    (funcall send-function connection string)))

(provide 'jabber-core)

;;; arch-tag: 9d273ce6-c45a-447b-abf3-21d3ce73a51a
