;; jabber-conn.el - Network transport functions

;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni
;; mostly inspired by Gnus.

;; Copyright (C) 2005 - Carl Henrik Lunde - chlunde+jabber+@ping.uio.no
;; (starttls)

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

;; A collection of functions, that hide the details of transmitting to
;; and fro a Jabber Server

(eval-when-compile (require 'cl))

;; Emacs 24 can be linked with GnuTLS
(ignore-errors (require 'gnutls))

;; Try two different TLS/SSL libraries, but don't fail if none available.
(or (ignore-errors (require 'tls))
    (ignore-errors (require 'ssl)))

(ignore-errors (require 'starttls))

(require 'srv)

(defgroup jabber-conn nil "Jabber Connection Settings"
  :group 'jabber)

(defun jabber-have-starttls ()
  "Return true if we can use STARTTLS."
  (or (and (fboundp 'gnutls-available-p)
	   (gnutls-available-p))
      (and (featurep 'starttls)
	   (or (and (bound-and-true-p starttls-gnutls-program)
		    (executable-find starttls-gnutls-program))
	       (and (bound-and-true-p starttls-program)
		    (executable-find starttls-program))))))

(defconst jabber-default-connection-type 
  (cond
   ;; Use STARTTLS if we can...
   ((jabber-have-starttls)
    'starttls)
   ;; ...else default to unencrypted connection.
   (t
    'network))
  "Default connection type.
See `jabber-connect-methods'.")

(defcustom jabber-connection-ssl-program nil
  "Program used for SSL/TLS connections.
nil means prefer gnutls but fall back to openssl.
'gnutls' means use gnutls (through `open-tls-stream').
'openssl means use openssl (through `open-ssl-stream')."
  :type '(choice (const :tag "Prefer gnutls, fall back to openssl" nil)
		 (const :tag "Use gnutls" gnutls)
		 (const :tag "Use openssl" openssl))
  :group 'jabber-conn)

(defcustom jabber-invalid-certificate-servers ()
  "Jabber servers for which we accept invalid TLS certificates.
This is a list of server names, each matching the hostname part
of your JID.

This option has effect only when using native GnuTLS in Emacs 24
or later."
  :type '(repeat string)
  :group 'jabber-conn)

(defvar jabber-connect-methods
  `((network jabber-network-connect jabber-network-send)
    (starttls
     ,(if (and (fboundp 'gnutls-available-p)
	       (gnutls-available-p))
	  ;; With "native" TLS, we can use a normal connection.
	  'jabber-network-connect
	'jabber-starttls-connect)
     jabber-network-send)
    (ssl jabber-ssl-connect jabber-ssl-send)
    (virtual jabber-virtual-connect jabber-virtual-send))
  "Alist of connection methods and functions.
First item is the symbol naming the method.
Second item is the connect function.
Third item is the send function.")

(defun jabber-get-connect-function (type)
  "Get the connect function associated with TYPE.
TYPE is a symbol; see `jabber-connection-type'."
  (let ((entry (assq type jabber-connect-methods)))
    (nth 1 entry)))

(defun jabber-get-send-function (type)
  "Get the send function associated with TYPE.
TYPE is a symbol; see `jabber-connection-type'."
  (let ((entry (assq type jabber-connect-methods)))
    (nth 2 entry)))

(defun jabber-srv-targets (server network-server port)
  "Find host and port to connect to.
If NETWORK-SERVER and/or PORT are specified, use them.
If we can't find SRV records, use standard defaults."
  ;; If the user has specified a host or a port, obey that.
  (if (or network-server port)
      (list (cons (or network-server server)
		  (or port 5222)))
    (or (condition-case nil
	    (srv-lookup (concat "_xmpp-client._tcp." server))
	  (error nil))
	(list (cons server 5222)))))

;; Plain TCP/IP connection
(defun jabber-network-connect (fsm server network-server port)
  "Connect to a Jabber server with a plain network connection.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message (:connection-failed ERRORS) if
connection fails."
  (cond
   ((featurep 'make-network-process '(:nowait t))
    ;; We can connect asynchronously!
    (jabber-network-connect-async fsm server network-server port))
   (t
    ;; Connecting to the server will block Emacs.
    (jabber-network-connect-sync fsm server network-server port))))

(defun jabber-network-connect-async (fsm server network-server port)
  ;; Get all potential targets...
  (lexical-let ((targets (jabber-srv-targets server network-server port))
		errors
		(fsm fsm))
    ;; ...and connect to them one after another, asynchronously, until
    ;; connection succeeds.
    (labels
	((connect
	  (target remaining-targets)
	  (lexical-let ((target target) (remaining-targets remaining-targets))
	    (labels ((connection-successful
		      (c)
		      ;; This mustn't be `fsm-send-sync', because the FSM
		      ;; needs to change the sentinel, which cannot be done
		      ;; from inside the sentinel.
		      (fsm-send fsm (list :connected c)))
		     (connection-failed
		      (c status)
		      (when (and (> (length status) 0)
				 (eq (aref status (1- (length status))) ?\n))
			(setq status (substring status 0 -1)))
		      (let ((err
			     (format "Couldn't connect to %s:%s: %s"
				     (car target) (cdr target) status)))
			(message "%s" err)
			(push err errors))
		      (when c (delete-process c))
		      (if remaining-targets
			  (progn
			    (message
			     "Connecting to %s:%s..."
			     (caar remaining-targets) (cdar remaining-targets))
			    (connect (car remaining-targets) (cdr remaining-targets)))
			(fsm-send fsm (list :connection-failed (nreverse errors))))))
	      (condition-case e
		  (make-network-process
		   :name "jabber"
		   :buffer (generate-new-buffer jabber-process-buffer)
		   :host (car target) :service (cdr target)
		   :coding 'utf-8
		   :nowait t
		   :sentinel
		   (lexical-let ((target target) (remaining-targets remaining-targets))
		     (lambda (connection status)
		       (cond
			((string-match "^open" status)
			 (connection-successful connection))
			((string-match "^failed" status)
			 (connection-failed connection status))
			((string-match "^deleted" status)
			 ;; This happens when we delete a process in the
			 ;; "failed" case above.
			 nil)
			(t
			 (message "Unknown sentinel status `%s'" status))))))
		(file-error
		 ;; A file-error has the error message in the third list
		 ;; element.
		 (connection-failed nil (car (cddr e))))
		(error
		 ;; Not sure if we ever get anything but file-errors,
		 ;; but let's make sure we report them:
		 (connection-failed nil (error-message-string e))))))))
      (message "Connecting to %s:%s..." (caar targets) (cdar targets))
      (connect (car targets) (cdr targets)))))

(defun jabber-network-connect-sync (fsm server network-server port)
  ;; This code will AFAIK only be used on Windows.  Apologies in
  ;; advance for any bit rot...
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(targets (jabber-srv-targets server network-server port))
	errors)
    (catch 'connected
      (dolist (target targets)
	(condition-case e
	    (let ((process-buffer (generate-new-buffer jabber-process-buffer))
		  connection)
	      (unwind-protect
		  (setq connection (open-network-stream
				    "jabber"
				    process-buffer
				    (car target)
				    (cdr target)))

		(unless (or connection jabber-debug-keep-process-buffers)
		  (kill-buffer process-buffer)))
	
	      (when connection
		(fsm-send fsm (list :connected connection))
		(throw 'connected connection)))
	  (file-error
	   ;; A file-error has the error message in the third list
	   ;; element.
	   (let ((err (format "Couldn't connect to %s:%s: %s"
			      (car target) (cdr target)
			      (car (cddr e)))))
	     (message "%s" err)
	     (push err errors)))
	  (error
	   ;; Not sure if we ever get anything but file-errors,
	   ;; but let's make sure we report them:
	   (let ((err (format "Couldn't connect to %s:%s: %s"
			      (car target) (cdr target)
			      (error-message-string e))))
	     (message "%s" err)
	     (push err errors)))))
      (fsm-send fsm (list :connection-failed (nreverse errors))))))

(defun jabber-network-send (connection string)
  "Send a string via a plain TCP/IP connection to the Jabber Server."
  (process-send-string connection string))

;; SSL connection, we use openssl's s_client function for encryption
;; of the link
;; TODO: make this configurable
(defun jabber-ssl-connect (fsm server network-server port)
  "connect via OpenSSL or GnuTLS to a Jabber Server
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message (:connection-failed ERRORS) if
connection fails."
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(connect-function
	 (cond
	  ((and (memq jabber-connection-ssl-program '(nil gnutls))
		(fboundp 'open-tls-stream))
	   'open-tls-stream)
	  ((and (memq jabber-connection-ssl-program '(nil openssl))
		(fboundp 'open-ssl-stream))
	   'open-ssl-stream)
	  (t
	   (error "Neither TLS nor SSL connect functions available"))))
	error-msg)
    (let ((process-buffer (generate-new-buffer jabber-process-buffer))
	  connection)
      (setq network-server (or network-server server))
      (setq port (or port 5223))
      (condition-case e
	  (setq connection (funcall connect-function
				    "jabber"
				    process-buffer
				    network-server
				    port))
	(error
	 (setq error-msg
	       (format "Couldn't connect to %s:%d: %s" network-server port
		       (error-message-string e)))
	 (message "%s" error-msg)))
      (unless (or connection jabber-debug-keep-process-buffers)
	(kill-buffer process-buffer))
      (if connection
	  (fsm-send fsm (list :connected connection))
	(fsm-send fsm (list :connection-failed
			    (when error-msg (list error-msg))))))))

(defun jabber-ssl-send (connection string)
  "Send a string via an SSL-encrypted connection to the Jabber Server."
  ;; It seems we need to send a linefeed afterwards.
  (process-send-string connection string)
  (process-send-string connection "\n"))

(defun jabber-starttls-connect (fsm server network-server port)
  "Connect via an external GnuTLS process to a Jabber Server.
Send a message of the form (:connected CONNECTION) to FSM if
connection succeeds.  Send a message (:connection-failed ERRORS) if
connection fails."
  (let ((coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	(targets (jabber-srv-targets server network-server port))
	errors)
    (unless (fboundp 'starttls-open-stream)
      (error "starttls.el not available"))
    (catch 'connected
      (dolist (target targets)
	(condition-case e
	    (let ((process-buffer (generate-new-buffer jabber-process-buffer))
		  connection)
	      (unwind-protect
		  (setq connection
			(starttls-open-stream
			 "jabber"
			 process-buffer
			 (car target)
			 (cdr target)))
		(unless (or connection jabber-debug-keep-process-buffers)
		  (kill-buffer process-buffer)))
	      (if (null connection)
		  ;; It seems we don't actually get an error if we
		  ;; can't connect.  Let's try to convey some useful
		  ;; information to the user at least.
		  (let ((err (format "Couldn't connect to %s:%s"
				     (car target) (cdr target))))
		    (message "%s" err)
		    (push err errors))
		(fsm-send fsm (list :connected connection))
		(throw 'connected connection)))
	  (error
	   (let ((err (format "Couldn't connect to %s: %s" target
			      (error-message-string e))))
	     (message "%s" err)
	     (push err errors)))))
	(fsm-send fsm (list :connection-failed (nreverse errors))))))

(defun jabber-starttls-initiate (fsm)
  "Initiate a starttls connection"
  (jabber-send-sexp fsm
   '(starttls ((xmlns . "urn:ietf:params:xml:ns:xmpp-tls")))))

(defun jabber-starttls-process-input (fsm xml-data)
  "Process result of starttls request.
On failure, signal error."
  (cond
   ((eq (car xml-data) 'proceed)
    (let* ((state-data (fsm-get-state-data fsm))
	   (connection (plist-get state-data :connection)))
      ;; Did we use open-network-stream or starttls-open-stream?  We
      ;; can tell by process-type.
      (case (process-type connection)
	(network
	 (let* ((hostname (plist-get state-data :server))
		(verifyp (not (member hostname jabber-invalid-certificate-servers))))
	   ;; gnutls-negotiate might signal an error, which is caught
	   ;; by our caller
	   (gnutls-negotiate
	    :process connection
	    ;; This is the hostname that the certificate should be valid for:
	    :hostname hostname
	    :verify-hostname-error verifyp
	    :verify-error verifyp)))
	(real
	 (or
	  (starttls-negotiate connection)
	  (error "Negotiation failure"))))))
   ((eq (car xml-data) 'failure)
    (error "Command rejected by server"))))

(defvar *jabber-virtual-server-function* nil
  "Function to use for sending stanzas on a virtual connection.
The function should accept two arguments, the connection object
and a string that the connection wants to send.")

(defun jabber-virtual-connect (fsm server network-server port)
  "Connect to a virtual \"server\".
Use `*jabber-virtual-server-function*' as send function."
  (unless (functionp *jabber-virtual-server-function*)
    (error "No virtual server function specified"))
  ;; We pass the fsm itself as "connection object", as that is what a
  ;; virtual server needs to send stanzas.
  (fsm-send fsm (list :connected fsm)))

(defun jabber-virtual-send (connection string)
  (funcall *jabber-virtual-server-function* connection string))

(provide 'jabber-conn)
;; arch-tag: f95ec240-8cd3-11d9-9dbf-000a95c2fcd0
