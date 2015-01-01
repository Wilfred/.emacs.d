;; jabber-sasl.el - SASL authentication

;; Copyright (C) 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu

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

;;; This file uses sasl.el from FLIM or Gnus.  If it can't be found,
;;; jabber-core.el won't use the SASL functions.
(eval-and-compile
  (condition-case nil
      (require 'sasl)
    (error nil)))

;;; Alternatives to FLIM would be the command line utility of GNU SASL,
;;; or anything the Gnus people decide to use.

;;; See XMPP-CORE and XMPP-IM for details about the protocol.

(require 'jabber-xml)

(defun jabber-sasl-start-auth (jc stream-features)
  ;; Find a suitable common mechanism.
  (let* ((mechanism-elements (car (jabber-xml-get-children stream-features 'mechanisms)))
	 (mechanisms (mapcar
		      (lambda (tag)
			(car (jabber-xml-node-children tag)))
		      (jabber-xml-get-children mechanism-elements 'mechanism)))
	 (mechanism
	  (if (and (member "ANONYMOUS" mechanisms)
		   (or jabber-silent-mode (yes-or-no-p "Use anonymous authentication? ")))
	      (sasl-find-mechanism '("ANONYMOUS"))
	    (sasl-find-mechanism mechanisms))))

    ;; No suitable mechanism?
    (if (null mechanism)
	;; Maybe we can use legacy authentication
	(let ((iq-auth (find "http://jabber.org/features/iq-auth"
			  (jabber-xml-get-children stream-features 'auth)
			  :key #'jabber-xml-get-xmlns
			  :test #'string=))
	      ;; Or maybe we have to use STARTTLS, but can't
	      (starttls (find "urn:ietf:params:xml:ns:xmpp-tls"
			      (jabber-xml-get-children stream-features 'starttls)
			      :key #'jabber-xml-get-xmlns
			      :test #'string=)))
	  (cond
	   (iq-auth
	    (fsm-send jc :use-legacy-auth-instead))
	   (starttls
	    (message "STARTTLS encryption required, but disabled/non-functional at our end")
	    (fsm-send jc :authentication-failure))
	   (t
	    (message "Authentication failure: no suitable SASL mechanism found")
	    (fsm-send jc :authentication-failure))))

      ;; Watch for plaintext logins over unencrypted connections
      (if (and (not (plist-get (fsm-get-state-data jc) :encrypted))
	       (member (sasl-mechanism-name mechanism)
		       '("PLAIN" "LOGIN"))
	       (not (yes-or-no-p "Jabber server only allows cleartext password transmission!  Continue? ")))
	  (fsm-send jc :authentication-failure)

	;; Start authentication.
	(let* (passphrase
	       (client (sasl-make-client mechanism 
					 (plist-get (fsm-get-state-data jc) :username)
					 "xmpp"
					 (plist-get (fsm-get-state-data jc) :server)))
	       (sasl-read-passphrase (jabber-sasl-read-passphrase-closure
				      jc
				      (lambda (p) (setq passphrase (copy-sequence p)) p)))
	       (step (sasl-next-step client nil)))
	  (jabber-send-sexp
	   jc
	   `(auth ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl")
		   (mechanism . ,(sasl-mechanism-name mechanism)))
		  ,(when (sasl-step-data step)
		     (base64-encode-string (sasl-step-data step) t))))
	  (list client step passphrase))))))

(defun jabber-sasl-read-passphrase-closure (jc remember)
  "Return a lambda function suitable for `sasl-read-passphrase' for JC.
Call REMEMBER with the password.  REMEMBER is expected to return it as well."
  (lexical-let ((password (plist-get (fsm-get-state-data jc) :password))
		(bare-jid (jabber-connection-bare-jid jc))
		(remember remember))
    (if password
	(lambda (prompt) (funcall remember (copy-sequence password)))
      (lambda (prompt) (funcall remember (jabber-read-password bare-jid))))))

(defun jabber-sasl-process-input (jc xml-data sasl-data)
  (let* ((client (first sasl-data))
	 (step (second sasl-data))
	 (passphrase (third sasl-data))
	 (sasl-read-passphrase (jabber-sasl-read-passphrase-closure 
				jc
				(lambda (p) (setq passphrase (copy-sequence p)) p))))
    (cond
     ((eq (car xml-data) 'challenge)
      (sasl-step-set-data step (base64-decode-string (car (jabber-xml-node-children xml-data))))
      (setq step (sasl-next-step client step))
      (jabber-send-sexp
       jc
       `(response ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl"))
		  ,(when (sasl-step-data step)
		     (base64-encode-string (sasl-step-data step) t)))))

     ((eq (car xml-data) 'failure)
      (message "%s: authentication failure: %s"
	       (jabber-connection-bare-jid jc)
	       (jabber-xml-node-name (car (jabber-xml-node-children xml-data))))
      (fsm-send jc :authentication-failure))

     ((eq (car xml-data) 'success)
      ;; The server might, depending on the mechanism, send
      ;; "additional data" (see RFC 4422) with the <success/> element.
      ;; Since some SASL mechanisms perform mutual authentication, we
      ;; need to pass this data to sasl.el - we're not necessarily
      ;; done just because the server says we're done.
      (let* ((data (car (jabber-xml-node-children xml-data)))
	     (decoded (if data
			  (base64-decode-string data)
			"")))
	(sasl-step-set-data step decoded)
	(condition-case e
	    (progn
	      ;; Check that sasl-next-step doesn't signal an error.
	      ;; TODO: once sasl.el allows it, check that all steps have
	      ;; been completed.
	      (sasl-next-step client step)
	      (message "Authentication succeeded for %s" (jabber-connection-bare-jid jc))
	      (fsm-send jc (cons :authentication-success passphrase)))
	  (sasl-error
	   (message "%s: authentication failure: %s"
		    (jabber-connection-bare-jid jc)
		    (error-message-string e))
	   (fsm-send jc :authentication-failure))))))
    (list client step passphrase)))

(provide 'jabber-sasl)
;;; arch-tag: 2a4a234d-34d3-49dd-950d-518c899c0fd0
