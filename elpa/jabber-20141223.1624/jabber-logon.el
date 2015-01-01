;; jabber-logon.el - logon functions

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

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

(require 'jabber-xml)
(require 'jabber-util)
;; In Emacs 24, sha1 is built in, so this require is only needed for
;; earlier versions.  It's supposed to be a noop in Emacs 24, but
;; sometimes, for some people, it isn't, and fails with
;; (file-error "Cannot open load file" "sha1").
(unless (fboundp 'sha1)
  (require 'sha1))

(defun jabber-get-auth (jc to session-id)
  "Send IQ get request in namespace \"jabber:iq:auth\"."
  (jabber-send-iq jc to
		  "get"
		  `(query ((xmlns . "jabber:iq:auth"))
			  (username () ,(plist-get (fsm-get-state-data jc) :username)))
		  #'jabber-do-logon session-id
		  #'jabber-report-success "Impossible error - auth field request"))

(defun jabber-do-logon (jc xml-data session-id)
  "send username and password in logon attempt"
  (let* ((digest-allowed (jabber-xml-get-children (jabber-iq-query xml-data) 'digest))
	 (passwd (when
		     (or digest-allowed
			 (plist-get (fsm-get-state-data jc) :encrypted)
			 (yes-or-no-p "Jabber server only allows cleartext password transmission!  Continue? "))
		   (or (plist-get (fsm-get-state-data jc) :password)
		       (jabber-read-password (jabber-connection-bare-jid jc)))))
	 auth)
    (if (null passwd)
	(fsm-send jc :authentication-failure)
      (if digest-allowed
	  (setq auth `(digest () ,(sha1 (concat session-id passwd))))
	(setq auth `(password () ,passwd)))

      ;; For legacy authentication we must specify a resource.
      (unless (plist-get (fsm-get-state-data jc) :resource)
	;; Yes, this is ugly.  Where is my encapsulation?
	(plist-put (fsm-get-state-data jc) :resource "emacs-jabber"))

      (jabber-send-iq jc (plist-get (fsm-get-state-data jc) :server)
		      "set"
		      `(query ((xmlns . "jabber:iq:auth"))
			      (username () ,(plist-get (fsm-get-state-data jc) :username))
			      ,auth
			      (resource () ,(plist-get (fsm-get-state-data jc) :resource)))
		      #'jabber-process-logon passwd
		      #'jabber-process-logon nil))))

(defun jabber-process-logon (jc xml-data closure-data)
  "receive login success or failure, and request roster.
CLOSURE-DATA should be the password on success and nil on failure."
  (if closure-data
      ;; Logon success
      (fsm-send jc (cons :authentication-success closure-data))

    ;; Logon failure
    (jabber-report-success jc xml-data "Logon")
    (fsm-send jc :authentication-failure)))

(provide 'jabber-logon)

;;; arch-tag: f24ebe5e-3420-44bb-af81-d4de21f378b0
