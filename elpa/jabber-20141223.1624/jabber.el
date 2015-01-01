;; jabber.el - a minimal jabber client

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

;; SSL - Support, mostly inspired by Gnus
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

;;; load Unicode tables if this needed
(when (and (featurep 'xemacs) (not (emacs-version>= 21 5 5)))
    (require 'un-define))

;;; these customize fields should come first
(defgroup jabber nil "Jabber instant messaging"
  :group 'applications)

;;;###autoload
(defcustom jabber-account-list nil
  "List of Jabber accounts.
Each element of the list is a cons cell describing a Jabber account,
where the car is a JID and the CDR is an alist.

JID is a full Jabber ID string (e.g. foo@bar.tld). You can also
specify the resource (e.g. foo@bar.tld/emacs).
The following keys can be present in the alist:
:password is a string to authenticate ourself against the server.
It can be empty.
:network-server is a string identifying the address to connect to,
if it's different from the server part of the JID.
:port is the port to use (default depends on connection type).
:connection-type is a symbol. Valid symbols are `starttls',
`network' and `ssl'.

Only JID is mandatory.  The rest can be guessed at run-time.

Examples:

Two accounts without any special configuration:
\((\"foo@example.com\") (\"bar@example.net\"))

One disabled account with a non-standard port:
\((\"romeo@montague.net\" (:port . 5242) (:disabled . t)))

If you don't have SRV and STARTTLS capabilities in your Emacs,
configure a Google Talk account like this:
\((\"username@gmail.com\" 
  (:network-server . \"talk.google.com\")
  (:connection-type . ssl)))"
  :type '(repeat
	  (cons :tag "Account information"
		(string :tag "JID")
		(set :format "%v"
		     (cons :format "%v"
			   (const :format "" :disabled)
			   (const :tag "Disabled" t))
		     (cons :format "%v"
			   (const :format "" :password)
			   (string :tag "Password"))
		     (cons :format "%v"
			   (const :format "" :network-server)
			   (string :tag "Network server"))
		     (cons :format "%v"
			   (const :format "" :port)
			   (integer :tag "Port" 5222))
		     (cons :format "%v"
			   (const :format "" :connection-type)
			   (choice :tag "Connection type"
				   ;; XXX: detect whether we have STARTTLS?  option
				   ;; for enforcing encryption?
				   (const :tag "STARTTLS" starttls)
				   (const :tag "Unencrypted" network)
				   (const :tag "Legacy SSL/TLS" ssl))))))
  :group 'jabber)

(defcustom jabber-default-show ""
  "default show state"
  :type '(choice (const :tag "Online" "")
		 (const :tag "Chatty" "chat")
		 (const :tag "Away" "away")
		 (const :tag "Extended away" "xa")
		 (const :tag "Do not disturb" "dnd"))
  :group 'jabber)

(defcustom jabber-default-status ""
  "default status string"
  :type 'string
  :group 'jabber)

(defcustom jabber-default-priority 10
  "default priority"
  :type 'integer
  :group 'jabber)

;;; guess internal dependencies!
(require 'jabber-util)
(require 'jabber-menu)
(require 'jabber-xml)
(require 'jabber-conn)
(require 'jabber-core)
(require 'jabber-logon)
(require 'jabber-roster)
(require 'jabber-presence)
(require 'jabber-alert)
(require 'jabber-chat)
(require 'jabber-disco)
(require 'jabber-iq)
(require 'jabber-widget)
(require 'jabber-register)
(require 'jabber-search)
(require 'jabber-browse)
(require 'jabber-muc)
(require 'jabber-muc-nick-completion)
(require 'jabber-version)
(require 'jabber-ahc-presence)
(require 'jabber-modeline)
(require 'jabber-watch)
(require 'jabber-activity)
(require 'jabber-vcard)
(require 'jabber-events)
(require 'jabber-chatstates)
(require 'jabber-vcard-avatars)
(require 'jabber-autoaway)
(require 'jabber-time)
(require 'jabber-truncate)

(require 'jabber-ft-client)
(require 'jabber-ft-server)
(require 'jabber-socks5)

;; External notifiers
(require 'jabber-screen)
(require 'jabber-tmux)
(require 'jabber-ratpoison)
(require 'jabber-sawfish)
(require 'jabber-festival)
(require 'jabber-xmessage)
(require 'jabber-wmii)
(require 'jabber-osd)
(require 'jabber-awesome)
(require 'jabber-libnotify)
(require 'jabber-notifications)

;;;###autoload
(defvar *jabber-current-status* nil
  "the users current presence status")

;;;###autoload
(defvar *jabber-current-show* nil
  "the users current presence show")

;;;###autoload
(defvar *jabber-current-priority* nil
  "the user's current priority")

(defvar *jabber-status-history* nil
  "history of status messages")

(defgroup jabber-faces nil "faces for displaying jabber instant messaging"
  :group 'jabber)

(defface jabber-title-small
  '((t (:weight bold :width semi-expanded :height 1.0 :inherit variable-pitch)))
  "face for small titles"
  :group 'jabber-faces)

(defface jabber-title-medium
  '((t (:weight bold :width expanded :height 2.0 :inherit variable-pitch)))
  "face for medium titles"
  :group 'jabber-faces)

(defface jabber-title-large
  '((t (:weight bold :width ultra-expanded :height 3.0 :inherit variable-pitch)))
  "face for large titles"
  :group 'jabber-faces)

(defgroup jabber-debug nil "debugging options"
  :group 'jabber)

(defcustom jabber-debug-log-xml nil
  "Set to non-nil to log all XML i/o in *-jabber-console-JID-* buffer. Set to string to also dump XML i/o in specified file."
  :type '(choice (const :tag "Do not dump XML i/o" nil)
                 (const :tag "Dump XML i/o in console" t)
                 (string :tag "Dump XML i/o in console and this file"))
  :group 'jabber-debug)

(defcustom jabber-debug-keep-process-buffers nil
  "If nil, kill process buffers when the process dies.
Contents of process buffers might be useful for debugging."
  :type 'boolean
  :group 'jabber-debug)

(defcustom jabber-silent-mode nil
  "Silent mode switch. Not ask confirmanions for some operations. DANGEROUS!"
  :type 'boolean
  :group 'jabber)

;;;###autoload
(defconst jabber-presence-faces
 '(("" . jabber-roster-user-online)
   ("away" . jabber-roster-user-away)
   ("xa" . jabber-roster-user-xa)
   ("dnd" . jabber-roster-user-dnd)
   ("chat" . jabber-roster-user-chatty)
   ("error" . jabber-roster-user-error)
   (nil . jabber-roster-user-offline))
 "Mapping from presence types to faces")

(defconst jabber-presence-strings
  `(("" . ,(jabber-propertize "Online" 'face 'jabber-roster-user-online))
    ("away" . ,(jabber-propertize "Away" 'face 'jabber-roster-user-away))
    ("xa" . ,(jabber-propertize "Extended Away" 'face 'jabber-roster-user-xa))
    ("dnd" . ,(jabber-propertize "Do not Disturb" 'face 'jabber-roster-user-dnd))
    ("chat" . ,(jabber-propertize "Chatty" 'face 'jabber-roster-user-chatty))
    ("error" . ,(jabber-propertize "Error" 'face 'jabber-roster-user-error))
    (nil . ,(jabber-propertize "Offline" 'face 'jabber-roster-user-offline)))
  "Mapping from presence types to readable, colorized strings")

;;;###autoload
(defun jabber-customize ()
  "customize jabber options"
  (interactive)
  (customize-group 'jabber))

;;;###autoload
(defun jabber-info ()
  "open jabber.el manual"
  (interactive)
  (info "jabber"))

(provide 'jabber)

;;; arch-tag: 5145153e-4d19-4dc2-800c-b1282feb155d
