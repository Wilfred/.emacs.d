;; jabber-menu.el - menu definitions

;; Copyright (C) 2003, 2004, 2008 - Magnus Henoch - mange@freemail.hu
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

(require 'jabber-util)
(eval-when-compile (require 'cl))

;;;###autoload
(defvar jabber-menu
  (let ((map (make-sparse-keymap "jabber-menu")))
    (define-key-after map
      [jabber-menu-connect]
      '("Connect" . jabber-connect-all))

    (define-key-after map
      [jabber-menu-disconnect]
      '(menu-item "Disconnect" jabber-disconnect
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-status]
      `(menu-item "Set Status" ,(make-sparse-keymap "set-status")
		  :enable (bound-and-true-p jabber-connections)))

    (define-key map
      [jabber-menu-status jabber-menu-status-chat]
      '(menu-item
	"Chatty"
	(lambda ()
	  (interactive)
	  (jabber-send-presence "chat"
				(jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*)
				*jabber-current-priority*))
	:button (:radio . (and (boundp '*jabber-current-show*)
			       (equal *jabber-current-show* "chat")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-dnd]
      '(menu-item
	"Do not Disturb"
	(lambda ()
	  (interactive)
	  (jabber-send-presence "dnd"
				(jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*)
				*jabber-current-priority*))
	:button (:radio . (and (boundp '*jabber-current-show*)
			       (equal *jabber-current-show* "dnd")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-xa]
      '(menu-item "Extended Away" jabber-send-xa-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "xa")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-away]
      '(menu-item "Away" jabber-send-away-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "away")))))
    (define-key map
      [jabber-menu-status jabber-menu-status-online]
      '(menu-item "Online" jabber-send-default-presence
		  :button (:radio . (and (boundp '*jabber-current-show*)
					 (equal *jabber-current-show* "")))))

    (define-key-after map
      [separator]
      '(menu-item "--"))

    (define-key-after map
      [jabber-menu-chat-with]
      '(menu-item "Chat with..." jabber-chat-with
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-nextmsg]
      '(menu-item "Next unread message" jabber-activity-switch-to
		  :enable (bound-and-true-p jabber-activity-jids)))

    (define-key-after map
      [jabber-menu-send-subscription-request]
      '(menu-item "Send subscription request" jabber-send-subscription-request
		  :enable (bound-and-true-p jabber-connections)))

    (define-key-after map
      [jabber-menu-roster]
      '("Switch to roster" . jabber-switch-to-roster-buffer))

    (define-key-after map
      [separator2]
      '(menu-item "--"))


    (define-key-after map
      [jabber-menu-customize]
      '("Customize" . jabber-customize))

    (define-key-after map
      [jabber-menu-info]
      '("Help" . jabber-info))

    map))

;;;###autoload
(defcustom jabber-display-menu 'maybe
  "Decide whether the \"Jabber\" menu is displayed in the menu bar.
If t, always display.
If nil, never display.
If maybe, display if jabber.el is installed under `package-user-dir', or
if any of `jabber-account-list' or `jabber-connections' is non-nil."
  :group 'jabber
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When installed by user, or when any accounts have been configured or connected" maybe)))

(defun jabber-menu (&optional remove)
  "Put \"Jabber\" menu on menubar.
With prefix argument, remove it."
  (interactive "P")
  (setq jabber-display-menu (if remove nil t))
  (force-mode-line-update))
(make-obsolete 'jabber-menu "set the variable `jabber-display-menu' instead.")

;; This used to be:
;; (define-key-after global-map [menu-bar jabber-menu] ...)
;; but that doesn't work in Emacs 21.
;;;###autoload
(define-key-after (lookup-key global-map [menu-bar])
  [jabber-menu]
  (list 'menu-item "Jabber" jabber-menu
	:visible
	;; If the package was installed by the user personally, it's
	;; probably ok to "clutter" the menu bar with a Jabber menu.
	(let ((user-installed-package
	       (and (bound-and-true-p package-user-dir)
		    (string=
		     (file-name-as-directory
		      (expand-file-name ".." (file-name-directory load-file-name)))
		     (file-name-as-directory
		      (expand-file-name package-user-dir))))))
	  `(or (eq jabber-display-menu t)
	       (and (eq jabber-display-menu 'maybe)
		    (or ,user-installed-package
			jabber-account-list
			(bound-and-true-p jabber-connections)))))))

(defvar jabber-jid-chat-menu nil
  "Menu items for chat menu")

(defvar jabber-jid-info-menu nil
  "Menu item for info menu")

(defvar jabber-jid-roster-menu nil
  "Menu items for roster menu")

(defvar jabber-jid-muc-menu nil
  "Menu items for MUC menu")

(defvar jabber-jid-service-menu nil
  "Menu items for service menu")

(defun jabber-popup-menu (which-menu)
  "Popup specified menu"
  (let* ((mouse-event (and (listp last-input-event) last-input-event))
	 (choice (widget-choose "Actions" which-menu mouse-event)))
    (if mouse-event
	(mouse-set-point mouse-event))
    (if choice
	(call-interactively choice))))

(defun jabber-popup-chat-menu ()
  "Popup chat menu"
  (interactive)
  (jabber-popup-menu jabber-jid-chat-menu))

(defun jabber-popup-info-menu ()
  "Popup info menu"
  (interactive)
  (jabber-popup-menu jabber-jid-info-menu))

(defun jabber-popup-roster-menu ()
  "Popup roster menu"
  (interactive)
  (jabber-popup-menu jabber-jid-roster-menu))

(defun jabber-popup-muc-menu ()
  "Popup MUC menu"
  (interactive)
  (jabber-popup-menu jabber-jid-muc-menu))

(defun jabber-popup-service-menu ()
  "Popup service menu"
  (interactive)
  (jabber-popup-menu jabber-jid-service-menu))

(defun jabber-popup-combined-menu ()
  "Popup combined menu"
  (interactive)
  (jabber-popup-menu (append jabber-jid-chat-menu jabber-jid-info-menu jabber-jid-roster-menu jabber-jid-muc-menu)))

(provide 'jabber-menu)

;;; arch-tag: 5147f52f-de47-4348-86ff-b799d7a75e3f
