;; jabber-notifications.el - emacs-jabber interface to notifications.el

;; Copyright (C) 2014 - Adam Sjøgren - asjo@koldfront.dk
;; Copyright (C) 2010 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2007 - Rodrigo Lazo - rlazo.paz@gmail.com

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

;; Built on jabber-libnotify.el.

(eval-when-compile (require 'jabber-alert))
(unless (string< emacs-version "24.1")  ;notifications.el preset since Emacs 24.1
  (require 'notifications)

  (defcustom jabber-notifications-icon ""
    "Icon to be used on the notification pop-up. Default is empty"
    :type '(file :must-match t)
    :group 'jabber-alerts)

  (defcustom jabber-notifications-timeout nil
    "Specifies the timeout of the pop up window in millisecond"
    :type 'integer
    :group 'jabber-alerts)

  (defcustom jabber-notifications-message-header "Jabber message"
    "Defines the header of the pop up."
    :type 'string
    :group 'jabber-alerts)

  (defcustom jabber-notifications-app "Emacs Jabber"
    "Defines the app of the pop up."
    :type 'string
    :group 'jabber-alerts)

  (defcustom jabber-notifications-urgency "low"
    "Urgency of message"
    :type '(choice (const :tag "Low" "low")
                   (const :tag "Normal" "normal")
                   (const :tag "Critical" "critical"))
    :group 'jabber-alerts)

  (defun jabber-message-notifications (from buffer text title)
    "Show a message through the notifications.el interface"
    (let
        ((body (or (jabber-escape-xml text) " "))
         (head (jabber-escape-xml
                (or title
                    (or jabber-notifications-message-header " ")
                    text)))
         (avatar-hash (get (jabber-jid-symbol from) 'avatar-hash)))
      (notifications-notify
       :title title
       :body body
       :app-icon (or (and avatar-hash (jabber-avatar-find-cached avatar-hash))
                     jabber-notifications-icon)
       :app-name jabber-notifications-app
       :category "jabber.message"
       :timeout jabber-notifications-timeout)))

  (defun jabber-muc-notifications (nick group buffer text title)
    "Show MUC message through the notifications.el interface"
    (jabber-message-notifications group buffer (if nick (format "%s: %s" nick text) text) title)
    )

  (defun jabber-muc-notifications-personal (nick group buffer text title)
    "Show personal MUC message through the notifications.el interface"
    (if (jabber-muc-looks-like-personal-p text group)
        (jabber-muc-notifications nick group buffer text title))
    )

  ;; jabber-*-notifications* requires "from" argument, so we cant use
  ;; define-jabber-alert/define-personal-jabber-alert here and do the
  ;; work by hand:
  (pushnew 'jabber-message-notifications (get 'jabber-alert-message-hooks 'custom-options))
  (pushnew 'jabber-muc-notifications (get 'jabber-alert-muc-hooks 'custom-options))
  (pushnew 'jabber-muc-notifications-personal (get 'jabber-alert-muc-hooks 'custom-options))
  )

(provide 'jabber-notifications)
