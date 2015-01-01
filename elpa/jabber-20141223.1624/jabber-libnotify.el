;; jabber-libnotify.el - emacs-jabber interface to libnotify

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

(require 'dbus nil t)
(eval-when-compile (require 'jabber-alert))

(defcustom jabber-libnotify-icon ""
  "Icon to be used on the notification pop-up. Default is empty"
  :type '(file :must-match t)
  :group 'jabber-alerts)


(defcustom jabber-libnotify-timeout 2500
  "Specifies the timeout of the pop up window in millisecond"
  :type 'integer
  :group 'jabber-alerts)

(defcustom jabber-libnotify-message-header "Jabber message"
  "Defines the header of the pop up."
  :type 'string
  :group 'jabber-alerts)

(defcustom jabber-libnotify-app "Emacs Jabber"
  "Defines the app of the pop up."
  :type 'string
  :group 'jabber-alerts)

(defcustom jabber-libnotify-urgency "low"
  "Urgency of libnotify message"
  :type '(choice (const :tag "Low" "low")
                 (const :tag "Normal" "normal")
                 (const :tag "Critical" "critical"))
  :group 'jabber-alerts)

(defcustom jabber-libnotify-method (if (featurep 'dbus) 'dbus 'shell)
  "Specifies the method for libnotify call. Dbus is more faster but require emacs23+"
  :type '(choice (const :tag "Shell" shell)
                 (const :tag "D-Bus" dbus))
  :group 'jabber-alerts)

(defvar jabber-libnotify-id 0)

(defun jabber-libnotify-next-id ()
  "Return the next notification id."
  (setq jabber-libnotify-id (+ jabber-libnotify-id 1)))

(defun jabber-libnotify-message (text &optional title)
  "Show MSG using libnotify"
  (let
      ((body (or (jabber-escape-xml text) " "))
       (head (jabber-escape-xml
              (or title
                  (or jabber-libnotify-message-header " ")
                  text))))
    ;; Possible errors include not finding the notify-send binary.
    (condition-case e
        (cond
         ((eq jabber-libnotify-method 'shell)
          (let ((process-connection-type nil))
            (start-process "notification" nil "notify-send"
                           "-t" (format "%s" jabber-libnotify-timeout)
                           "-i" (or jabber-libnotify-icon "\"\"")
                           "-u" jabber-libnotify-urgency
                           head body)))
         ((eq jabber-libnotify-method 'dbus)
          (dbus-call-method
           :session                                 ; use the session (not system) bus
           "org.freedesktop.Notifications"          ; service name
           "/org/freedesktop/Notifications"         ; path name
           "org.freedesktop.Notifications" "Notify" ; Method
           jabber-libnotify-app
           (jabber-libnotify-next-id)
           jabber-libnotify-icon
           ':string (encode-coding-string head 'utf-8)
           ':string (encode-coding-string body 'utf-8)
           '(:array)
           '(:array :signature "{sv}")
           ':int32 jabber-libnotify-timeout)))
      (error nil))))

(define-jabber-alert libnotify "Show a message through the libnotify interface"
  'jabber-libnotify-message)
(define-personal-jabber-alert jabber-muc-libnotify)

(provide 'jabber-libnotify)

;; arch-tag: e9c4c210-8245-11dd-bddf-000a95c2fcd0
