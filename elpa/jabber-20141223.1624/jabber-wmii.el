;; jabber-wmii.el - emacs-jabber interface to wmii

;; Copyright (C) 2007 - Detlev Zundel - dzu@gnu.org

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

(eval-when-compile (require 'jabber-alert))

(defvar jabber-wmii-color "#ffffff #335577 #447799"
  "Color specification as needed by the wmii window manager for the jabber alert messages.")

(defvar jabber-wmii-reset-time "20 sec"
  "If non-nil time to reset wmii message.  If nil the message has to be cleared by other means, i.e. from wmiirc.")

(defvar jabber-wmii-timer nil
  "Timer to clear wmii message.")

(defun jabber-wmii-clear ()
  "Clear any previous message output through wmii window manager."
  (condition-case e
      (call-process "wmiir" nil nil nil "remove" "/rbar/jabber")
    (error nil)))
  
(defun jabber-wmii-message (text &optional title)
  "Show MSG in wmii."
  (when jabber-wmii-timer
    (cancel-timer jabber-wmii-timer))
  (let ((tmp (make-temp-file temporary-file-directory)))
    (with-temp-file tmp
      (insert  jabber-wmii-color " " (or title text)))
    ;; Possible errors include not finding the wmiir binary, and
    ;; too many pipes open because of message flood.
    (condition-case e
	(call-process "wmiir" tmp nil nil "create" "/rbar/jabber")
      (error nil))
    (delete-file tmp))
  (when jabber-wmii-reset-time
    (setq jabber-wmii-timer
	  (run-at-time jabber-wmii-reset-time nil 'jabber-wmii-clear))))

(define-jabber-alert wmii "Show a message through the wmii window manager."
  'jabber-wmii-message)

(provide 'jabber-wmii)
