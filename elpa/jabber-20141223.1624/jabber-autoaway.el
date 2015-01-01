;;; jabber-autoaway.el --- change status to away after idleness

;; Copyright (C) 2010 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2010 - Terechkov Evgenii - evg@altlinux.org
;; Copyright (C) 2006, 2008  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(eval-when-compile (require 'cl))
(require 'time-date)

(defgroup jabber-autoaway nil
  "Change status to away after idleness"
  :group 'jabber)

(defcustom jabber-autoaway-methods
  (if (fboundp 'jabber-autoaway-method)
      (list jabber-autoaway-method)
    (list 'jabber-current-idle-time
          'jabber-xprintidle-get-idle-time
          'jabber-termatime-get-idle-time))
  "Methods used to keep track of idleness.
This is a list of functions that takes no arguments, and returns the
number of seconds since the user was active, or nil on error."
  :group 'jabber-autoaway
  :options '(jabber-current-idle-time
             jabber-xprintidle-get-idle-time
             jabber-termatime-get-idle-time))

(defcustom jabber-autoaway-timeout 5
  "Minutes of inactivity before changing status to away"
  :group 'jabber-autoaway
  :type 'number)

(defcustom jabber-autoaway-xa-timeout 10
  "Minutes of inactivity before changing status to xa. Set to 0 to disable."
  :group 'jabber-autoaway
  :type 'number)

(defcustom jabber-autoaway-status "Idle"
  "Status string for autoaway"
  :group 'jabber-autoaway
  :type 'string)

(defcustom jabber-autoaway-xa-status "Extended away"
  "Status string for autoaway in xa state"
  :group 'jabber-autoaway
  :type 'string)

(defcustom jabber-autoaway-priority nil
  "Priority for autoaway.
If nil, don't change priority.  See the manual for more
information about priority."
  :group 'jabber-autoaway
  :type '(choice (const :tag "Don't change")
		 (integer :tag "Priority"))
  :link '(info-link "(jabber)Presence"))

(defcustom jabber-autoaway-xa-priority nil
  "Priority for autoaway in xa state.
If nil, don't change priority.  See the manual for more
information about priority."
  :group 'jabber-autoaway
  :type '(choice (const :tag "Don't change")
		 (integer :tag "Priority"))
  :link '(info-link "(jabber)Presence"))

(defcustom jabber-xprintidle-program (executable-find "xprintidle")
  "Name of the xprintidle program"
  :group 'jabber-autoaway
  :type 'string)

(defcustom jabber-autoaway-verbose nil
  "If nil, don't print autoaway status messages."
  :group 'jabber-autoaway
  :type 'boolean)

(defvar jabber-autoaway-timer nil)

(defvar jabber-autoaway-last-idle-time nil
  "Seconds of idle time the last time we checked.
This is used to detect whether the user has become unidle.")

(defun jabber-autoaway-message (&rest args)
  (when jabber-autoaway-verbose
    (apply #'message args)))

;;;###autoload
(defun jabber-autoaway-start (&optional ignored)
  "Start autoaway timer.
The IGNORED argument is there so you can put this function in
`jabber-post-connect-hooks'."
  (interactive)
  (unless jabber-autoaway-timer
    (setq jabber-autoaway-timer
	  (run-with-timer (* jabber-autoaway-timeout 60) nil #'jabber-autoaway-timer))
    (jabber-autoaway-message "Autoaway timer started")))

(defun jabber-autoaway-stop ()
  "Stop autoaway timer."
  (interactive)
  (when jabber-autoaway-timer
    (jabber-cancel-timer jabber-autoaway-timer)
    (setq jabber-autoaway-timer nil)
    (jabber-autoaway-message "Autoaway timer stopped")))

(defun jabber-autoaway-get-idle-time ()
  "Get idle time in seconds according to jabber-autoaway-methods.
Return nil on error."
  (car (sort (mapcar 'funcall jabber-autoaway-methods) (lambda (a b) (if a (if b (< a b) t) nil)))))

(defun jabber-autoaway-timer ()
  ;; We use one-time timers, so reset the variable.
  (setq jabber-autoaway-timer nil)
  (let ((idle-time (jabber-autoaway-get-idle-time)))
    (when (numberp idle-time)
      ;; Has "idle timeout" passed?
      (if (> idle-time (* 60 jabber-autoaway-timeout))
	  ;; If so, mark ourselves idle.
	  (jabber-autoaway-set-idle)
	;; Else, start a timer for the remaining amount.
	(setq jabber-autoaway-timer 
	      (run-with-timer (- (* 60 jabber-autoaway-timeout) idle-time)
			      nil #'jabber-autoaway-timer))))))

(defun jabber-autoaway-set-idle (&optional xa)
  (jabber-autoaway-message "Autoaway triggered")
  ;; Send presence, unless the user has set a custom presence
  (unless (member *jabber-current-show* '("xa" "dnd"))
    (jabber-send-presence
     (if xa "xa" "away")
     (if (or (string= *jabber-current-status* jabber-default-status) (string= *jabber-current-status* jabber-autoaway-status)) (if xa jabber-autoaway-xa-status jabber-autoaway-status) *jabber-current-status*)
     (or (if xa jabber-autoaway-priority jabber-autoaway-xa-priority) *jabber-current-priority*)))

  (setq jabber-autoaway-last-idle-time (jabber-autoaway-get-idle-time))
  ;; Run unidle timer every 10 seconds (if xa specified, timer already running)
  (unless xa
    (setq jabber-autoaway-timer (run-with-timer 10 10
					      #'jabber-autoaway-maybe-unidle))))

(defun jabber-autoaway-maybe-unidle ()
  (let ((idle-time (jabber-autoaway-get-idle-time)))
    (jabber-autoaway-message "Idle for %d seconds" idle-time)
    (if (member *jabber-current-show* '("xa" "away"))
        ;; As long as idle time increases monotonically, stay idle.
        (if (> idle-time jabber-autoaway-last-idle-time)
            (progn
              ;; Has "Xa timeout" passed?
              (if (and (> jabber-autoaway-xa-timeout 0) (> idle-time (* 60 jabber-autoaway-xa-timeout)))
                  ;; iIf so, mark ourselves xa.
                  (jabber-autoaway-set-idle t))
              (setq jabber-autoaway-last-idle-time idle-time))
          ;; But if it doesn't, go back to unidle state.
          (jabber-autoaway-message "Back to unidle")
          ;; But don't mess with the user's custom presence.
          (if (or (string= *jabber-current-status* jabber-autoaway-status) (string= *jabber-current-status* jabber-autoaway-xa-status))
              (jabber-send-default-presence)
            (progn
              (jabber-send-presence jabber-default-show *jabber-current-status* jabber-default-priority)
              (jabber-autoaway-message "%S /= %S - not resetting presence" *jabber-current-status* jabber-autoaway-status)))
          (jabber-autoaway-stop)
          (jabber-autoaway-start)))))

(defun jabber-xprintidle-get-idle-time ()
  "Get idle time through the xprintidle program."
  (when jabber-xprintidle-program
    (with-temp-buffer
      (when (zerop (call-process jabber-xprintidle-program
				 nil t))
	(/ (string-to-number (buffer-string)) 1000.0)))))

(defun jabber-termatime-get-idle-time ()
  "Get idle time through atime of terminal.
The method for finding the terminal only works on GNU/Linux."
  (let ((terminal (cond
		   ((file-exists-p "/proc/self/fd/0")
		    "/proc/self/fd/0")
		   (t
		    nil))))
    (when terminal
      (let* ((atime-of-tty (nth 4 (file-attributes terminal)))
	     (diff (time-to-seconds (time-since atime-of-tty))))
	(when (> diff 0)
	  diff)))))

(defun jabber-current-idle-time ()
  "Get idle time through `current-idle-time'.
`current-idle-time' was introduced in Emacs 22."
  (if (fboundp 'current-idle-time)
      (let ((idle-time (current-idle-time)))
        (if (null idle-time)
            0
          (float-time idle-time)))))

(provide 'jabber-autoaway)
;; arch-tag: 5bcea14c-842d-11da-a120-000a95c2fcd0
