;;; jabber-compose.el --- compose a Jabber message in a buffer

;; Copyright (C) 2006, 2007  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>
;; Keywords: 

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

;;; Code:

;;;###autoload
(defun jabber-compose (jc &optional recipient)
  "Create a buffer for composing a Jabber message."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "To whom? ")))

  (with-current-buffer (get-buffer-create 
			(generate-new-buffer-name
			 (concat
			  "Jabber-Compose"
			  (when recipient
			    (format "-%s" (jabber-jid-displayname recipient))))))
    (set (make-local-variable 'jabber-widget-alist) nil)
    (setq jabber-buffer-connection jc)
    (use-local-map widget-keymap)

    (insert (jabber-propertize "Compose Jabber message\n" 'face 'jabber-title-large))

    (insert (substitute-command-keys "\\<widget-field-keymap>Completion available with \\[widget-complete].\n"))
    (push (cons :recipients
		(widget-create '(repeat :tag "Recipients" jid)
			       :value (when recipient
					(list recipient))))
	  jabber-widget-alist)

    (insert "\nSubject: ")
    (push (cons :subject
		(widget-create 'editable-field :value ""))
	  jabber-widget-alist)

    (insert "\nText:\n")
    (push (cons :text
		(widget-create 'text :value ""))
	  jabber-widget-alist)

    (insert "\n")
    (widget-create 'push-button :notify #'jabber-compose-send "Send")

    (widget-setup)

    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))

(defun jabber-compose-send (&rest ignore)
  (let ((recipients (widget-value (cdr (assq :recipients jabber-widget-alist))))
	(subject (widget-value (cdr (assq :subject jabber-widget-alist))))
	(text (widget-value (cdr (assq :text jabber-widget-alist)))))
    (when (null recipients)
      (error "No recipients specified"))

    (dolist (to recipients)
      (jabber-send-message jabber-buffer-connection to subject text nil))

    (bury-buffer)
    (message "Message sent")))

(provide 'jabber-compose)
;; arch-tag: 59032c00-994d-11da-8d97-000a95c2fcd0
