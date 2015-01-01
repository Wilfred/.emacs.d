;; jabber-ft-client.el - send file transfer requests, by JEP-0096

;; Copyright (C) 2004 - Magnus Henoch - mange@freemail.hu

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

(eval-when-compile (require 'cl))

(require 'jabber-si-client)
(require 'jabber-util)

(require 'jabber-ft-common)

(defun jabber-ft-send (jc jid filename desc)
  "Attempt to send FILENAME to JID."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Send file to: " nil nil nil 'full t)
		     (read-file-name "Send which file: " nil nil t)
		     (jabber-read-with-input-method "Description (optional): ")))
  (if (zerop (length desc)) (setq desc nil))
  (setq filename (expand-file-name filename))
  (access-file filename "Couldn't open file")

  (let* ((attributes (file-attributes filename))
	 (size (nth 7 attributes))
	 (date (nth 5 attributes))
	 (hash (jabber-ft-get-md5 filename)))
    (jabber-si-initiate jc jid "http://jabber.org/protocol/si/profile/file-transfer"
			`(file ((xmlns . "http://jabber.org/protocol/si/profile/file-transfer")
				(name . ,(file-name-nondirectory filename))
				(size . ,size)
				(date . ,(jabber-encode-time date))
				,@(when hash
				    (list (cons 'hash hash))))
			       (desc () ,desc))
			(lexical-let ((filename filename))
			  (lambda (jc jid sid send-data-function)
			    (jabber-ft-do-send
			     jid sid send-data-function filename))))))

(defun jabber-ft-do-send (jid sid send-data-function filename)
  (if (stringp send-data-function)
      (message "File sending failed: %s" send-data-function)
    (with-temp-buffer
      (insert-file-contents-literally filename)
      
      ;; Ever heard of buffering?
      (funcall send-data-function (buffer-string))
      (message "File transfer completed")))
  ;; File transfer is monodirectional, so ignore received data.
  #'ignore)

(provide 'jabber-ft-client)
;;; arch-tag: fba686d5-37b5-4165-86c5-49b76fa0ea6e
