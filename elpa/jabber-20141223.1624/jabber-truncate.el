;; jabber-truncate.el - cleanup top lines in chatbuffers

;; Copyright (C) 2007 - Kirill A. Korinskiy - catap@catap.ru

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

(require 'jabber-chat)
(require 'jabber-alert)

(require 'cl)

(defvar jabber-log-lines-to-keep 1000
  "Maximum number of lines in chat buffer")

(defun jabber-truncate-top (buffer &optional ewoc)
  "Clean old history from a chat BUFFER.
Optional EWOC is ewoc-widget to work. Default is jabber-chat-ewoc
`jabber-log-lines-to-keep' specifies the number of lines to
keep.

Note that this might interfer with
`jabber-chat-display-more-backlog': you ask for more history, you
get it, and then it just gets deleted."
  (interactive)
    (let* ((inhibit-read-only t)
           (work-ewoc (if ewoc ewoc jabber-chat-ewoc))
          (delete-before
           ;; go back one node, to make this function "idempotent"
           (ewoc-prev
            work-ewoc
            (ewoc-locate work-ewoc
                         (save-excursion
                           (set-buffer buffer)
                           (goto-char (point-max))
                           (forward-line (- jabber-log-lines-to-keep))
                           (point))))))
      (while delete-before
        (setq delete-before
              (prog1
                  (ewoc-prev work-ewoc delete-before)
                (ewoc-delete work-ewoc delete-before))))))

(defun jabber-truncate-muc (nick group buffer text proposed-alert)
  "Clean old history from MUC buffers.
`jabber-log-lines-to-keep' specifies the number of lines to
keep."
  (jabber-truncate-top buffer))

(defun jabber-truncate-chat (from buffer text proposed-alert)
  "Clean old history from chat buffers.
`jabber-log-lines-to-keep' specifies the number of lines to
keep.

Note that this might interfer with
`jabber-chat-display-more-backlog': you ask for more history, you
get it, and then it just gets deleted."
  (jabber-truncate-top buffer))

(provide 'jabber-truncate)

;; arch-tag: 3d1e3428-f598-11db-a314-000a95c2fcd0
