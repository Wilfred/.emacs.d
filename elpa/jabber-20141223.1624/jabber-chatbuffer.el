;; jabber-chatbuffer.el - functions common to all chat buffers

;; Copyright (C) 2005, 2007, 2008 - Magnus Henoch - mange@freemail.hu

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

(require 'jabber-keymap)

(defvar jabber-point-insert nil
  "Position where the message being composed starts")

(defvar jabber-send-function nil
  "Function for sending a message from a chat buffer.")

(defvar jabber-chat-mode-hook nil
  "Hook called at the end of `jabber-chat-mode'.
Note that functions in this hook have no way of knowing
what kind of chat buffer is being created.")

(defcustom jabber-chat-fill-long-lines t
  "If non-nil, fill long lines in chat buffers.
Lines are broken at word boundaries at the width of the
window or at `fill-column', whichever is shorter."
  :group 'jabber-chat
  :type 'boolean)

(defvar jabber-chat-ewoc nil
  "The ewoc showing the messages of this chat buffer.")

;;;###autoload
(defvar jabber-buffer-connection nil
  "The connection used by this buffer.")
;;;###autoload
(make-variable-buffer-local 'jabber-buffer-connection)

(defun jabber-chat-mode (jc ewoc-pp)
  "\\{jabber-chat-mode-map}"
  (kill-all-local-variables)
  ;; Make sure to set this variable somewhere
  (make-local-variable 'jabber-send-function)
  (make-local-variable 'scroll-conservatively)
  (make-local-variable 'jabber-point-insert)
  (make-local-variable 'jabber-chat-ewoc)
  (make-local-variable 'buffer-undo-list)
  
  (setq jabber-buffer-connection jc
        scroll-conservatively 5
        buffer-undo-list t)             ;dont keep undo list for chatbuffer
  
  (unless jabber-chat-ewoc
    (setq jabber-chat-ewoc
	  (ewoc-create ewoc-pp nil "---"))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))
    (setq jabber-point-insert (point-marker)))

  ;;(setq header-line-format jabber-chat-header-line-format)

  (setq major-mode 'jabber-chat-mode
        mode-name "jabber-chat")
  (use-local-map jabber-chat-mode-map)

  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'jabber-chat-mode-hook)
    (run-hooks 'jabber-chat-mode-hook)))

(put 'jabber-chat-mode 'mode-class 'special)

;; Spell check only what you're currently writing
(defun jabber-chat-mode-flyspell-verify ()
  (>= (point) jabber-point-insert))
(put 'jabber-chat-mode 'flyspell-mode-predicate
  'jabber-chat-mode-flyspell-verify)

(defvar jabber-chat-mode-map 
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jabber-common-keymap)
    (define-key map "\r" 'jabber-chat-buffer-send)
    map))

(defun jabber-chat-buffer-send ()
  (interactive)
  ;; If user accidentally hits RET without writing anything, just
  ;; ignore it.
  (when (plusp (- (point-max) jabber-point-insert))
    ;; If connection was lost...
    (unless (memq jabber-buffer-connection jabber-connections)
      ;; ...maybe there is a new connection to the same account.
      (let ((new-jc (jabber-find-active-connection jabber-buffer-connection)))
	(if new-jc
	    ;; If so, just use it.
	    (setq jabber-buffer-connection new-jc)
	  ;; Otherwise, ask for a new account.
	  (setq jabber-buffer-connection (jabber-read-account t)))))

    (let ((body (delete-and-extract-region jabber-point-insert (point-max))))
      (funcall jabber-send-function jabber-buffer-connection body))))

(defun jabber-chat-buffer-fill-long-lines ()
  "Fill lines that are wider than the window width."
  ;; This was mostly stolen from article-fill-long-lines
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (width (window-width (get-buffer-window (current-buffer)))))
      (goto-char (point-min))
      (let ((adaptive-fill-mode nil))	;Why?  -sm
	(while (not (eobp))
	  (end-of-line)
	  (when (>= (current-column) (min fill-column width))
	    (save-restriction
	      (narrow-to-region (min (1+ (point)) (point-max))
				(point-at-bol))
	      (let ((goback (point-marker)))
		(fill-paragraph nil)
		(goto-char (marker-position goback)))))
	  (forward-line 1))))))

(provide 'jabber-chatbuffer)
;; arch-tag: 917e5b60-5894-4c49-b3bc-12e1f97ffdc6
