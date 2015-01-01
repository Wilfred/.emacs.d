;; jabber-tmux.el - emacs-jabber interface to tmux

;; Copyright (C) 2012 - Michael Cardell Widerkrantz <mc@hack.org>

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

(defun jabber-tmux-message (msg)
  "Show MSG in tmux"
  (call-process "tmux" nil nil nil "display-message" msg))

; Automatically defines jabber-{message,muc,presence,info}-tmux
; functions.
(define-jabber-alert tmux "Show a message through the tmux terminal multiplexer"
  'jabber-tmux-message)

(provide 'jabber-tmux)
