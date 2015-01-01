;; jabber-ratpoison.el - emacs-jabber interface to ratpoison

;; Copyright (C) 2005, 2008 - Magnus Henoch - mange@freemail.hu

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

(defun jabber-ratpoison-message (text &optional title)
  "Show MSG in Ratpoison"
  ;; Possible errors include not finding the ratpoison binary.
  (condition-case e
      (let ((process-connection-type))
	(call-process "ratpoison" nil 0 nil "-c" (concat "echo " (or title text))))
    (error nil)))
  
(define-jabber-alert ratpoison "Show a message through the Ratpoison window manager"
  'jabber-ratpoison-message)

(provide 'jabber-ratpoison)
;; arch-tag: 19650075-5D05-11D9-B80F-000A95C2FCD0
