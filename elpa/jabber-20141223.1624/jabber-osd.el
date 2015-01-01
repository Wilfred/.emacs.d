;;; jabber-osd.el --- OSD support for jabber.el

;; Copyright (C) 2008 - Terechkov Evgenii - evg@altlinux.org

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile (require 'jabber-alert))

(condition-case e
    (progn
      ;; Most people don't have osd.el, so this will often fail
      (require 'osd)
      (define-jabber-alert osd "Display a message in osd"
	(lambda (text &optional title) (osd-show-string (or title text))))
      (define-personal-jabber-alert jabber-muc-osd))
  (error nil))

(provide 'jabber-osd)

;; arch-tag: 3eb8d55a-dd86-11dc-b2c6-000a95c2fcd0
