;;; jabber-ft-common.el --- Common functions for sending and receiving files (JEP-0096)

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

(defcustom jabber-ft-md5sum-program (or (when (executable-find "md5")
					  (list (executable-find "md5") "-n"))
					(when (executable-find "md5sum")
					  (list (executable-find "md5sum"))))
  "The program to use to calculate MD5 sums of files.
The first item should be the name of the program, and the remaing
items the arguments.  The file name is appended as the last
argument."
  :type '(repeat string)
  :group 'jabber)

(defun jabber-ft-get-md5 (file-name)
  "Get MD5 sum of FILE-NAME, and return as hex string.
Return nil if no MD5 summing program is available."
  (when jabber-ft-md5sum-program
    (with-temp-buffer
      (apply 'call-process (car jabber-ft-md5sum-program) nil t nil
	     (append (cdr jabber-ft-md5sum-program) (list file-name)))
      ;; Output is "hexsum filename"
      (goto-char (point-min))
      (forward-word 1)
      (buffer-substring (point-min) (point)))))

(provide 'jabber-ft-common)
;; arch-tag: 1ce4cce0-8360-11da-a5ba-000a95c2fcd0
