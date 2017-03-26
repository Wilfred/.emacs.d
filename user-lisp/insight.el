;;; insight.el --- overview of elisp functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by <youtube link here>

;;; Code:

(defun insight-mock ()
  (interactive)
  (let ((buf (get-buffer-create "*insight*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Symbol: nthcdr
Type: Function
Defnition: C source code

* Description
foo bar

* Code
foo bar
baz

* Advice
None

* Callers
foo-bar (foo.el:21)
foo-baz (foo.el:31)
")
      (outline-mode))
    (switch-to-buffer buf)))

(provide 'insight)
;;; insight.el ends here
