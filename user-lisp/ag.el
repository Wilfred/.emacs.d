;;; ag.el --- A front-end for ag, the C ack replacement.

;; Copyright (C) 2012 Wilfred Hughes <me@wilfred.meuk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 January 2012
;; Version: 0.1

;;; Commentary

;; This file is heavily based on the excellent ack-and-a-half.el.
;; You will need dash.el[1] and s.el[2] to use ag.el
;;
;; [1]: https://github.com/magnars/dash.el
;; [2]: https://github.com/magnars/s.el

;;; Todo

;; 1. Remove external dependencies.
;; 2. Add ag-project
;; 3. Add ag-project-at-point
;; 4. Add ag-regexp
;; 5. Add highlighting to *Ag* buffer

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 's)
(require 'dash)

(defcustom ag-arguments
  (list "--nocolor" "--literal" "--smart-case" "--nogroup" "--column")
  "Default arguments passed to ag."
  :type '(repeat (string)))

(define-compilation-mode ag-mode "Ag"
  "Ag results compilation mode")

(defun ag/shell-quote (string)
  "Wrap in single quotes, and quote existing single quotes to make shell safe."
  (concat "'" (ack-and-a-half-string-replace "'" "'\\''" string) "'"))

(defun ag (string directory)
  "Run ag searching for the literal STRING given in DIRECTORY."
  (interactive "sSearch string: \nDDirectory: ")
  (compilation-start (s-join " "
                             (-concat '("ag") ag-arguments (list (ag/shell-quote string))))
                     'ag-mode))
