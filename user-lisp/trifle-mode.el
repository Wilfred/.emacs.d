;;; trifle-mode.el --- A major mode for Trifle lisp.

;; Copyright (C) 2013 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 23 February 2014

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

;;; Code:

(defvar trifle-mode-hook nil)

(defvar trifle-mode-map
  (make-sparse-keymap)
  "Keymap for Trifle lisp major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tfl$" . trifle-mode))

(defconst trifle-font-lock-keywords
  (list
   `(,(regexp-opt '("if" "let" "function" "macro" "quote" "while" "set!" "set-symbol!") 'symbols) . font-lock-builtin-face))
  "Highlighting for Trifle mode")

(defvar trifle-mode-syntax-table
  (make-syntax-table))


(define-derived-mode trifle-mode lisp-mode "Trifle"
  "Major mode for editing Trifle lisp code."
  :syntax-table trifle-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(trifle-font-lock-keywords)))

(provide 'trifle-mode)
