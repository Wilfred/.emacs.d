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

;;; Commentary:
;; Basic highlighting and indentation for Trifle programs.

;;; Code:
(require 'smartparens) ;; sp-up-sexp
(require 'dash) ;; --dotimes

(defvar trifle-mode-hook nil)

(defvar trifle-mode-map
  (make-sparse-keymap)
  "Keymap for Trifle Lisp major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tfl$" . trifle-mode))

(defvar trifle-keyword-regex
  (regexp-opt
   '("if" "let" "function" "macro" "quote" "unquote" "unquote*"
     "while" "set!" "set-symbol!" "throw")
   'symbols))

(defconst trifle-constant-regex
  (regexp-opt '("#false" "#true" "#null")))

(defconst trifle-keyword-symbol-regex
  ":[a-z]+")

(defconst trifle-function-regex
  (rx symbol-start "function" (1+ space) (group (1+ (or word ?! ?- ??)))))

(defconst trifle-macro-regex
  (rx symbol-start "macro" (1+ space) (group (1+ (or word ?! ?- ??)))))

(defconst trifle-font-lock-keywords
  (list
   (cons trifle-keyword-regex font-lock-builtin-face)
   (cons trifle-constant-regex font-lock-constant-face)
   (cons trifle-keyword-symbol-regex font-lock-constant-face)
   (list trifle-function-regex 1 'font-lock-function-name-face)
   (list trifle-macro-regex 1 'font-lock-function-name-face))
  "Highlighting for Trifle mode.")

(defconst trifle-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table for `trifle-mode'.")

;;;###autoload
(define-derived-mode trifle-mode lisp-mode "Trifle"
  "Major mode for editing Trifle lisp code."
  :syntax-table trifle-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(trifle-font-lock-keywords))
  (setq indent-line-function 'trifle-indent))

(defun trifle-indent ()
  "Indent the current line according to Trifle indent rules.
For every level of parentheses, indent by two spaces."
  (interactive)
  (let ((sexp-depth 0))
    ;; Calculate sexp depth by calling sp-up-sexp until we cannot go
    ;; up any further.
    (save-excursion
      (beginning-of-line)
      (when (looking-at " *)")
        (end-of-line))
      (while (sp-up-sexp)
        (incf sexp-depth)))

    (back-to-indentation)
    (let ((target-indent (* sexp-depth 2))
          (current-indent (current-column)))
      (cond
       ;; Indent more if we haven't indented enough.
       ((< current-indent target-indent)
        (--dotimes (- target-indent  current-indent)
          (insert " ")))
       ;; Unindent if we're too indented.
       ((> current-indent target-indent)
        (delete-char (- target-indent current-indent))))
      )
    ))

(provide 'trifle-mode)
;;; trifle-mode.el ends here
