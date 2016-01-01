;;; toml-mode.el --- Mojor mode for editing TOML files

;; Copyright (C) 2013 Felix Chern

;; Author: Felix Chern <idryman@gmail.com>
;; Keywords: data toml
;; Package-Version: 20150818.120
;; Version: 0.1.3
;; URL: https://github.com/dryman/toml-mode.el

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary

;; This is a mojor mode for editing files in TOML data format

;;; Code:

(defvar toml-syntax-table nil "Syntax table for `toml-mode'.")
(setq toml-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?# "< b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        syn-table))

(defvar toml-keywords
  '(("\\[\\{1,2\\}[a-zA-Z][^ \n\t\r]+\\]\\{1,2\\}" . font-lock-keyword-face)
    ("[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][Zz]"
     . font-lock-variable-name-face)
    ("\\b[-+]?\\(?:[0-9]*\\.[0-9]+|[0-9]+\\)\\b" . font-lock-variable-name-face))
  "Syntax highlight keywords for `toml-mode`")

(defconst toml-mode-align-rules
  '((toml-equals
     (regexp . "\\(\\s-*\\)=\\(\\s-*\\)")
     (group  . (1 2))
     (modes  . '(toml-mode))
     (separate . entire)))
  "Align rules for Toml Mode.")

;;;###autoload           
(define-derived-mode toml-mode prog-mode "toml"
  :syntax-table toml-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]+")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
  (setq-local parse-sexp-ignore-comments t)
  (setq-local indent-tabs-mode nil)
  (setq font-lock-defaults '(toml-keywords))
  (setq align-mode-rules-list toml-mode-align-rules))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode))

(provide 'toml-mode)

;;; toml-mode.el ends here
