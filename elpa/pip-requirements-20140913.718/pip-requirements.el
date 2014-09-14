;;; pip-requirements.el --- A major mode for editing pip requirements files.

;; Copyright (C) 2014 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 September 2014
;; Version: 20140913.718
;; X-Original-Version: 0.2

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

;; This mode adds basic highlighting for pip requirements files to Emacs.

;; TODO: Steal shamelessly all the fantasic ideas in
;; https://github.com/wuub/requirementstxt

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(rx ".pip" string-end) . pip-requirements-mode))
;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(rx "requirements" (zero-or-more anything) ".txt" string-end) . pip-requirements-mode))

(defconst pip-requirements-name-regex
  (rx
   line-start
   (group (1+ (or alphanumeric "-")))))

(defconst pip-requirements-version-regex
  (rx
   (group (or "==" ">" ">=" "<" "<=" "!="))
   (group (1+ (or digit "b" ".")))))

(defconst pip-requirements-operators
  (list
   (list pip-requirements-name-regex 1 'font-lock-variable-name-face)
   (list pip-requirements-version-regex 1 'font-lock-builtin-face)
   (list pip-requirements-version-regex 2 'font-lock-constant-face)))

(defconst pip-requirements-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

;;;###autoload
(define-derived-mode pip-requirements-mode fundamental-mode "pip-require"
  "Major mode for editing pip requirements files."
  :syntax-table pip-requirements-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(pip-requirements-operators)))

(provide 'pip-requirements)
;;; pip-requirements.el ends here
