;;; pip-requirements.el --- A major mode for editing pip requirements files.

;; Copyright (C) 2014 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 September 2014
;; Version: 20140911.2221
;; X-Original-Version: 0.1

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
(add-to-list 'auto-mode-alist '("\\.pip\\'" . pip-requirements-mode))

(defconst pip-requirements-regex
  (rx
   (group (1+ (or alphanumeric "-")))
   (optional
    "[" (1+ (or alphanumeric "-")) "]")
   (optional
    (group (or "==" ">" ">=" "<" "<="))
    (group (1+ (or digit "."))))))

(defconst pip-requirements-operators
  (list
   (list pip-requirements-regex 1 'font-lock-variable-name-face)
   (list pip-requirements-regex 2 'font-lock-builtin-face)
   (list pip-requirements-regex 3 'font-lock-constant-face)))

;;;###autoload
(define-derived-mode pip-requirements-mode fundamental-mode "pip-require"
  "Major mode for editing pip requirements files."
  (set (make-local-variable 'font-lock-defaults) '(pip-requirements-operators)))

(provide 'pip-requirements)
;;; pip-requirements.el ends here
