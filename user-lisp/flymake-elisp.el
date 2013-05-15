;;; flymake-jshint.el --- making flymake work with JSHint

;; Copyright (C) 2012 Wilfred Hughes <me@wilfred.me.uk>

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 15 May 2013
;; Version: 0.1
;; Package-Requires: ((flymake-easy "0.1"))

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

;; Run Emacs' elint on the current buffer, using an Emacs
;; subprocess. This helps catch missing imports, forward declared
;; macros, and other coding errors.


;;; Usage

;; Add to your Emacs config:

;; (require 'flymake-elisp)
;; (add-hook 'emacs-lisp-mode-hook 'flymake-elisp-load)

;; making sure that flymake-elisp.el is on your load-path.  If not,
;; also add to your config:

;; (add-to-list 'load-path "~/.emacs.d/path/to/flymake-elisp.el")

;;; Debugging

;; If flyamake-elisp isn't working for any reason, execute

;; M-x set-variable flymake-log-level <RET> 3

;; and you will see what is going wrong listed in the *Messages*
;; buffer.

;;; Alternatives

;; * https://github.com/lunaryorn/flycheck supports Elisp

;;; Code:

(require 'flymake-easy)

(defconst flymake-elisp-err-line-patterns
  '(("^\\(.+\\):\\([0-9]+\\):Warning: \\(.+\\)"
     nil 2 nil 3)
    ("^\\(.+\\):\\([0-9]+\\):Error: \\(.+\\)"
     nil 2 nil 3)))

(defun flymake-elisp-command (filename)
  "Construct a command that flymake can use to check elisp source for FILENAME."
  (list
   "emacs" "--no-site-file" "--no-site-lisp" "--batch" "--eval"
   ;; todo: pick up elpa packages (only if some flag is set)
   (format "(progn (package-initialize) (elint-file \"%s\"))" filename)))

;;;###autoload
(defun flymake-elisp-load ()
  "Configure flymake mode to lint the current buffer's Elisp."
  (interactive)
  (flymake-easy-load
   'flymake-elisp-command
   flymake-elisp-err-line-patterns
   'tempdir
   "el"))

(provide 'flymake-elisp)
;;; flymake-elisp.el ends here
