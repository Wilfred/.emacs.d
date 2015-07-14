;; Copyright (C) 2015 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

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
;;; Regular expressions for nodejs Javascript debugger.
;;; Copyright (C) 2014-2015 Rocky Bernstein <rocky@gnu.org>

;;  `nodejs' Main interface to nodejs debugger via Emacs
(require 'list-utils)
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:nodejs-")
(require-relative-list '("../../lang/js") "realgud-lang-")

(declare-function realgud:js-remove-ansi-schmutz 'realgud-lang-js)
(declare-function realgud:run-debugger 'realgud:run)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:nodejs nil
  "The realgud interface to the nodejs debugger"
  :group 'realgud
  :version "24.1")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:nodejs-command-name
  "node debug"
  "File name for executing the Javascript debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:nodejs)

;; -------------------------------------------------------------------
;; The end.
;;

(declare-function nodejs-track-mode     'realgud-nodejs-track-mode)
(declare-function nodejs-query-cmdline  'realgud:nodejs-core)
(declare-function nodejs-parse-cmd-args 'realgud:nodejs-core)
(declare-function realgud:run-process   'realgud:run)

;;;###autoload
(defun realgud:nodejs (&optional opt-cmd-line no-reset)
  "Invoke the nodejs shell debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run nodejs.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `nodejs-parse-cmd-args' and path elements found by that
are expanded using `realgud:expand-file-name-if-exists'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)
  (let ((cmd-buf
	 (realgud:run-debugger "nodejs"
			       'nodejs-query-cmdline 'nodejs-parse-cmd-args
			       'realgud:nodejs-minibuffer-history
			       opt-cmd-line no-reset)))
    (if cmd-buf
	(with-current-buffer cmd-buf
	  ;; FIXME should allow customization whether to do or not
	  ;; and also only do if hook is not already there.
	  (realgud:js-remove-ansi-schmutz)
	  )
      )))

;; There is already a nodejs command in `nodejs-repl'.
;; (defalias 'nodejs 'realgud:nodejs)

(provide-me "realgud-")
