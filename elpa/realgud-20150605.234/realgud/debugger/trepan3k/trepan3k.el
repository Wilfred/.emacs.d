;;; Copyright (C) 2010-2014 Rocky Bernstein <rocky@gnu.org>
;;  `trepan3k' Main interface to trepan3k via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:trepan3k-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:trepan3k nil
  "The realgud interface to the Python debugger, trepan3k"
  :group 'realgud
  :group 'python
  :version "24.1")

(declare-function trepan3k-query-cmdline  'realgud:trepan3k-core)
(declare-function trepan3k-parse-cmd-args 'realgud:trepan3k-core)
(declare-function realgud:run-debugger    'realgud:run)

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:trepan3k-command-name
  ;;"trepan3k --emacs 3"
  "trepan3k"
  "File name for executing the Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:trepan3k)

(declare-function trepan3k-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:trepan3k (&optional opt-cmd-line no-reset)
  "Invoke the trepan3k Python debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepan2-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "trepan3k"
			'trepan3k-query-cmdline
			'trepan3k-parse-cmd-args
			'realgud:trepan3k-minibuffer-history
			opt-cmd-line no-reset)
  )


(defalias 'trepan3k 'realgud:trepan3k)

(provide-me "realgud-")
