;;; cider-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cider-connect cider-jack-in cider-version) "cider"
;;;;;;  "cider.el" (21423 59529 695352 907000))
;;; Generated autoloads from cider.el

(autoload 'cider-version "cider" "\
Display CIDER's version.

\(fn)" t nil)

(autoload 'cider-jack-in "cider" "\
Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(autoload 'cider-connect "cider" "\
Connect to an nREPL server identified by HOST and PORT.

\(fn HOST PORT)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") 'cider-jack-in) (define-key clojure-mode-map (kbd "C-c M-c") 'cider-connect)))

;;;***

;;;### (autoloads (cider-inspect) "cider-inspector" "cider-inspector.el"
;;;;;;  (21423 59529 548688 386000))
;;; Generated autoloads from cider-inspector.el

(autoload 'cider-inspect "cider-inspector" "\
Eval the string EXPRESSION and inspect the result.

\(fn EXPRESSION)" t nil)

;;;***

;;;### (autoloads (cider-macroexpand-all cider-macroexpand-1) "cider-macroexpansion"
;;;;;;  "cider-macroexpansion.el" (21423 59529 652020 208000))
;;; Generated autoloads from cider-macroexpansion.el

(autoload 'cider-macroexpand-1 "cider-macroexpansion" "\
Invoke 'macroexpand-1' on the expression preceding point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'.

\(fn &optional PREFIX)" t nil)

(autoload 'cider-macroexpand-all "cider-macroexpansion" "\
Invoke 'clojure.walk/macroexpand-all' on the expression preceding point.

\(fn)" t nil)

;;;***

;;;### (autoloads (cider-mode) "cider-mode" "cider-mode.el" (21423
;;;;;;  59529 805351 298000))
;;; Generated autoloads from cider-mode.el

(autoload 'cider-mode "cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cider-scratch) "cider-scratch" "cider-scratch.el"
;;;;;;  (21423 59529 572021 378000))
;;; Generated autoloads from cider-scratch.el

(autoload 'cider-scratch "cider-scratch" "\
Create a scratch buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (cider-selector) "cider-selector" "cider-selector.el"
;;;;;;  (21423 59529 518688 825000))
;;; Generated autoloads from cider-selector.el

(autoload 'cider-selector "cider-selector" "\
Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer.  The `?' character describes then
available methods.  OTHER-WINDOW provides an optional target.

See `def-cider-selector-method' for defining new methods.

\(fn &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads nil nil ("cider-client.el" "cider-doc.el" "cider-eldoc.el"
;;;;;;  "cider-interaction.el" "cider-pkg.el" "cider-repl.el" "cider-stacktrace.el"
;;;;;;  "cider-test.el" "cider-util.el" "nrepl-client.el") (21423
;;;;;;  59530 25815 165000))

;;;***

(provide 'cider-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cider-autoloads.el ends here
