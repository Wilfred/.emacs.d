;;; nodejs-repl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "nodejs-repl" "nodejs-repl.el" (22697 45686
;;;;;;  122221 548000))
;;; Generated autoloads from nodejs-repl.el

(autoload 'nodejs-repl-send-region "nodejs-repl" "\
Send the current region to the `nodejs-repl-process'

\(fn START END)" t nil)

(autoload 'nodejs-repl-send-buffer "nodejs-repl" "\
Send the current buffer to the `nodejs-repl-process'

\(fn)" t nil)

(autoload 'nodejs-repl-load-file "nodejs-repl" "\
Load the file to the `nodejs-repl-process'

\(fn FILE)" t nil)

(autoload 'nodejs-repl-send-last-sexp "nodejs-repl" "\
Send the expression before point to the `nodejs-repl-process'

\(fn)" t nil)

(autoload 'nodejs-repl-switch-to-repl "nodejs-repl" "\
If there is a `nodejs-repl-process' running switch to it,
otherwise spawn one.

\(fn)" t nil)

(autoload 'nodejs-repl "nodejs-repl" "\
Run Node.js REPL.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nodejs-repl-autoloads.el ends here
