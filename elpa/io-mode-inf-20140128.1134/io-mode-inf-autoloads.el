;;; io-mode-inf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "io-mode-inf" "io-mode-inf.el" (22483 30472
;;;;;;  680365 397000))
;;; Generated autoloads from io-mode-inf.el

(autoload 'io-run-io "io-mode-inf" "\
Run a Io interpreter in an Emacs buffer

\(fn &optional CMD-LINE)" t nil)

(autoload 'io-switch-to-interpreter "io-mode-inf" "\
Switch to buffer containing the interpreter

\(fn)" t nil)

(autoload 'io-eval-region "io-mode-inf" "\
Send current region to io interpreter.

\(fn START END)" t nil)

(autoload 'io-eval-buffer "io-mode-inf" "\
Send whole buffer to Io interpreter.

\(fn)" t nil)

(autoload 'io-load-file "io-mode-inf" "\
Load a file in the Io interpreter.

\(fn FILE-NAME)" t nil)

(autoload 'io-quit-interpreter "io-mode-inf" "\
Quit io interpreter.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; io-mode-inf-autoloads.el ends here
