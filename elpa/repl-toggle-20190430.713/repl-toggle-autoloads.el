;;; repl-toggle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "repl-toggle" "repl-toggle.el" (0 0 0 0))
;;; Generated autoloads from repl-toggle.el

(autoload 'repl-toggle-mode "repl-toggle" "\
A minor mode to allow uniform repl buffer switching.

\(fn &optional ARG)" t nil)

(autoload 'rtog/switch-to-shell-buffer "repl-toggle" "\
Make sure that `BUFFER-NAME' exists and is displayed.

Executes `SHELL-COMMAND', passing `SHELL-ARGS', if buffer
`BUFFER-NAME' doesn't exist.

\(fn BUFFER-NAME SHELL-COMMAND &optional SHELL-ARGS)" nil t)

(autoload 'rtog/add-repl "repl-toggle" "\
Associate MODE with REPL-CMD at runtime..

If in a buffer with `major-mode' MODE, execute REPL-CMD when
`rtog/toggle-repl' is called.

\(fn MODE REPL-CMD)" t nil)

(autoload 'rtog/toggle-repl "repl-toggle" "\
Switch to the repl asscociated with the current major mode.

If in a repl already switch back to the buffer we
came from.

If you provide PASSALONG? as a universal prefix with
\\[universal-argument], the current line or region is passed to
the repl buffer, using \\[universal-argument]
\\[universal-argument] the current function or definition is
passed, and finaly using
\\[universal-argument]\\[universal-argument]\\[universal-argument]
you can pass the whole current buffer.

Additional paramters passed will be IGNORED.

\(fn &optional PASSALONG\\=\\? &rest IGNORED)" t nil)

(autoload 'rtog/activate "repl-toggle" "\
Activate the repl-toggle minor mode.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "repl-toggle" '("repl-toggle-mode-map" "rtog/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; repl-toggle-autoloads.el ends here
