;;; nameless-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nameless" "nameless.el" (0 0 0 0))
;;; Generated autoloads from nameless.el

(autoload 'nameless-mode "nameless" "\
Toggle Nameless mode on or off.

This is a minor mode.  If called interactively, toggle the
`Nameless mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `nameless-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{nameless-mode-map}

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'nameless-mode-from-hook 'nameless-mode "1.0.0")

(register-definition-prefixes "nameless" '("nameless-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nameless-autoloads.el ends here
