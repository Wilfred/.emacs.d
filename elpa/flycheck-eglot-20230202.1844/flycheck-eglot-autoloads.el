;;; flycheck-eglot-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-eglot" "flycheck-eglot.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from flycheck-eglot.el

(autoload 'flycheck-eglot-mode "flycheck-eglot" "\
Minor mode for using Flycheck with Eglot.

This is a minor mode.  If called interactively, toggle the
`Flycheck-Eglot mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `flycheck-eglot-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-flycheck-eglot-mode 'globalized-minor-mode t)

(defvar global-flycheck-eglot-mode nil "\
Non-nil if Global Flycheck-Eglot mode is enabled.
See the `global-flycheck-eglot-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-eglot-mode'.")

(custom-autoload 'global-flycheck-eglot-mode "flycheck-eglot" nil)

(autoload 'global-flycheck-eglot-mode "flycheck-eglot" "\
Toggle Flycheck-Eglot mode in all buffers.
With prefix ARG, enable Global Flycheck-Eglot mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Flycheck-Eglot mode is enabled in all buffers where `(lambda nil (when
\(flycheck-eglot--eglot-available-p) (flycheck-eglot-mode 1)))' would
do it.

See `flycheck-eglot-mode' for more information on Flycheck-Eglot
mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "flycheck-eglot" '("flycheck-eglot-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-eglot-autoloads.el ends here
