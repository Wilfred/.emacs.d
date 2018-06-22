;;; mode-line-debug-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mode-line-debug" "mode-line-debug.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from mode-line-debug.el

(defvar mode-line-debug-mode nil "\
Non-nil if Mode-Line-Debug mode is enabled.
See the `mode-line-debug-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mode-line-debug-mode'.")

(custom-autoload 'mode-line-debug-mode "mode-line-debug" nil)

(autoload 'mode-line-debug-mode "mode-line-debug" "\
Mode to show the status of `debug-on-error' in the mode-line.

Depending on the state of `debug-on-error' this mode inserts a
different string into the mode-line before the list of active
modes.  The inserted character can be used to toggle the state of
`debug-on-error'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mode-line-debug" '("mode-line-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mode-line-debug-autoloads.el ends here
