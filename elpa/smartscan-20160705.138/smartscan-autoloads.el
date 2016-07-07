;;; smartscan-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "smartscan" "smartscan.el" (22398 3706 501729
;;;;;;  900000))
;;; Generated autoloads from smartscan.el

(autoload 'smartscan-symbol-go-forward "smartscan" "\
Jumps forward to the next symbol at point

\(fn)" t nil)

(autoload 'smartscan-symbol-go-backward "smartscan" "\
Jumps backward to the previous symbol at point

\(fn)" t nil)

(autoload 'smartscan-symbol-replace "smartscan" "\
Replaces the symbol at point with another string in the entire buffer.

With C-u the scope is limited to the current defun, as defined by
`narrow-to-defun'.

This function uses `search-forward' and `replace-match' to do the
actual work.

\(fn ARG)" t nil)

(autoload 'smartscan-mode "smartscan" "\
Jumps between other symbols found at point.

When Smart Scan mode is enabled, you can jump between the all the
symbols in your current buffer that point is on.

You can customize Smart Scan by editing
`smartscan-use-extended-syntax' and `smartscan-symbol-selector'.

Key bindings:
\\{smartscan-map}

\(fn &optional ARG)" t nil)

(defvar global-smartscan-mode nil "\
Non-nil if Global-Smartscan mode is enabled.
See the command `global-smartscan-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-smartscan-mode'.")

(custom-autoload 'global-smartscan-mode "smartscan" nil)

(autoload 'global-smartscan-mode "smartscan" "\
Toggle Smartscan mode in all buffers.
With prefix ARG, enable Global-Smartscan mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartscan mode is enabled in all buffers where
`smartscan-mode-turn-on' would do it.
See `smartscan-mode' for more information on Smartscan mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("smartscan-pkg.el") (22398 3706 542744
;;;;;;  764000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; smartscan-autoloads.el ends here
