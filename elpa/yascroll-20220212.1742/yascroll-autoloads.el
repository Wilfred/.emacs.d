;;; yascroll-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yascroll" "yascroll.el" (0 0 0 0))
;;; Generated autoloads from yascroll.el

(autoload 'yascroll:show-scroll-bar "yascroll" "\
Default key to show all scroll bars." t nil)

(autoload 'yascroll:hide-scroll-bar "yascroll" "\
Hide scroll bar of BUFFER." t nil)

(autoload 'yascroll-bar-mode "yascroll" "\
Yet Another Scroll Bar Mode.

This is a minor mode.  If called interactively, toggle the
`Yascroll-Bar mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `yascroll-bar-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-yascroll-bar-mode 'globalized-minor-mode t)

(defvar global-yascroll-bar-mode nil "\
Non-nil if Global Yascroll-Bar mode is enabled.
See the `global-yascroll-bar-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-yascroll-bar-mode'.")

(custom-autoload 'global-yascroll-bar-mode "yascroll" nil)

(autoload 'global-yascroll-bar-mode "yascroll" "\
Toggle Yascroll-Bar mode in all buffers.
With prefix ARG, enable Global Yascroll-Bar mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Yascroll-Bar mode is enabled in all buffers where `yascroll:turn-on'
would do it.

See `yascroll-bar-mode' for more information on Yascroll-Bar mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "yascroll" '("yascroll:"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yascroll-autoloads.el ends here
