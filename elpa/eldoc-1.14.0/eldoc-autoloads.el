;;; eldoc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eldoc" "eldoc.el" (0 0 0 0))
;;; Generated autoloads from eldoc.el

(defvar eldoc-minor-mode-string (purecopy " ElDoc") "\
String to display in mode line when ElDoc Mode is enabled; nil for none.")

(custom-autoload 'eldoc-minor-mode-string "eldoc" t)

(autoload 'eldoc-mode "eldoc" "\
Toggle echo area display of Lisp objects at point (ElDoc mode).

This is a minor mode.  If called interactively, toggle the `Eldoc
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `eldoc-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

ElDoc mode is a buffer-local minor mode.  When enabled, the echo
area displays information about a function or variable in the
text where point is.  If point is on a documented variable, it
displays the first line of that variable's doc string.  Otherwise
it displays the argument list of the function called in the
expression point is on.

\(fn &optional ARG)" t nil)

(put 'global-eldoc-mode 'globalized-minor-mode t)

(defcustom global-eldoc-mode t "\
Non-nil if Global Eldoc mode is enabled.
See the `global-eldoc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-eldoc-mode'." :set #'custom-set-minor-mode :initialize 'custom-initialize-delay :type 'boolean)

(custom-autoload 'global-eldoc-mode "eldoc" nil)

(autoload 'global-eldoc-mode "eldoc" "\
Toggle Eldoc mode in all buffers.
With prefix ARG, enable Global Eldoc mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Eldoc mode is enabled in all buffers where `turn-on-eldoc-mode' would
do it.

See `eldoc-mode' for more information on Eldoc mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-eldoc-mode "eldoc" "\
Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-strategy' for more detail." nil nil)

(register-definition-prefixes "eldoc" '("eldoc"))

;;;***

;;;### (autoloads nil nil ("eldoc-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eldoc-autoloads.el ends here
