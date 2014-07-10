;;; keyfreq-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (keyfreq-autosave-mode keyfreq-mode) "keyfreq"
;;;;;;  "keyfreq.el" (21423 59471 49548 315000))
;;; Generated autoloads from keyfreq.el

(defvar keyfreq-mode nil "\
Non-nil if Keyfreq mode is enabled.
See the command `keyfreq-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keyfreq-mode'.")

(custom-autoload 'keyfreq-mode "keyfreq" nil)

(autoload 'keyfreq-mode "keyfreq" "\
Keyfreq mode records number of times each command was
called making it possible to access usage statistics through
various keyfreq-* functions.

\(fn &optional ARG)" t nil)

(defvar keyfreq-autosave-mode nil "\
Non-nil if Keyfreq-Autosave mode is enabled.
See the command `keyfreq-autosave-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keyfreq-autosave-mode'.")

(custom-autoload 'keyfreq-autosave-mode "keyfreq" nil)

(autoload 'keyfreq-autosave-mode "keyfreq" "\
Keyfreq Autosave mode automatically saves
`keyfreq-table' every `keyfreq-autosave-timeout' seconds
and when emacs is killed.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("keyfreq-pkg.el") (21423 59471 103091
;;;;;;  905000))

;;;***

(provide 'keyfreq-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; keyfreq-autoloads.el ends here
