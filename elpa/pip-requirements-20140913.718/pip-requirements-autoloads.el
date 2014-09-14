;;; pip-requirements-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pip-requirements-mode) "pip-requirements" "pip-requirements.el"
;;;;;;  (21525 35180 777492 478000))
;;; Generated autoloads from pip-requirements.el

(add-to-list 'auto-mode-alist `(,(rx ".pip" string-end) . pip-requirements-mode))

(add-to-list 'auto-mode-alist `(,(rx "requirements" (zero-or-more anything) ".txt" string-end) . pip-requirements-mode))

(autoload 'pip-requirements-mode "pip-requirements" "\
Major mode for editing pip requirements files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pip-requirements-pkg.el") (21525 35180
;;;;;;  872430 72000))

;;;***

(provide 'pip-requirements-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pip-requirements-autoloads.el ends here
