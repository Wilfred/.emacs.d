;;; crux-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "crux" "crux.el" (22398 4315 129594 819000))
;;; Generated autoloads from crux.el

(defvar crux-reopen-as-root-mode nil "\
Non-nil if Crux-Reopen-As-Root mode is enabled.
See the command `crux-reopen-as-root-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `crux-reopen-as-root-mode'.")

(custom-autoload 'crux-reopen-as-root-mode "crux" nil)

(autoload 'crux-reopen-as-root-mode "crux" "\
Automatically reopen files as root if we can't write to them
as the current user.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; crux-autoloads.el ends here
