;;; firestarter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "firestarter" "firestarter.el" (22697 45768
;;;;;;  997846 376000))
;;; Generated autoloads from firestarter.el

(defvar firestarter-mode nil "\
Non-nil if Firestarter mode is enabled.
See the `firestarter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `firestarter-mode'.")

(custom-autoload 'firestarter-mode "firestarter" nil)

(autoload 'firestarter-mode "firestarter" "\
Toggle `firestarter-mode'.
When activated, run a command as specified in the buffer-local
`firestarter' variable on every file save.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; firestarter-autoloads.el ends here
