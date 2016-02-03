;;; org-password-manager-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-password-manager" "org-password-manager.el"
;;;;;;  (22161 33553 448218 686000))
;;; Generated autoloads from org-password-manager.el

(autoload 'org-password-manager-get-username "org-password-manager" "\
Get username.

If ASK-FOR-INPUT? is t, will ask for input even if point is on a
heading that contains the username property.

\(fn &optional ASK-FOR-INPUT\\?)" t nil)

(autoload 'org-password-manager-get-password "org-password-manager" "\
Get password.

If ASK-FOR-INPUT? is t, will ask for input even if point is on a
heading that contains the password property.

\(fn &optional ASK-FOR-INPUT\\?)" t nil)

(autoload 'org-password-manager-generate-password "org-password-manager" "\
Generate password.

If EDIT-PWGEN-STRING? is t, let the user edit the pwgen command
line before running it.

\(fn &optional EDIT-PWGEN-STRING\\?)" t nil)

(autoload 'org-password-manager-key-bindings "org-password-manager" "\
Binds keys for org-password-manager.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-password-manager-autoloads.el ends here
