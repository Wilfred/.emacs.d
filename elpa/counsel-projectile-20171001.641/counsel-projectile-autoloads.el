;;; counsel-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "counsel-projectile" "counsel-projectile.el"
;;;;;;  (23014 16745 568373 104000))
;;; Generated autoloads from counsel-projectile.el

(autoload 'counsel-projectile-find-file "counsel-projectile" "\
Jump to a project's file using completion.

Replacement for `projectile-find-file'.  With a prefix ARG
invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile-find-dir "counsel-projectile" "\
Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile-switch-to-buffer "counsel-projectile" "\
Switch to a project buffer.

\(fn)" t nil)

(autoload 'counsel-projectile-ag "counsel-projectile" "\
Run an ag search in the project.

\(fn &optional OPTIONS)" t nil)

(autoload 'counsel-projectile-rg "counsel-projectile" "\
Run an rg search in the project.

\(fn &optional OPTIONS)" t nil)

(autoload 'counsel-projectile-switch-project "counsel-projectile" "\
Switch to a project we have visited before.

Invokes the command referenced by
`projectile-switch-project-action' on switch.

\(fn)" t nil)

(autoload 'counsel-projectile "counsel-projectile" "\
Use projectile with Ivy instead of ido.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(eval-after-load 'projectile '(progn (define-key projectile-command-map (kbd "SPC") #'counsel-projectile)))

(autoload 'counsel-projectile-on "counsel-projectile" "\
Turn on counsel-projectile key bindings.

\(fn)" t nil)

(autoload 'counsel-projectile-off "counsel-projectile" "\
Turn off counsel-projectile key bindings.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; counsel-projectile-autoloads.el ends here
