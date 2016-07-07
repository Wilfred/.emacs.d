;;; excorporate-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "excorporate" "excorporate.el" (22398 4293
;;;;;;  660534 249000))
;;; Generated autoloads from excorporate.el

(autoload 'excorporate "excorporate" "\
Start Excorporate.
Prompt for a mail address to use for autodiscovery, with an
initial suggestion of `user-mail-address'.  However, if
`excorporate-configuration' is non-nil, `excorporate' will use
that without prompting.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "excorporate-org" "excorporate-org.el" (22398
;;;;;;  4293 410506 370000))
;;; Generated autoloads from excorporate-org.el

(autoload 'exco-org-show-day "excorporate-org" "\
Show meetings for the date specified by MONTH DAY YEAR.

\(fn MONTH DAY YEAR)" nil nil)

;;;***

;;;### (autoloads nil nil ("excorporate-calendar.el" "excorporate-pkg.el")
;;;;;;  (22398 4293 796711 615000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; excorporate-autoloads.el ends here
