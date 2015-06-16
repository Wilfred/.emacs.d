;;; excorporate-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "excorporate" "excorporate.el" (21887 65285
;;;;;;  178033 864000))
;;; Generated autoloads from excorporate.el

(autoload 'excorporate "excorporate" "\
Start Excorporate.
Prompt for a mail address to use for autodiscovery, with an
initial suggestion of `user-mail-address'.  However, if
`excorporate-configuration' is non-nil, `excorporate' will use
that without prompting.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "excorporate-calendar" "excorporate-calendar.el"
;;;;;;  (21887 65285 172033 864000))
;;; Generated autoloads from excorporate-calendar.el

(require 'calendar)

(autoload 'exco-calendar-show-day "excorporate-calendar" "\
Show meetings for the selected date.

\(fn)" t nil)

(define-key calendar-mode-map "e" #'exco-calendar-show-day)

;;;***

;;;### (autoloads nil nil ("exco-fsm.el" "exco-ntlm.el" "exco-soap-client.el"
;;;;;;  "exco-url-http-24.1.el" "exco-url-http-24.2.el" "exco-url-http-24.3.el"
;;;;;;  "exco-url-http-24.4.el" "exco-url-http-25.0.el" "exco-url-http-ntlm.el"
;;;;;;  "excorporate-calfw.el" "excorporate-pkg.el") (21887 65285
;;;;;;  550413 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; excorporate-autoloads.el ends here
