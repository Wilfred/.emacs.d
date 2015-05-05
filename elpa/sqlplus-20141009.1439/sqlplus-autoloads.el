;;; sqlplus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "sqlplus" "sqlplus.el" (21777 59848 75278 435000))
;;; Generated autoloads from sqlplus.el

(autoload 'sqlplus "sqlplus" "\
Create SQL*Plus process connected to Oracle according to
CONNECT-STRING, open (or create) input buffer with specified
name (do not create if INPUT-BUFFER-NAME is nil).
OUTPUT-BUFFER-FLAG has meanings: nil or SHOW-OUTPUT-BUFFER -
create output buffer and show it, DONT-SHOW-OUTPUT-BUFFER -
create output buffer but dont show it, DONT-CREATE-OUTPUT-BUFFER
- dont create output buffer

\(fn CONNECT-STRING &optional INPUT-BUFFER-NAME OUTPUT-BUFFER-FLAG)" t nil)

(autoload 'sqlplus-shutdown "sqlplus" "\
Kill input, output and process buffer for specified CONNECT-STRING.

\(fn CONNECT-STRING &optional DONT-KILL-INPUT-BUFFER)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sqlplus-autoloads.el ends here
