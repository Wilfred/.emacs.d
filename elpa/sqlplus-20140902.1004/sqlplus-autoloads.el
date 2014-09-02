;;; sqlplus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (sqlplus-shutdown sqlplus) "sqlplus" "sqlplus.el"
;;;;;;  (21510 17236 997271 441000))
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

;;;### (autoloads nil nil ("sqlplus-pkg.el") (21510 17237 145625
;;;;;;  583000))

;;;***

(provide 'sqlplus-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sqlplus-autoloads.el ends here
