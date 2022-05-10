;;; deadgrep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "deadgrep" "deadgrep.el" (0 0 0 0))
;;; Generated autoloads from deadgrep.el

(autoload 'deadgrep "deadgrep" "\
Start a ripgrep search for SEARCH-TERM in DIRECTORY.

If not provided, DIR defaults to the directory as determined by
`deadgrep-project-root-function'.

See also `deadgrep-project-root-overrides'.

If called with a prefix argument, create the results buffer but
don't actually start the search.

\(fn SEARCH-TERM &optional DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "deadgrep" '("deadgrep-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; deadgrep-autoloads.el ends here
