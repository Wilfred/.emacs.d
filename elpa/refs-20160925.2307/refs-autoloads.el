;;; refs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "refs" "refs.el" (22504 37500 578031 445000))
;;; Generated autoloads from refs.el

(autoload 'refs-function "refs" "\
Display all the references to function SYMBOL, in all loaded
elisp files.

\(fn SYMBOL)" t nil)

(autoload 'refs-macro "refs" "\
Display all the references to macro SYMBOL, in all loaded
elisp files.

\(fn SYMBOL)" t nil)

(autoload 'refs-special "refs" "\
Display all the references to special form SYMBOL, in all loaded
elisp files.

\(fn SYMBOL)" t nil)

(autoload 'refs-variable "refs" "\
Display all the references to variable SYMBOL, in all loaded
elisp files.

\(fn SYMBOL)" t nil)

(autoload 'refs-symbol "refs" "\
Display all the references to SYMBOL in all loaded elisp files.

\(fn SYMBOL)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; refs-autoloads.el ends here
