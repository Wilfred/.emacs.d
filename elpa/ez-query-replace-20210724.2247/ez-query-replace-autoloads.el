;;; ez-query-replace-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ez-query-replace" "ez-query-replace.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ez-query-replace.el

(autoload 'ez-query-replace "ez-query-replace" "\
Replace occurrences of FROM-STRING with TO-STRING, defaulting
to the symbol at point." t nil)

(autoload 'ez-query-replace-repeat "ez-query-replace" "\
Run `ez-query-replace' with an old FROM and TO value." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ez-query-replace" '("ez-query-replace/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ez-query-replace-autoloads.el ends here
