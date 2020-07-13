;;; suggest-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "suggest" "suggest.el" (0 0 0 0))
;;; Generated autoloads from suggest.el

(autoload 'suggest "suggest" "\
Open a Suggest buffer that provides suggestions for the inputs
and outputs given.

\(fn)" t nil)

(autoload 'suggest-update "suggest" "\
Update the suggestions according to the latest inputs/output given.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "suggest" '("suggest-")))

;;;***

;;;### (autoloads nil "suggest-bench" "suggest-bench.el" (0 0 0 0))
;;; Generated autoloads from suggest-bench.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "suggest-bench" '("suggest-")))

;;;***

;;;### (autoloads nil nil ("suggest-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; suggest-autoloads.el ends here
