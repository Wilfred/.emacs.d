;;; realgud-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "realgud" "realgud.el" (0 0 0 0))
;;; Generated autoloads from realgud.el

(defconst realgud--recursive-autoloads-file-name "realgud-recursive-autoloads.el" "\
Where to store autoloads for subdirectory contents.")

(defconst realgud--recursive-autoloads-base-directory (file-name-directory (if load-in-progress load-file-name buffer-file-name)))

(with-demoted-errors "Error in RealGUD's autoloads: %s" (load (expand-file-name realgud--recursive-autoloads-file-name realgud--recursive-autoloads-base-directory) t t))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "realgud" '("realgud--rebuild-recursive-autoloads")))

;;;***

;;;### (autoloads nil nil ("realgud-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; realgud-autoloads.el ends here
