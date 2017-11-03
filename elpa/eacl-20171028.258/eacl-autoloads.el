;;; eacl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "eacl" "eacl.el" (23036 18474 559099 993000))
;;; Generated autoloads from eacl.el

(autoload 'eacl-get-project-root "eacl" "\
Get project root.

\(fn)" nil nil)

(autoload 'eacl-current-line "eacl" "\
Current line.

\(fn)" nil nil)

(autoload 'eacl-get-keyword "eacl" "\
Get trimmed keyword from CUR-LINE.

\(fn CUR-LINE)" nil nil)

(autoload 'eacl-complete-multi-lines-internal "eacl" "\
Complete multi-lines.  REGEX is used to match the lines.

\(fn REGEX)" nil nil)

(autoload 'eacl-complete-line "eacl" "\
Complete line by grepping project.

\(fn)" t nil)

(autoload 'eacl-complete-statement "eacl" "\
Complete statement which ends with \";\" by grepping project.

\(fn)" t nil)

(autoload 'eacl-complete-snippet "eacl" "\
Complete snippet which ends with \"}\" by grepping in project.

\(fn)" t nil)

(autoload 'eacl-complete-tag "eacl" "\
Complete snippet which ends with \">\" by grepping in project.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; eacl-autoloads.el ends here
