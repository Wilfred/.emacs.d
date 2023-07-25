;;; just-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "just-mode" "just-mode.el" (0 0 0 0))
;;; Generated autoloads from just-mode.el

(autoload 'just-mode "just-mode" "\
Major mode for editing standard Justfiles.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("/[Jj]ustfile\\'" . just-mode))

(add-to-list 'auto-mode-alist '("\\.[Jj]ust\\(file\\)?\\'" . just-mode))

(register-definition-prefixes "just-mode" '("just-"))

;;;***

;;;### (autoloads nil nil ("just-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; just-mode-autoloads.el ends here
