;;; ocamlformat-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ocamlformat" "ocamlformat.el" (0 0 0 0))
;;; Generated autoloads from ocamlformat.el

(autoload 'ocamlformat-before-save "ocamlformat" "\
Add this to .emacs to run ocamlformat on the current buffer when saving:

\(add-hook 'before-save-hook 'ocamlformat-before-save)." t nil)

(autoload 'ocamlformat-setup-indent "ocamlformat" nil t nil)

(autoload 'ocamlformat-caml-mode-setup "ocamlformat" nil nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ocamlformat" '("ocamlformat")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ocamlformat-autoloads.el ends here
