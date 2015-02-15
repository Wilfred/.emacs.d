;;; brainfuck-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "brainfuck-mode" "brainfuck-mode.el" (21719
;;;;;;  52057 446486 90000))
;;; Generated autoloads from brainfuck-mode.el

(autoload 'brainfuck-mode "brainfuck-mode" "\
Major mode for brainfuck

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.bf\\'" . brainfuck-mode))

(langdoc-define-help-mode bf-help "Major mode for brainfuck help" "*Brainfuck Help*" 'bf-help-sym-called-at-point '(">" "<" "+" "-" "." "," "[" "]") 'bf-help-lookup-doc "`\\([^']+\\)'" (lambda (a b) b) (lambda (a b) b) "`" "'")

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; brainfuck-mode-autoloads.el ends here
