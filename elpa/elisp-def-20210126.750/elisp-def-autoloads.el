;;; elisp-def-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elisp-def" "elisp-def.el" (0 0 0 0))
;;; Generated autoloads from elisp-def.el

(autoload 'elisp-def "elisp-def" "\
Go to the definition of the symbol at point." t nil)

(autoload 'elisp-def-mode "elisp-def" "\
Minor mode for finding definitions with `elisp-def'.

If called interactively, enable Elisp-Def mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{elisp-def-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-def" '("elisp-def-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elisp-def-autoloads.el ends here
