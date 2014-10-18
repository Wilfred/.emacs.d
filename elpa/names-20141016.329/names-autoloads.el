;;; names-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (define-namespace) "names" "names.el" (21569 43842
;;;;;;  75266 275000))
;;; Generated autoloads from names.el

(autoload 'define-namespace "names" "\
Inside the namespace NAME, execute BODY.
NAME can be any symbol (not quoted), but it's highly recommended
to use some form of separator (such as :, /, or -).

This has two main effects:

1. Any definitions inside BODY will have NAME prepended to the
symbol given. Ex:
    (define-namespace foo:
    (defvar bar 1 \"docs\")
    )
expands to
    (defvar foo:bar 1 \"docs\")


2. Any function calls and variable names get NAME prepended to
them if possible. Ex:
    (define-namespace foo:
    (message \"%s\" my-var)
    )
expands to
    (foo:message \"%s\" foo:my-var)
but only if `foo:message' has a function definition. Similarly,
`my-var' becomes `foo:my-var', but only if `foo:my-var' has
a variable definition.

If `foo:message' is not a defined function, the above would
expand instead to
    (message \"%s\" foo:my-var)

===============================

AUTOLOAD

In order for `define-namespace' to work with ;;;###autoload
comments just replace all instances of ;;;###autoload inside your
`define-namespace' with `:autoload', and then add an ;;;###autoload
comment just above your `define-namespace'.

===============================

KEYWORDS

Immediately after NAME you may add keywords which customize the
behaviour of `define-namespace'. For a description of these keywords, see
the manual on
http://github.com/Bruce-Connor/names

\(fn NAME [KEYWORDS] BODY)" nil t)

(put 'define-namespace 'lisp-indent-function '(lambda (&rest x) 0))

(defadvice make-autoload (before names-before-make-autoload-advice (form file &optional expansion) activate) "\
Make sure `make-autoload' understands `define-namespace'.
Use a letbind to indicate to `define-namespace' that we're generating autoloads." (let ((names--inside-make-autoload t) space) (when (eq (car-safe form) (quote define-namespace)) (setq space (macroexpand form)) (ad-set-arg 0 space) (ad-set-arg 2 (quote expansion)))))

;;;***

;;;### (autoloads nil nil ("names-dev.el" "names-pkg.el") (21569
;;;;;;  43842 171769 892000))

;;;***

(provide 'names-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; names-autoloads.el ends here
