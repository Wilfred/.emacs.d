;;; flycheck-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flycheck-define-error-level flycheck-info flycheck-def-option-var
;;;;;;  flycheck-def-config-file-var global-flycheck-mode flycheck-mode)
;;;;;;  "flycheck" "flycheck.el" (21311 13885 715475 100000))
;;; Generated autoloads from flycheck.el

(defconst flycheck-mode-line-lighter " FlyC" "\
The standard lighter for flycheck mode.")

(autoload 'flycheck-mode "flycheck" "\
Minor mode for on-the-fly syntax checking.

When called interactively, toggle `flycheck-mode'.  With prefix
ARG, enable `flycheck-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `flycheck-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-mode'.
Otherwise behave as if called interactively.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

\\{flycheck-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-flycheck-mode nil "\
Non-nil if Global-Flycheck mode is enabled.
See the command `global-flycheck-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.")

(custom-autoload 'global-flycheck-mode "flycheck" nil)

(autoload 'global-flycheck-mode "flycheck" "\
Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global-Flycheck mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flycheck mode is enabled in all buffers where
`flycheck-mode-on-safe' would do it.
See `flycheck-mode' for more information on Flycheck mode.

\(fn &optional ARG)" t nil)

(autoload 'flycheck-def-config-file-var "flycheck" "\
Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable (see `defcustom`)
providing a configuration file for CHECKER.  The CHECKER argument
is used for documentation purposes only.  If given use FILE-NAME
as initial value.

The variable is declared with `defcustom', and declared
buffer-local.  CUSTOM-ARGS are forwarded to `defcustom'

Use this together with the `config-file' cell in syntax checker
commands.

\(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t)

(put 'flycheck-def-config-file-var 'lisp-indent-function '3)

(autoload 'flycheck-def-option-var "flycheck" "\
Define SYMBOL as option variable with INIT-VALUE for CHECKER.

INIT-VALUE is the initial value for the new variable.  DOCSTRING
is its docstring.

The variable is declared with `defcustom', and declared
buffer-local.  CUSTOM-ARGS are forwarded to `defcustom'.

Use this together with the `option' cell in syntax checker
commands.

\(fn SYMBOL INIT-VALUE CHECKER DOCSTRING &rest CUSTOM-ARGS)" nil t)

(put 'flycheck-def-option-var 'lisp-indent-function '3)

(put 'flycheck-def-option-var 'doc-string-elt '4)

(autoload 'flycheck-info "flycheck" "\
Open the Flycheck manual.

\(fn)" t nil)

(autoload 'flycheck-define-error-level "flycheck" "\
Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay properties' for more information about
     overlay categories.

`:fringe-bitmap BITMAP'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level.  See Info node `(elisp)Fringe
     Bitmaps' for more information about fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

\(fn LEVEL &rest PROPERTIES)" nil nil)

(put 'flycheck-define-error-level 'lisp-indent-function '1)

;;;***

;;;### (autoloads nil nil ("flycheck-pkg.el") (21311 13885 911768
;;;;;;  838000))

;;;***

(provide 'flycheck-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-autoloads.el ends here
