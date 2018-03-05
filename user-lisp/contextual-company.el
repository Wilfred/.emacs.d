;; Ideas:

;; (defcustom racer-complete-in-comments
;;   nil
;;   "foo"
;;   :type 'boolean
;;   :group 'racer)
;;
;; Could complete group value, simple type values, and keys to
;; defcustom.

;; Completing functions and variables based on context.
;;
;; Auto insert space when completing (foo ) if foo takes multiple
;; args.
;;
;; Completion inside emacs style `backticks' inside docstrings and
;; comments.
;;
;; Even when we can't offer an exact set of options, offer the current
;; package as a prefix.
;;
;; Consider only offering autoloaded/required functions in the current
;; file.
;;
;; Offer sensible completion inside (declare ...) forms and
;; (interactive ...) forms.
;; 
;; Consider expanding macros to work out what's bound.
;;
;; Offer &rest and &optional in parameter lists.
;;
;; Offer bound variables inside let/condition-case.
;;
;; (require ...) and (provide ...) should be trivially completable.
;;
;; define-key and global-set-key: offer keymaps and interactive
;; commands.
;;
;; Autocomplete keywords inside rx.
;;
;; Complete () to a function call, unless we're in quotes.
;;
;; Show num args (dropdown) and docstring (minibuffer) when completing.
;;
;; Sort prefix first, but allow substring, so we offer
;; `map-char-table' for input "char-table".
;;
;; Write a crude type inference (look at integer and list assignments)
;; and prioritise arguments to e.g. `nth' (based on its docstring
;; saying arg type) that are known to be integer/lists.
;;
;; Don't offer completion inside quoted forms, except `(foo,
;; bar). This again requires macro expansion.
;;
;; Don't offer completion for the first argument of var-val pairs in
;; `let', as they're usually new vars.
;;
;; Offer completion of FOO in a docstring when `foo' is an argument to
;; a function.
;;
;; Offer completion of known signals when defining handlers in
;; `condition-case'.
;;
;; Offer completion of literals for argument of a known type,
;; particularly booleans. Types may be inferred from parameter names
;; in docstrings.
;;
;; Rank functions/variables more highly if they've been used in a
;; recent function body. See page 22 of
;; https://users.dcc.uchile.cl/~rrobbes/p/JASE-completion.pdf which
;; measures the effectiveness of this approach. This would require
;; advising `eval-last-sexp', `edebug-eval-defun', and `eval-buffer'.
;;
;; Also see if a user has recently viewed docs for a symbol.
;;
;; `defadvice' should offer completion for existing functions as the
;; first argument.

(require 'dash)
(require 's)
(require 'elisp-def)

(defvar elisp-complete--recent-syms nil)
(defvar elisp-complete--history-size 1000)

(defun elisp-complete--global-syms (form)
  "Return all the globally bound symbol references in FORM."
  ;; TODO: we can't macro expand, because we want to preserve macro
  ;; references. However, this erroneously offers binding variable
  ;; names (probably harmless) and function (due to #'foo, which will
  ;; lead to noise).
  (let* ((atoms (-flatten form))
         (syms (-filter #'symbolp atoms))
         ;; Ignore quote and function, because those probably result
         ;; from forms using 'foo and #'foo so it's unlikely they've
         ;; been used by the user.
         (syms (--filter (not (memq it '(quote function))) syms))
         (bound-syms (--filter (or (boundp it) (fboundp it)) syms)))
    (-uniq bound-syms)))

(defun elisp-complete--add-to-recent (form)
  (let* ((used-syms (elisp-complete--global-syms form))
         (syms (-uniq (append used-syms elisp-complete--recent-syms))))
    (setq
     elisp-complete--recent-syms
     (-take elisp-complete--history-size syms))))

(defadvice edebug-eval-defun (after elisp-complete--record-form activate)
  (let ((form (edebug-read-top-level-form)))
    (elisp-complete--add-to-recent form)))

(defadvice eval-last-sexp (after elisp-complete--record-last-form activate)
  (let ((form (elisp--preceding-sexp)))
    (elisp-complete--add-to-recent form)))

;; TODO: Play with `company-diag'.

;; not relevant, this is for idle completion.
(setq company-minimum-prefix-length 3)
(setq-local company-minimum-prefix-length 3)

(defun elisp-complete--locals-at-point ()
  (catch 'done
    ;; Macro expand the source around point and see what bindings are
    ;; present.
    (-let* (((form-start form-end) (elisp-def--enclosing-form
                                    (elisp-def--syntax-depth)))
            (placeholder (elisp-def--fresh-placeholder))
            (src (elisp-def--source-with-placeholder form-start form-end placeholder))
            (form (condition-case nil
                      (read src)
                    (end-of-file nil)))
            (expanded-form (macroexpand-all form)))
      ;; TODO: this includes generated symbols produced by dash macro expansion.
      (elisp-def--bound-syms expanded-form placeholder))))

(defun elisp-complete--candidates (prefix)
  (let* ((sym-names (-map #'symbol-name elisp-complete--recent-syms))
         (matching-names
          (--filter (s-starts-with-p prefix it) sym-names))
         ;; TODO: elisp-complete--locals-at-point isn't offering these
         ;; let-bound vars for some reason.
         (local-names
          (-map #'symbol-name (elisp-complete--locals-at-point)))
         (matching-locals
          (--filter (s-starts-with-p prefix it) local-names)))
    (append
     matching-locals
     matching-names)))

;; TODO: consider using `completion-in-region' like
;; `anaconda-mode-complete-callback' rather than using company.
(defun elisp-complete (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'elisp-complete))
    (`prefix (company-grab-symbol))
    (`candidates (elisp-complete--candidates arg))
    (`meta (format "This value is named %s" arg))))
