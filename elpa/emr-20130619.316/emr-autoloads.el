;;; emr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emr-initialize emr-show-refactor-menu emr-declare-command
;;;;;;  emr-reporting-buffer-changes emr-collapse-vertical-whitespace
;;;;;;  emr-insert-above-defun emr-line-matches\? emr-blank-line\?
;;;;;;  emr-line-str emr-blank\? emr-looking-at-comment\? emr-looking-at-string\?
;;;;;;  emr-move-above-defun) "emr" "emr.el" (21258 39093 848152
;;;;;;  718000))
;;; Generated autoloads from emr.el

(autoload 'emr-move-above-defun "emr" "\
Move to the start of the current defun.
If the defun is preceded by comments, move above them.

\(fn)" t nil)

(autoload 'emr-looking-at-string\? "emr" "\
Return non-nil if point is inside a string.

\(fn)" nil nil)

(autoload 'emr-looking-at-comment\? "emr" "\
Non-nil if point is on a comment.

\(fn)" nil nil)

(autoload 'emr-blank\? "emr" "\
Non-nil if STR is null, empty or whitespace-only.

\(fn STR)" nil nil)

(autoload 'emr-line-str "emr" "\
Return the contents of the current line.

\(fn)" nil nil)

(autoload 'emr-blank-line\? "emr" "\
Non-nil if POINT is on a blank line.

\(fn &optional (point (point)))" nil nil)

(autoload 'emr-line-matches\? "emr" "\
Non-nil if POINT is on a line that matches REGEX.

\(fn REGEX &optional (point (point)))" nil nil)

(autoload 'emr-insert-above-defun "emr" "\
Insert and indent STR above the current top level form.
Return the position of the end of STR.

\(fn STR)" nil nil)

(autoload 'emr-collapse-vertical-whitespace "emr" "\
Collapse blank lines around point.
Ensure there are at most `emr-lines-between-toplevel-forms' blanks.

\(fn)" nil nil)

(autoload 'emr-reporting-buffer-changes "emr" "\
Perform a refactoring action and show a brief diff.
* DESCRIPTION describes the overall action, and is shown to the user.
* BODY forms perform the refactor action.

\(fn DESCRIPTION &rest BODY)" nil t)

(put 'emr-reporting-buffer-changes 'lisp-indent-function '1)

(autoload 'emr-declare-command "emr" "\
Define a refactoring command.

* FUNCTION is the refactoring command to perform. It should be
  either the name of a refactoring command or a
  lambda-expression.

* MODES is a symbol or list of symbols of the modes in which this
  command will be available. This will also enable the command
  for derived modes.

* TITLE is the name of the command that will be displayed in the
  popup menu.

* PREDICATE is a condition that must be satisfied to display this
  item. It should be a lambda-expression or function name.

* DESCRIPTION is shown to the left of the title in the popup
  menu.

\(fn FUNCTION &key MODES TITLE DESCRIPTION PREDICATE)" nil (quote macro))

(autoload 'emr-show-refactor-menu "emr" "\
Show the refactor menu at point.

\(fn)" t nil)

(autoload 'emr-initialize "emr" "\
Activate language support for EMR.

\(fn)" nil nil)

;;;***

;;;### (autoloads (emr-c-initialize emr-c-mode emr-c-insert-include
;;;;;;  emr-c-tidy-includes) "emr-c" "emr-c.el" (21258 39093 801488
;;;;;;  147000))
;;; Generated autoloads from emr-c.el

(autoload 'emr-c-tidy-includes "emr-c" "\
Collate and reorder include directives in the current buffer.
Library and project includes are kept separate.

\(fn)" t nil)

(autoload 'emr-c-insert-include "emr-c" "\
Insert an include for HEADER and tidy the includes in the buffer.

\(fn HEADER)" t nil)

(defvar emr-c-mode-map (let ((km (make-sparse-keymap))) (define-key km (kbd "C-c i") 'emr-c-insert-include) km) "\
Key map for `emr-c-mode'.")

(autoload 'emr-c-mode "emr-c" "\
A minor-mode for C that makes extra key bidings available.

\(fn &optional ARG)" t nil)

(autoload 'emr-c-initialize "emr-c" "\
Initialize EMR in C buffers and enable the EMR menu.

\(fn)" nil nil)

;;;***

;;;### (autoloads (emr-el-initialize emr-el-find-unused-definitions
;;;;;;  emr-el-delete-unused-definition emr-el-inline-function emr-el-inline-let-variable
;;;;;;  emr-el-extract-to-let emr-el-delete-let-binding-form emr-el-extract-autoload
;;;;;;  emr-el-tidy-autoloads emr-el-insert-autoload-directive emr-el-extract-constant
;;;;;;  emr-el-extract-variable emr-el-implement-function emr-el-extract-function
;;;;;;  emr-el-eval-and-replace emr-el-inline-variable) "emr-elisp"
;;;;;;  "emr-elisp.el" (21258 39093 711492 187000))
;;; Generated autoloads from emr-elisp.el

(autoload 'emr-el-inline-variable "emr-elisp" "\
Inline the variable defined at point.

Uses of the variable in the current buffer are replaced with the
initvalue in the variable definition.

EXAMPLE:

  (emr-el-inline-variable)

BEFORE:

  (defvar x| value)

  (usage x)

AFTER:

  (usage value)
 

\(fn)" t nil)

(autoload 'emr-el-eval-and-replace "emr-elisp" "\
Replace the current region or the form at point with its value.

EXAMPLE:

  (emr-el-eval-and-replace)

BEFORE:

  (+ (+ 1 2)| 3)

AFTER:

  (+ 3 3)

\(fn)" t nil)

(autoload 'emr-el-extract-function "emr-elisp" "\
Extract a function, using the current region or form at point as the body.
NAME is the name of the new function.
ARGLIST is its argument list.

EXAMPLE:

  (emr-el-extract-function \"extracted\" '(x))

BEFORE:

  (defun orig (x)
    (application| x))

AFTER:

  (defun extracted (x)
    (application x))

  (defun orig (x)
    (extracted x))

\(fn NAME ARGLIST)" t nil)

(autoload 'emr-el-implement-function "emr-elisp" "\
Create a function definition for the symbol at point.
The function will be called NAME and have the given ARGLIST.

EXAMPLE:

  (emr-el-implement-function \"hello\" '(x y))

BEFORE:

  |(hello x y)

AFTER:

  (defun hello (x y)
    )

  (hello x y)

\(fn NAME ARGLIST)" t nil)

(autoload 'emr-el-extract-variable "emr-elisp" "\
Extract the current region or form at point to a special variable.
The variable will be called NAME.

EXAMPLE:

  (emr-el-extract-variable \"x\")

BEFORE:

  (usage (+ 1 2)|)

AFTER:

  (defvar x (+ 1 2))

  (usage x)

\(fn NAME)" t nil)

(autoload 'emr-el-extract-constant "emr-elisp" "\
Extract the current region or form at point to a constant special variable.
The variable will be called NAME.

EXAMPLE:

  (emr-el-extract-constant \"x\")

BEFORE:

  (usage (+ 1 2)|)

AFTER:

  (defconst x (+ 1 2))

  (usage x)

\(fn NAME)" t nil)

(autoload 'emr-el-insert-autoload-directive "emr-elisp" "\
Insert an autoload directive above the current defun, macro or keymap.

EXAMPLE:

  (emr-el-insert-autoload-directive)

BEFORE:

  (defun hello| ())

AFTER:

  ;;;###autoload
  (defun hello ())

\(fn)" t nil)

(autoload 'emr-el-tidy-autoloads "emr-elisp" "\
Consolidate and reorder autoloads in the current buffer.
Order autoloads alphabetically by their file, then by their function name.

\(fn)" t nil)

(autoload 'emr-el-extract-autoload "emr-elisp" "\
Create an autoload for FUNCTION and insert it into the buffer.
FILE is the file that declares FUNCTION.  See `autoload' for
details.

* If there are no autoloads in the buffer, the new autoload will
  be inserted above the current toplevel form.

* If other autoloads exist in the buffer, the new autoload will
  be inserted near them.

\(fn FUNCTION FILE)" t nil)

(autoload 'emr-el-delete-let-binding-form "emr-elisp" "\
Delete the let binding around point.

\(fn)" t nil)

(autoload 'emr-el-extract-to-let "emr-elisp" "\
Extract the region or expression at point to a let-binding named SYMBOL.

* extracts the list at or around point

* if there is no enclosing let-form, inserts one at the top of
  the current context (e.g. the enclosing `defun' or `lambda' form).

\(fn SYMBOL)" t nil)

(autoload 'emr-el-inline-let-variable "emr-elisp" "\
Inline the let-bound variable at point.

\(fn)" t nil)

(autoload 'emr-el-inline-function "emr-elisp" "\
Replace usages of a function with its body forms.
Replaces all usages in the current buffer.

\(fn)" t nil)

(autoload 'emr-el-delete-unused-definition "emr-elisp" "\
Delete the definition form at point if it does not have usages.

\(fn)" t nil)

(autoload 'emr-el-find-unused-definitions "emr-elisp" "\
Search the buffer for functions and variables that have no usages.
Definitions with export directives are ignored.  If any unused
definitions are found, they will be collated and displayed in a
popup window.

\(fn)" t nil)

(autoload 'emr-el-initialize "emr-elisp" "\
Enable the EMR menu for Elisp buffers.

\(fn)" nil nil)

;;;***

;;;### (autoloads (emr-lisp-uncomment-block emr-lisp-comment-form)
;;;;;;  "emr-lisp" "emr-lisp.el" (21258 39093 758156 758000))
;;; Generated autoloads from emr-lisp.el

(autoload 'emr-lisp-comment-form "emr-lisp" "\
Comment out the Lisp form at point.

\(fn)" t nil)

(autoload 'emr-lisp-uncomment-block "emr-lisp" "\
Uncomment the Lisp form or forms at point.

Searches the comment block for Lisp forms to avoid uncommenting
textual comments.

\(fn)" t nil)

;;;***

;;;### (autoloads (emr-scm-extract-variable emr-scm-extract-function)
;;;;;;  "emr-scheme" "emr-scheme.el" (21258 39093 981480 67000))
;;; Generated autoloads from emr-scheme.el

(autoload 'emr-scm-extract-function "emr-scheme" "\
Extract a function, using the current region or form at point as the body.
NAME is the name of the new function.
ARGLIST is its argument list.

\(fn NAME ARGLIST)" t nil)

(autoload 'emr-scm-extract-variable "emr-scheme" "\
Extract the current region or form at point to a special variable.
The variable will be called NAME.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil nil ("emr-pkg.el" "emr-prog.el") (21258 39094
;;;;;;  20271 950000))

;;;***

(provide 'emr-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emr-autoloads.el ends here
