;;; smart-eval.el --- A do-what-I-mean elisp eval command

;;; Commentary:
;; When evaluating an elisp buffer, we want the following features:
;;
;; 1. Evaluate the current conntent. This could either be the form
;; before point, the top-level form enclosing point, a region, or a
;; buffer.
;; 
;; 2. Easily debug the current form, perhaps by giving a prefix
;; argument.
;;
;; 3. Force `defvar', `defface' and `defcustom' to be re-evaluated (as
;; normally they do not override existing values).
;;
;; 4. Always open a debugger if the evaluation throws an error.
;;
;; Note that eval-last-sexp sets debug-on-error regardless of what it
;; was set to before. Customise eval-expression-debug-on-error to
;; change it. We need to think about what's best for smart-eval
;; (always debug I think).

(defun smart-eval-sexp ()
  (interactive))

(defun smart-eval-outer-sexp ()
  (interactive))

(defun smart-eval-region ()
  (interactive))

(defun smart-eval-buffer ()
  (interactive))
