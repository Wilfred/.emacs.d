(require 'python)
(require 'python-utils)
(eval-when-compile (require 'cl)) ;; first, second, loop

;; TODO: properly highlight differently named self arguments (often seen in nested classes):

;; class Foo(object):
;;     def __init__(_self, *args):
;;         pass

(require 'which-func)

(defun* python-insert-super-function (&aux start-pos)
  "Insert a call to super for the current class and function."
  (interactive)

  (setq start-pos (point))

  ;; for some reason, we need a non-empty block for which-function to find the name of the method
  (insert "pass")

  (let* ((exact-position (split-string (which-function) (rx "."))) ; e.g. ("FooBar" "method")
         (class-name (first exact-position))
         (method-name (second exact-position))
         args-list-start
         args-start
         args-end)
    ;; remove 'pass'
    (backward-delete-char 4)

    ;; go to the function definition and find the arguments
    (beginning-of-defun)

    (loop until (looking-at "(")
          do (forward-char))
    (setq args-list-start (point))
    
    ;; move beyond `self,'
    (loop until (looking-at ",")
          do (forward-char))
    ;; move to start of next argument regardless of whitespace
    (forward-word)
    (backward-word)
    (setq args-start (point))

    ;; find the end of the argument list
    (goto-char args-list-start)
    (forward-sexp)
    (setq args-end (1- (point)))

    ;; reset point to where we started
    (goto-char start-pos)

    (let ((args (buffer-substring args-start args-end)))
      (insert (format "super(%s, self).%s(%s)" class-name method-name args)))

    ;; backward one char so the user can enter the argument for the superclass's function
    (backward-char)))

(define-skeleton python-insert-docstring
  "Insert a Python docstring."
  "This string is ignored!"
  "\"\"\"" - "\"\"\"")

(define-key python-mode-map (kbd "C-c s") 'python-insert-docstring)

;; pylookup -- python docs
(setq pylookup-dir "~/.emacs.d/third-party-lisp/pylookup")
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

(provide 'python-customisations)
