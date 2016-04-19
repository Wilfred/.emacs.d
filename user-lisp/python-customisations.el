;;; python-customisations

;;; Commentary:
;; Configuring Python editing to make me happy.
;;
;; Outstanding issues:
;;
;; * It would be nice to have jump-to-definition for projects that use
;;   Python's stdlib. This will need thought to handle RPython projects.
;; * Add a command that automatically adds missing imports.
;;
;; Bugs:
;;
;; * We should report a bug where a buffer whose first line is a
;;   function header, e.g.:
;;
;;   def foo(x, y);
;;       return x + y
;;
;;   does not work with python-shell-send-defun because it tries to
;;   inspect the previous line for decorators.

(require 'python)
(require 'python-utils)
(eval-when-compile (require 'cl)) ;; first, second, loop

;; TODO: properly highlight differently named self arguments (often seen in nested classes):

;; class Foo(object):
;;     def __init__(_self, *args):
;;         pass

(require 'which-func)

;; FIXME: breaks for class methods, since it assumes `self'
(defun* python-insert-super-function (&aux start-pos)
  "Insert a call to super for the current class and function."
  (interactive)

  (setq start-pos (point))

  ;; for some reason, we need a non-empty block for which-function to find the name of the method
  (insert "pass")

  (let* ((exact-position (split-string (which-function) (rx "."))) ; e.g. ("FooBar" "method")
         (class-name (first exact-position))
         (method-name (second exact-position))
         args-start
         args-end)
    ;; remove 'pass'
    (backward-delete-char 4)

    ;; go to the function definition and find the arguments
    (beginning-of-defun)

    (loop until (looking-at "(")
          do (forward-char))
    (setq args-start (1+ (point)))

    (forward-sexp)
    (setq args-end (1- (point)))
    
    ;; reset point to where we started
    (goto-char start-pos)

    (let ((args (s-trim (buffer-substring args-start args-end))))
      
      ;; remove 'self'
      (setq args (s-trim (s-chop-prefixes '("self," "self") args)))

      (insert (format "super(%s, self).%s(%s)" class-name method-name args)))

    ;; backward one char so the user can enter the argument for the superclass's function
    (backward-char)))

(define-key python-mode-map (kbd "C-M-n") 'python-nav-forward-statement)
(define-key python-mode-map (kbd "C-M-p") 'python-nav-backward-statement)
;; To be consistent with our paredit keybindings, use super for syntatic movement.
(define-key python-mode-map (kbd "s-n") 'python-nav-forward-statement)
(define-key python-mode-map (kbd "s-p") 'python-nav-backward-statement)
(define-key python-mode-map (kbd "s-f") #'python-nav-forward-sexp)
(define-key python-mode-map (kbd "s-b") #'python-nav-backward-sexp)

(define-key python-mode-map (kbd "s-u") 'python-nav-backward-up-list)

;; mark-sexp is useless in python, we want the the equivalent command
;; for marking a Python statement.
(define-key python-mode-map (kbd "C-M-SPC") #'er/mark-python-statement)

(exec-path-from-shell-copy-env "WORKON_HOME")
(require 'virtualenvwrapper)

(defadvice python-nav-up-list (before python-nav-push-mark activate)
  "Push the mark before this Python nav command.
This means `pop-mark' can take us back to our previous position."
  (push-mark))

;; Anaconda is great for jump-to-definition. You do need to
;; tell it which virtualenv you're using, see `venv-workon'.
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(diminish 'anaconda-mode "Ana")

(define-key anaconda-mode-map (kbd "M-,") #'anaconda-mode-go-back)

;; Sometimes it's still useful to find tags, even when we're using
;; anaconda. Provide a fallback keybinding.
(define-key anaconda-mode-map (kbd "C-c M-.") #'etags-select-find-tag)

(require 'company)
(defun wh/company-in-python-mode ()
  (setq-local company-idle-delay 0.3)
  (set (make-local-variable 'company-backends)
       (list
        ;; It's rare for company-files to fire, but it's great
        ;; when we want it. Low noise, so put it first.
        #'company-files
        ;; anaconda is able to do smarter Python analysis, so it's
        ;; particularly useful with built-ins like IndexError and
        ;; packages like 'from foo import bar'.
        #'company-anaconda
        ;; This is a noisy backend, so we try it later.
        #'company-whole-line
        ;; As a last ditch attempt, just normal dabbrev.
        #'company-dabbrev-code)))

(add-hook 'python-mode-hook #'wh/company-in-python-mode)

;; Use ipython, if available.
;; from http://emacs.stackexchange.com/q/4161
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; Don't font lock in an inferior python shell. It's too easy for a
;; docstring (when using foo? in ipython) to contain doublequotes and
;; all the highlighting is broken from then onwards.
(setq python-shell-enable-font-lock nil)

(define-key python-mode-map (kbd "C-c C-t") #'pytest-one)

(setq pytest-cmd-flags "-x --tb=native")

(require 'eval-in-repl)
(require 'eval-in-repl-python)
(define-key python-mode-map (kbd "<f1>") #'eir-eval-in-python)

;; From http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/
;; (but widely used e.g. spacemacs has it too).
;;
;; This is useful when tab completion stops working in *Python*
;; buffers. This has been fixed in Emacs 25.
(defun wh/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;;###autoload
(defun wh/wrap-try-ipdb ()
  "Wrap the current line in a try-except that uses ipdb on exception.

Before:
x = foo()

After:
try:
    x = foo()
except Exception as e:
    import ipdb; ipdb.set_trace()"
  (interactive "*")
  (save-excursion
    ;; Insert try: above.
    (crux-smart-open-line-above)
    (insert "try:")
    ;; Indent the next line.
    (forward-line 1)
    (back-to-indentation)
    (insert "    ")
    ;; Add an except below
    (python-nav-forward-sexp 1)
    (newline-and-indent)
    (python-indent-dedent-line-backspace 1)
    (insert "except Exception as e:")
    (newline-and-indent)
    (insert "import ipdb; ipdb.set_trace()")))

(provide 'python-customisations)
