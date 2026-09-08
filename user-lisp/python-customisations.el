;;; -*- lexical-binding: nil; -*-
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

(defvar flycheck-python-mypy-args)
(defvar pytest-cmd-flags)

(declare-function python-smart-execute "python-smart-execute")
(declare-function python-smart-execute-no-move "python-smart-execute")
(declare-function sp-backward-sexp "smartparens")
(declare-function sp-forward-sexp "smartparens")

(defun wh/backward-up-python (old-function &rest args)
  "Use Python navigation, or call OLD-FUNCTION with ARGS in other modes."
  (if (eq major-mode 'python-mode)
      (python-nav-backward-up-list)
    (apply old-function args)))

(use-package python
  :config
  ;; Open .pyi in Python mode. This has been fixed upstream:
  ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=94b2e1fc00f90b4072b4a998caf2054c540b7ac4
  (when (< emacs-major-version 26)
    (add-to-list 'auto-mode-alist (cons (rx ".pyi" eos) #'python-mode)))

  ;; Everyone uses four space indents, and Emacs noisily announces
  ;; indentation guessing.
  (setq python-indent-guess-indent-offset-verbose nil)

  (require 'which-func)

  ;; FIXME: breaks for class methods, since it assumes `self'
  (defun wh/python-insert-super-function ()
    "Insert a call to super for the current class and function."
    (interactive)
    ;; for some reason, we need a non-empty block for which-function to find the name of the method
    (insert "pass")

    (let* (class-name method-name args-start args-end)

      (-let (((class-name method-name) (split-string (which-function) (rx ".")))
             (args-start nil)
             (args-end nil))

        ;; remove 'pass'
        (backward-delete-char 4)

        (save-excursion
          ;; go to the function definition and find the arguments
          (beginning-of-defun)

          (while (not (looking-at "("))
            (forward-char))
          (setq args-start (1+ (point)))
          (forward-sexp)
          (setq args-end (1- (point))))

        (let ((args (s-trim (buffer-substring args-start args-end))))
          ;; remove 'self'
          (setq args (s-trim (s-chop-prefixes '("self," "self") args)))

          (insert (format "super(%s, self).%s(%s)" class-name method-name args)))))

    ;; backward one char so the user can enter the argument for the superclass's function
    (backward-char))

  (define-key python-mode-map (kbd "C-M-n") 'python-nav-forward-statement)
  (define-key python-mode-map (kbd "C-M-p") 'python-nav-backward-statement)
  ;; To be consistent with our paredit keybindings, use super for syntatic movement.
  (define-key python-mode-map (kbd "s-n") 'python-nav-forward-statement)
  (define-key python-mode-map (kbd "s-p") 'python-nav-backward-statement)

  (define-key python-mode-map (kbd "s-f") #'sp-forward-sexp)
  (define-key python-mode-map (kbd "C-M-f") #'sp-forward-sexp)
  (define-key python-mode-map (kbd "s-b") #'sp-backward-sexp)
  (define-key python-mode-map (kbd "C-M-b") #'sp-backward-sexp)

  (define-key python-mode-map (kbd "s-u") 'python-nav-backward-up-list)
  ;; TODO: this is only necessary because the above keybinding is
  ;; overridden by smartparens.
  (advice-add 'sp-backward-up-sexp :around #'wh/backward-up-python)

  ;; mark-sexp is useless in python, we want the the equivalent command
  ;; for marking a Python statement.
  (define-key python-mode-map (kbd "C-M-SPC") #'er/mark-python-statement)

  ;; Use ipython, if available.
  ;; from http://emacs.stackexchange.com/q/4161
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          ;; ipython 5 compat
          python-shell-interpreter-args "--simple-prompt -i"))

  ;; Don't font lock in an inferior python shell. It's too easy for a
  ;; docstring (when using foo? in ipython) to contain doublequotes and
  ;; all the highlighting is broken from then onwards.
  (setq python-shell-font-lock-enable nil)

  (define-key python-mode-map (kbd "C-c C-t") #'pytest-one)

  (setq pytest-cmd-flags "-x --tb=native")

  (require 'python-smart-execute)
  (define-key python-mode-map (kbd "C-c e") #'python-smart-execute)
  (define-key python-mode-map (kbd "<f1>") #'python-smart-execute)
  (define-key python-mode-map (kbd "<S-f1>") #'python-smart-execute-no-move))

(setq flycheck-python-mypy-args '("--py2" "--strict" "--disallow-any=generics"))
(setq flycheck-python-mypy-executable "/Users/whughes/pyenvs/mypyenv/bin/mypy")

;; TODO: properly highlight differently named self arguments (often seen in nested classes):

;; class Foo(object):
;;     def __init__(_self, *args):
;;         pass

(exec-path-from-shell-copy-env "WORKON_HOME")

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

(defun wh/wrap-try-ipdb-post-mortem ()
  "Equivalent to `wh/wrap-try-ipdb', but using post-mortem debugging."
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
    (insert "except Exception:")
    (newline-and-indent)
    (insert "import ipdb, sys; tb = sys.exc_info()[2]; ipdb.post_mortem(tb)")))

;; TODO: move to emr.
(defun wh/unwrap-python-try ()
  "Extract a single line wrapped in a try: block and remove the try/except."
  (interactive "*")
  ;; Move to the except: block
  (python-nav-forward-statement)
  ;; Delete it.
  (er/mark-python-block)
  (delete-region (line-beginning-position) (1+ (region-end)))
  ;; Move to the try: keyword.
  (python-nav-forward-statement -2)
  ;; Delete it.
  (delete-region (line-beginning-position) (1+ (line-end-position)))
  ;; Unindent the current line.
  (python-indent-shift-left (line-beginning-position) (line-end-position)))

(define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)

(provide 'python-customisations)
