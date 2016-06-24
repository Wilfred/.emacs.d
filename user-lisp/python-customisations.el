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
;; TODO: this is only necessary because the above keybinding is
;; overridden by smartparens.
(defadvice sp-backward-up-sexp (around wh/backward-up-python activate)
  "When editing python, defer to Python's navigation command."
  (if (eq major-mode 'python-mode)
      (python-nav-backward-up-list)
    ad-do-it))

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

(require 'company-whole-line)
(require 'rx)
(require 's)

(defun wh/import-lines (buffer)
  "Return all the lines in this Python buffer that look like imports."
  (with-current-buffer buffer
    (let (lines)
      (wh/for-each-line
       (when (looking-at (rx (or (seq bol "from ")
                                 (seq bol "import "))))
         (push (propertize (cwl--current-line) 'pyimport-path (buffer-name)) lines)))
      lines)))

;; TODO: factor out a function that just returns a list of lines in the file.
(defmacro wh/for-each-line (&rest body)
  `(save-excursion
     (goto-char (point-min))
     ;; TODO: this ignores the last line.
     (cl-loop
      until (cwl--last-line-p)
      do (progn
           ,@body
           (forward-line)))))

(defun wh/same-module (import1 import2)
  "Return t if both lines of Python imports are from the same module."
  (-let (((keyword1 mod1 ...) (s-split " " import1))
         ((keyword2 mod2 ...) (s-split " " import2)))
    (and (string= keyword1 "from")
         (string= keyword2 "from")
         (string= mod1 mod2))))

(defun wh/insert-import (line)
  "Insert IMPORT, a line of python imports, in the current buffer."
  (let* ((current-lines (wh/import-lines (current-buffer)))
         (same-pkg-lines (--filter (wh/same-module it line) current-lines)))
    (if same-pkg-lines
        ;; Find the first matching line, and append there
        (wh/for-each-line
         (when (wh/same-module (cwl--current-line) line)
           (move-end-of-line nil)
           (-let [(_ _module _ name) (s-split " " line)]
             (insert ", " name))
           ;; Break from this loop.
           (cl-return nil)))

      ;; We don't have any imports for this module yet, so just insert
      ;; LINE as-is.
      (save-excursion
        (goto-char (point-min))
        (crux-smart-open-line-above)
        (insert line)))))

(defun wh/import-simplify (line symbol)
  "Given LINE 'from foo import bar, baz', simplify it to 'from foo import baz', where
baz is SYMBOL."
  ;; TODO: simplify "from foo import bar, baz as biz" -> "from foo import baz as biz"
  (cond ((string-match "from .* import .* as .*" line)
         line)
        ((s-starts-with-p "from " line)
         (let ((parts (s-split " " line)))
           (format "from %s import %s" (nth 1 parts) symbol)))
        (t
         line)))

(defun wh/auto-import ()
  "Try to insert an import for the symbol at point.
Dumb: just scans open Python buffers."
  (interactive)
  (let ((symbol (substring-no-properties (thing-at-point 'symbol)))
        (matching-lines nil)
        (case-fold-search nil))
    ;; Find all the import lines in all Python buffers
    (dolist (buffer (cwl--buffers-in-mode 'python-mode))
      (dolist (line (wh/import-lines buffer))
        ;; If any of them contain the current symbol:
        (when (string-match (rx-to-string `(seq symbol-start ,symbol symbol-end)) line)
          (push line matching-lines))))

    ;; Sort by string length, because the shortest string is usually best.
    (cl-sort matching-lines #'< :key #'length)

    (if matching-lines
        (let* ((example-line (-first-item matching-lines))
               (line (wh/import-simplify example-line symbol)))
          (wh/insert-import line)
          (message "%s (from %s)" line (get-text-property 0 'pyimport-path example-line)))
      (user-error "No matches found"))))

(define-key python-mode-map (kbd "C-c C-i") #'wh/auto-import)

(defun wh/extract-unused-var (flycheck-message)
  "Given a string from flycheck of the form:

'foo' imported but unused

return 'foo'."
  (-last-item (s-match "'\\(.*\\)' imported but unused" flycheck-message)))

(defun wh/remove-on-line (text)
  "Remove the first text on the current line, if present.
Returns t on success, nil otherwise."
  (save-excursion
    (move-beginning-of-line nil)
    (let ((next-line-pos (save-excursion (forward-line 1) (point))))
      ;; Search forward, until we find the text on this line.
      (when (search-forward text next-line-pos t)
        ;; If we found it, delete it.
        (delete-backward-char (length text))
        t))))

(defun wh/delete-current-line ()
  (save-excursion
    (let ((line-start (progn (move-beginning-of-line nil) (point)))
          (next-line-start (progn (forward-line 1) (point))))
      (delete-region line-start next-line-start))))

(defun wh/remove-import (line var)
  "Given a line of Python code of the form

from foo import bar, baz, biz

on line number LINE, remove VAR (e.g. 'baz')."
  (let ((case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))

      (cond
       ;; If it's just 'import foo' or 'import foo.bar', just remove it.
       ((looking-at (rx "import " (1+ (not (any space))) line-end))
        (wh/delete-current-line))

       ;; Otherwise, it's '... import foo' or '... import foo as bar'
       (t
        ;; Remove the variable reference.
        (or (wh/remove-on-line (format ", %s" var))
            (wh/remove-on-line (format "%s, " var))
            (wh/remove-on-line var))
        ;; If we only have "from foo import " left, remove the rest of the line.
        (when (or (looking-at (rx "from " (1+ (not (any space))) " import " line-end))
                  (looking-at (rx "import " (1+ (not (any space))) " as " line-end)))
          (wh/delete-current-line)))))))

(defun wh/cleanup-unused-imports ()
  "Remove unused imports in Python code."
  (interactive)
  (let* ((filename (buffer-file-name))
         (flycheck-output (shell-command-to-string
                           (format "%s %s"
                                   flycheck-python-pyflakes-executable
                                   filename)))
         (raw-lines (s-split "\n" (s-trim flycheck-output)))
         (lines (--map (s-split ":" it) raw-lines))
         (import-lines (--filter (s-ends-with-p "imported but unused" (-last-item it)) lines))
         (unused-imports (--map (cons (read (nth 1 it))
                                      (wh/extract-unused-var (nth 2 it))) import-lines)))
    ;; Iterate starting form the last unused import, so our line
    ;; numbers stay correct, even when we delete lines.
    (--each (reverse unused-imports)
      (-let [(line . var ) it]
        (wh/remove-import line var)))))

(provide 'python-customisations)
