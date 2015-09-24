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

;; Anaconda is great for jump-to-definition and eldoc. You do need to
;; tell it which virtualenv you're using, see `venv-workon'.
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(diminish 'anaconda-mode "Ana")

(define-key python-mode-map (kbd "M-.") 'anaconda-mode-goto-definitions)
(define-key python-mode-map (kbd "M-,") 'anaconda-nav-pop-marker)

(require 'company)
(defun wh/company-in-python-mode ()
  (setq-local company-idle-delay 0.3)
  (set (make-local-variable 'company-backends)
       (list #'company-whole-line #'company-anaconda #'company-dabbrev-code)))

(add-hook 'python-mode-hook #'wh/company-in-python-mode)

;; Use ipython, if available.
;; from http://emacs.stackexchange.com/q/4161
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; Don't font lock in an inferior python shell. It's too easy for a
;; docstring (when using foo? in ipython) to contain doublequotes and
;; all the highlighting is broken from then onwards.
(setq python-shell-enable-font-lock nil)

(define-key python-mode-map (kbd "C-t") #'pytest-one)

(require 'python-smart-execute)
(define-key python-mode-map (kbd "<f1>") #'python-smart-execute)
(define-key python-mode-map (kbd "<S-f1>") #'python-smart-execute-no-move)

(provide 'python-customisations)
