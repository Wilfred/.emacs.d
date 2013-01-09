(require 'python)

;; TODO: properly highlight differently named self arguments (often seen in nested classes):

;; class Foo(object):
;;     def __init__(_self, *args):
;;         pass

; indent python by 4 spaces by default
(setq-default python-indent 4)

; use autopair for Python, since it doesn't really suit paredit
(require 'autopair)
(setq autopair-autowrap t)
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

; ensure autopair handles triple-quoted strings correctly
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))


; set flymake to use pyflakes to check code (requires pyflakes installed and on $PATH)
(require 'flymake-python-pyflakes)
(setq flymake-python-pyflakes-executable "~/.emacs.d/user-python/run-pyflakes")
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))

(define-key python-mode-map [(f7)] 'flymake-goto-prev-error)
(define-key python-mode-map [(f8)] 'flymake-goto-next-error)

(defun python-insert-init-function ()
  (interactive)
  
  (let* ((exact-position (split-string (which-function) (rx "."))) ; e.g. ("FooBar" "method")
         (class-name (car exact-position))
         (method-name (cdr exact-position)))

    (newline-and-indent)
    ; if we're currently after another method, we will end up one indent too deep
    ; (assuming autopair is in use, replace the backspace command with equivalent otherwise)
    (when method-name
      (delete-char -4))
    
    (insert "def __init__(self, *args, **kwargs):")
    (newline-and-indent)
    (python-insert-super-function)
    (move-end-of-line)
    (newline-and-indent)))

(defun python-insert-super-function ()
  "Insert a call to super for the current class and function."
  ;; TODO: automatically add arguments to the superclass's function based on the current arguements
  ;; suggestion: use (beginning-of-defun) and (python-beginning-of-block) to get class-name and method-name
  (interactive)

  ;; for some reason, we need a non-empty block for which-function to find the name of the method
  (insert "pass")

  (let* ((exact-position (split-string (which-function) (rx "."))) ; e.g. ("FooBar" "method")
         (class-name (first exact-position))
         (method-name (second exact-position)))
    ;; remove 'pass'
    (backward-delete-char 4)
    
    (insert (format "super(%s, self).%s()" class-name method-name))
    ;; backward one char so the user can enter the argument for the superclass's function
    (backward-char)))

(defun python-insert-ipdb ()
  "Insert ipdb break point on this line."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (insert "import ipdb; ipdb.set_trace()"))

(defun python-insert-logging-statement (statement)
  (interactive "sWhat to log: ")
  (let ((logging-statment
         (concat "import logging; logging.critical(" statement ")")))
    (insert logging-statment)))

(define-key python-mode-map (kbd "<f1>") 'python-insert-logging-statement)

(define-skeleton python-insert-docstring
  "Insert a Python docstring."
  "This string is ignored!"
  "\"\"\"" - "\"\"\"")

(define-key python-mode-map (kbd "C-c s") 'python-insert-docstring)

(defun python-skeleton-name-main ()
  (interactive)
  (move-beginning-of-line nil)
  (insert "if __name__ == \"__main__\":")
  (newline-and-indent))

(define-key python-mode-map (kbd "C-c C-t m") 'python-skeleton-name-main)

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
