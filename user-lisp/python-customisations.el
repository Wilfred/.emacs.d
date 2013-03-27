(require 'python)
(require 'python-utils)

;; TODO: properly highlight differently named self arguments (often seen in nested classes):

;; class Foo(object):
;;     def __init__(_self, *args):
;;         pass

;; use autopair for Python, since paredit is unsuitable and
;; electric-pair-mode isn't smart enough
(require 'autopair)
(setq autopair-autowrap t)
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

;; jedi provides autocompletion for python
(setq jedi:setup-keys t)
(require 'jedi)
(setq jedi:server-command
      (list "python2" jedi:server-script))
(add-hook 'python-mode-hook 'jedi:setup)

; set flymake to use pyflakes to check code (requires pyflakes installed and on $PATH)
(require 'flymake-python-pyflakes)
(setq flymake-python-pyflakes-executable "~/.emacs.d/user-python/run-pyflakes")
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))

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
