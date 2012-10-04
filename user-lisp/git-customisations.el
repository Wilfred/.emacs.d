(add-to-list 'load-path "~/.emacs.d/third-party-lisp/magit")
(autoload 'magit-status "magit")

(global-set-key (kbd "<f2>") 'magit-status)

(setq magit-completing-read-function 'magit-ido-completing-read)

(autoload 'symbol-at-point "thingatpt" nil t)

(defun git-grep-project (search-term)
  "Search a git project for SEARCH-TERM. The default search term is the symbol at point.
Note that this will not search git submodules."
 (interactive (list (read-from-minibuffer "Search with git-grep for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
 (if (buffer-file-name)
     (let ((project-root (expand-file-name (vc-git-root (buffer-file-name)))))
       (if project-root
           (vc-git-grep search-term "*" project-root)
         (message "Couldn't find project root.")))
   (message "You need to be in a file buffer.")))

(defun git-grep-to-first-result (search-term)
  "Search a git project for SEARCH-TERM, jumping to the first result."
  (git-grep-project search-term)
  (switch-to-buffer "*grep*")
  (sleep-for 0.3)
  (next-error))

; TODO: jump directly to first result
(defun git-grep-to-definition (function-or-class-name)
  "Search a git project for the definition of a Python function or class."
  (interactive (list (read-from-minibuffer "Find definition for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
  (let* ((first-char (substring function-or-class-name 0 1))
         (case-fold-search nil)
         (is-function (string-match "[a-z]" first-char)))
    (if is-function
        (git-grep-project (format "def %s(" function-or-class-name))
      ; we assume all Python classes are of the form "clas Foo(bar)" not "class Foo:"
      (git-grep-project (format "class %s(" function-or-class-name)))))

(global-set-key (kbd "<f5>") 'git-grep-project)
(global-set-key (kbd "<C-f5>") 'git-grep-to-definition)

(provide 'git-customisations)

