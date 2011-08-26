(add-to-list 'load-path "~/.emacs.d/user-lisp/magit")
(require 'magit)

(global-set-key [(f2)] 'magit-status)

(require 'thingatpt) ; provides symbol-at-point

(defun grep-git-project (search-term)
  "Search a git project using git grep. The default search term is the symbol at point."
  (interactive (list (read-from-minibuffer "Search for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
  (setq project-root (vc-git-root (buffer-file-name)))
  (if project-root
      (vc-git-grep search-term "*" project-root)
    (message "Couldn't find project root.")))

(global-set-key [(f5)] 'grep-git-project)

(provide 'git-customisations)