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

(global-set-key (kbd "<f5>") 'git-grep-project)

(provide 'git-customisations)

