(add-to-list 'load-path "~/.emacs.d/user-lisp/magit")
(require 'magit)

(global-set-key [(f2)] 'magit-status)

;; change magit diff colors to the standard red and green
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

(require 'thingatpt) ; provides symbol-at-point

(defun grep-git-project (search-term)
  "Search a git project for SEARCH-TERM. The default search term is the symbol at point.
Note that this will not search git submodules."
 (interactive (list (read-from-minibuffer "Search for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
 (if (buffer-file-name)
     (let ((project-root (expand-file-name (vc-git-root (buffer-file-name)))))
       (if project-root
           (vc-git-grep search-term "*" project-root)
         (message "Couldn't find project root.")))
   (message "You need to be in a file buffer.")))

(global-set-key (kbd "<f5>") 'grep-git-project)

(provide 'git-customisations)