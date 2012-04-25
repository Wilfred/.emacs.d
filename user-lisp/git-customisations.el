(add-to-list 'load-path "~/.emacs.d/third-party-lisp/magit")
(require 'magit)

(global-set-key (kbd "<f2>") 'magit-status)

;; change magit diff colors to the standard red and green
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

(autoload 'symbol-at-point "thingatpt" nil t)

; TODO: make more generic, falling back to ack if git isn't available
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

(defun git-grep-to-first-result (search-term)
  "Search a git project for SEARCH-TERM, jumping to the first result."
  (grep-git-project search-term)
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
        (grep-git-project (format "def %s(" function-or-class-name))
      ; we assume all Python classes are of the form "clas Foo(bar)" not "class Foo:"
      (grep-git-project (format "class %s(" function-or-class-name)))))

(defun git-show-conflicts ()
  "Show all the conflicts in the current buffer using occur-mode."
  (interactive)
  (occur "<<<<<<< HEAD")

  ; put mark on the first result in the occur window
  (other-window 1)
  (next-line))

(global-set-key (kbd "<f10>") 'git-show-conflicts)

(global-set-key (kbd "<f5>") 'grep-git-project)
(global-set-key (kbd "<C-f5>") 'git-grep-to-definition)

(defun git-keep-conflict-first ()
  "Given a buffer containing `<<<< HEAD' stuff, keep the first of
the two options."
  (interactive)
  ; remove the first line
  (search-forward-regexp "<<<<<<<.*?\n")
  (replace-match "")

  ; remove the second option
  (search-forward-regexp "=======\\(\n\\|.\\)*?>>>>>>>.*?\n")
  (replace-match ""))

(defun git-keep-conflict-second ()
  "Given a buffer containing `<<<< HEAD' stuff, keep the second of
the two options."
  (interactive)
  ; remove the first option
  (search-forward-regexp "<<<<<<<\\(\n\\|.\\)*?=======\n")
  (replace-match "")
  
  ; remove the last line
  (search-forward-regexp ">>>>>>>.*?\n")
  (replace-match ""))

(provide 'git-customisations)
