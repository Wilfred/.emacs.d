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

; vc-git-grep, fixed so it doesn't use the local color settings (Emacs bug #9408)
(defun vc-git-grep (regexp &optional files dir)
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
				   nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "In directory: "
					  nil default-directory t)))
	   (list regexp files dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
	  (if (string= command "git grep")
	      (setq command nil))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(setq command
              ; wilfred changed this line:
	      (grep-expand-template "git grep --no-color -n -e <R> -- <F>" regexp files))
	(when command
	  (if (equal current-prefix-arg '(4))
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir)
	      (compilation-environment '("PAGER=")))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start command 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))


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
        (grep-git-project (concat "def " function-or-class-name "("))
      ; we assume all Python classes are of the form "clas Foo(bar)" not "class Foo:"
      (grep-git-project (concat "class " function-or-class-name "(")))))

(global-set-key (kbd "<f5>") 'grep-git-project)
(global-set-key (kbd "<C-f5>") 'git-grep-to-definition)


(provide 'git-customisations)