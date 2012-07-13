;; offer recently accessed files
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 200)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; C-x C-r was previously bound to `find-file-read-only which is not very useful
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; better dired, with colouring
(require 'dired+)

;; deleting files should go to recycle bin
(setq delete-by-moving-to-trash t)

;; better backups rather than just littering the directory with foo~
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; note there is also set-visited-file-name but this is for name changes, not path changes
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (read-from-minibuffer "New name: " (buffer-file-name))))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name t)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))

(autoload 'shell-command-to-string-in-dir "shell-utils")

(defun list-files-recursive-git (directory)
  "Recursively list all the files in DIRECTORY, assuming
DIRECTORY is in a git repo. Return nil otherwise."
  (when (and directory (vc-git-root directory))
    (split-string
     (shell-command-to-string-in-dir "git ls-files" directory) "\n" t)))

(defun list-files-recursive-svn (directory)
  "Recursively list all the files in DIRECTORY, assuming
DIRECTORY is in a subversion repo. Return nil otherwise."
  (when (and directory (file-exists-p (file-path-join directory ".svn")))
    (split-string
     (shell-command-to-string-in-dir "svn list --depth infinity | grep -v \"/$\" " directory) "\n" t)))

(defun list-files-recursive-generic (directory)
  "Recursively list all the files in DIRECTORY."
  (split-string
   (shell-command-to-string-in-dir "find . -type f | sed \"s|^\./||\"" (message directory))
   "\n" t))

(autoload 'file-find-project-root "file-utils")
(autoload 'ido-completing-read "ido")

(defun find-file-fuzzy ()
  "Use ido to a pick a file anywhere in the current project."
  (interactive)
  (when (not default-directory) (error "This buffer isn't associated with a file"))
  (let* ((ido-enable-flex-matching nil) ; required for acceptable performance (over 1000 items)
         (search-directory (file-find-project-root default-directory))
         (file-names (or (list-files-recursive-git search-directory)
                         (list-files-recursive-svn search-directory)
                         (list-files-recursive-generic search-directory))))
    (find-file
     (file-path-join
      search-directory
      (ido-completing-read
       (format "Pick a file in %s: " search-directory)
       file-names)))))

(global-set-key (kbd "C-x C-g") 'find-file-fuzzy)

(provide 'file-customisations)
