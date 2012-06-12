; better dired, with colouring
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

(defun git-list-files (directory)
  "Recursively list all the files in DIRECTORY, assuming
DIRECTORY is in a git repo. Return nil otherwise."
  (when (vc-git-root directory)
    (split-string
     (shell-command-to-string-in-dir "git ls-files" directory) "\n" t)))

(defun list-files-recursive (directory)
  "Recursively list all the files in DIRECTORY, ignoring .svn directories."
  (split-string
   (shell-command-to-string-in-dir "find . -type f -not -iwholename '*.svn*' | sed \"s|^\./||\"" (message directory))
   "\n" t))

(autoload 'file-find-project-root "file-utils")
(autoload 'ido-completing-read "ido")

(defun find-file-fuzzy ()
  "Use ido to a pick a file anywhere in the current project."
  (interactive)
  (let* ((ido-enable-flex-matching nil) ; required for acceptable performance (over 1000 items)
         (search-directory (file-find-project-root default-directory))
         (file-names (or (git-list-files search-directory)
                         (list-files-recursive search-directory))))
    (find-file
     (file-path-join
      search-directory
      (ido-completing-read
       (format "Pick a file in %s: " search-directory)
       file-names)))))

(global-set-key (kbd "C-x C-g") 'find-file-fuzzy)

(provide 'file-customisations)