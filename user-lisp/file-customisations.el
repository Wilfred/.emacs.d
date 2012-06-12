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

(defun git-list-files (directory)
  "Recursively list all the files in DIRECTORY, assuming
DIRECTORY is in a git repo."
    (let ((command (format "cd %s; git ls-files" directory)))
      (split-string (shell-command-to-string command) "\n" t)))

(autoload 'path-for-current-buffer "file-utils")
(autoload 'ido-completing-read "ido")

(defun git-pick-file ()
  "List all the files in the repo, and use ido to pick one."
  (interactive)
  (let* ((current-directory (path-for-current-buffer))
         (repo-directory (if current-directory (vc-git-root current-directory))))
    (if (and current-directory repo-directory)
        (let* ((ido-enable-flex-matching nil) ; required for acceptable performance
               (file-names (git-list-files (expand-file-name repo-directory))))
          (find-file
           (concat
            repo-directory
            (ido-completing-read "Pick a file: " file-names))))
      (message "This buffer isn't related to a git repo."))))

(global-set-key (kbd "C-x C-g") 'git-pick-file)

(provide 'file-customisations)