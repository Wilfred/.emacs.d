; better dired, with colouring
(require 'dired+)

; deleting files should go to recycle bin
(setq delete-by-moving-to-trash t)

; better backups rather than just littering the directory with foo~
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

; note there is also set-visited-file-name but this is for name changes, not path changes
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

(defun git-pick-file ()
  "List all the files in the repo, and use ido to pick one."
  (interactive)
  (let* ((ido-enable-flex-matching nil) ; required for acceptable performance
         (project-root (expand-file-name (vc-git-root (buffer-file-name))))
         (command (format "cd %s; git ls-files" project-root)))
    (find-file
     (concat
      project-root
      (ido-completing-read
       "Pick a file: "
       (split-string (shell-command-to-string command) "\n" t))))))

(provide 'file-customisations)