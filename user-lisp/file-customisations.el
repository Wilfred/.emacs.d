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

(defun path-for-current-buffer ()
  "Find a path associated with the current buffer, if
possible. No trailing slash. Returns nil otherwise."
  (let ((file-name (buffer-file-name)))
    (if file-name (directory-file-name (file-name-directory file-name))
      (expand-file-name "."))))

(autoload 'ido-completing-read "ido")

(defun git-pick-file ()
  "List all the files in the repo, and use ido to pick one."
  (interactive)
  (let ((current-directory (path-for-current-buffer)))
    (if current-directory
        (let* ((ido-enable-flex-matching nil) ; required for acceptable performance
               (project-root (file-name-as-directory
                              (expand-file-name (vc-git-root current-directory))))
               (file-names (git-list-files project-root)))
          (find-file
           (concat
            project-root
            (ido-completing-read "Pick a file: " file-names))))
      (message "This buffer is not associated with a file or directory."))))

(global-set-key (kbd "C-x C-g") 'git-pick-file)

(defun parent-directory (path)
  (directory-file-name (file-name-directory path)))

(defun file-path-join (directory-name file-name)
  "Join the relative FILE-NAME to DIRECTORY-NAME, adding slashes where appropriate."
  (concat (file-name-as-directory directory-name) file-name))

(defun find-in-parent-directory (path file-name)
  "Search PATH and all parent directories for file FILE-NAME,
returning the first path found or nil."
  (let* ((canonical-path (expand-file-name path))
         (absolute-file-path (file-path-join canonical-path file-name)))
    (cond ((file-exists-p absolute-file-path)
           ;; success -- we've found it!
           absolute-file-path)
          ((string= canonical-path "/")
           ;; reached root, the file doesn't exist in any ancestor directory
           nil)
          (t
           (find-in-parent-directory (parent-directory canonical-path) file-name)))))

(defun project-find-root ()
  "Find the probable root of the project for the current buffer.
TODO: svn"
  (let ((current-directory (expand-file-name ".")))
    (or
     (find-in-parent-directory current-directory ".git")
     (find-in-parent-directory current-directory "pom.xml"))))

(provide 'file-customisations)