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

;; emacs doesn't actually save undo history with revert-buffer
;; see http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-04/msg00151.html
;; fix that.
(defun revert-buffer-keep-history (&optional IGNORE-AUTO NOCONFIRM PRESERVE-MODES)
  (interactive)

  ;; tell Emacs the modtime is fine, so we can edit the buffer
  (clear-visited-file-modtime)

  ;; insert the current contents of the file on disk
  (widen)
  (delete-region (point-min) (point-max))
  (insert-file-contents (buffer-file-name))

  ;; mark the buffer as not modified
  (not-modified)
  (set-visited-file-modtime))

(setq revert-buffer-function 'revert-buffer-keep-history)

;; note there is also set-visited-file-name but this is for name changes, not path changes
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (read-from-minibuffer "New name: " (buffer-file-name))))
  (let ((filename (buffer-file-name))
        (new-directory (file-name-directory new-name)))
    (unless filename (error "Buffer '%s' is not visiting a file!" (buffer-name)))
    
    (make-directory new-directory t)
    (rename-file filename new-name 1)
    (rename-buffer new-name t)
    (set-visited-file-name new-name)
    (set-buffer-modified-p nil)))

(require 'find-file-in-repository)

(defadvice find-file-in-repository (around disable-ido-flex-matching)
  (let ((ido-enable-flex-matching nil))
    ad-do-it))

(ad-activate 'find-file-in-repository)
(global-set-key (kbd "C-x C-g") 'find-file-in-repository)

(provide 'file-customisations)
