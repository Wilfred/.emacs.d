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

(defun find-file-sudo ()
  (interactive)
  (find-file (concat "/sudo::" (ido-read-file-name "Sudo find file:"))))

(defun duplicate-buffer (new-name)
  "Create a copy of the current buffer with the filename NEW-NAME.
The original buffer and file are untouched."
  (interactive (list (read-from-minibuffer "New name: " (buffer-file-name))))

  (let ((filename (buffer-file-name))
        (new-directory (file-name-directory new-name))
        (contents (buffer-substring (point-min) (point-max))))
    (unless filename (error "Buffer '%s' is not visiting a file!" (buffer-name)))
    
    (make-directory new-directory t)
    (find-file new-name)
    (insert contents)
    (basic-save-buffer)))

(defun emacs-d-magit ()
  (interactive)
  (magit-status "~/.emacs.d"))

;; When switching to a project (bound to `C-c p p'), open magit.
(require 'projectile)
(setq projectile-switch-project-action (lambda () (magit-status default-directory)))

;; When opening a file, restore point to the previous location.
(require 'saveplace)
(setq-default save-place t)

(provide 'file-customisations)
