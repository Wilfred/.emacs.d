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

;; When switching to a project (bound to `C-c p p'), open magit.
(use-package projectile
  :config
  (setq projectile-switch-project-action
        (lambda () (magit-status default-directory))))

;; When opening a file, restore point to the previous location.
(use-package saveplace
  :config
  (setq-default save-place t))

;;;###autoload
(defun wh/open-customisations (name)
  (interactive "sCustomisations file name: ")
  (let ((path (f-join (f-expand "~/.emacs.d")
                      "user-lisp" (format "%s.el" name))))
    (find-file path)))

(provide 'file-customisations)
