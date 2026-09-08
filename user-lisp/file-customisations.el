;;; -*- lexical-binding: nil; -*-
(declare-function -last-item "dash")
(declare-function f-expand "f")
(declare-function f-join "f")
(declare-function s-split "s")

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
  (set-buffer-modified-p nil)
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

(defun wh--visit-parts (path line-num &optional col-num)
  "Visit the path/line/column specified."
  (let ((buf (find-file-noselect path)))
    (pop-to-buffer buf)
    (widen)
    (goto-char (point-min))
    (when line-num
      (forward-line (1- line-num))
      (when col-num
        (forward-char col-num)))))

(defun wh--visit (path-and-pos)
  "Visit \"/foo/bar:123:4\"."
  (let* ((parts (s-split ":" path-and-pos))
         (path (nth 0 parts))
         (line-num (read (nth 1 parts)))
         (col-num (read (nth 2 parts))))
    (find-file path)
    (widen)
    (goto-char (point-min))
    (forward-line (1- line-num))
    (forward-char (1- col-num))))

(defun wh/visit-abs-file-at-pos ()
  (interactive)
  ;; E.g. "#0 Errors /home/wilfred/foo/bar.ml:2196:3"
  (let* ((line
          (buffer-substring (line-beginning-position)
                            (line-end-position)))
         (parts (s-split " " line))
         (line-spec (-last-item parts)))
    (wh--visit line-spec)))

(provide 'file-customisations)
