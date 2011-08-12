(require 'python)

; indent python by 4 spaces by default
(setq-default python-indent 4)

; use autopair+ for Python, since it doesn't really suit paredit
(require 'autopair)
(setq autopair-autowrap t)
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))


; set flymake to use pyflakes to check code (requires pyflakes installed and on $PATH)
(require 'tramp)

(defun flymake-friendly-p ()
  "True only if the current buffer is local and writable."
  (let ((is-local (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers))))
        (is-writable (file-writable-p (buffer-file-name))))
    (and is-local is-writable)))

(defun flymake-pyflakes-init ()
  (when (flymake-friendly-p)
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/user-python/run-pyflakes" (list local-file)))))

(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

; now always load flymake-mode with python-mode
(add-hook 'python-mode-hook 'flymake-mode)

(define-key python-mode-map [(f7)] 'flymake-goto-prev-error)
(define-key python-mode-map [(f8)] 'flymake-goto-next-error)

(defun flymake-error-at-point ()
  "Show the flymake error in the minibuffer when point is on an invalid line."
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'flymake-error-at-point)


; outline mode, note that the the minor mode shorcuts have an @ in them
; e.g. C-c C-c becomes C-c @ C-c
(defun py-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))

(defun python-outline-minor-mode ()
  ; match lines with no indent and indented "class"
  ; and "def" lines.
  (setq outline-regexp "\\(def\\|class\\) ")
  ; enable our level computation
  (setq outline-level 'py-outline-level)
  ; turn on outline mode
  (outline-minor-mode t)
  ; initially hide all but the headers
  (hide-body))

; load when we open a python file
(add-hook 'python-mode-hook 'python-outline-minor-mode)

(defun insert-break-point ()
  (interactive)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (insert "import gae_pdb; gae_pdb.set_trace()"))

(define-key python-mode-map [(f1)] 'insert-break-point)

(provide 'python-customisations)
