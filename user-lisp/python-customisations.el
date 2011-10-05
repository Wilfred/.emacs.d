(require 'python)

; indent python by 4 spaces by default
(setq-default python-indent 4)

; use autopair for Python, since it doesn't really suit paredit
(require 'autopair)
(setq autopair-autowrap t)
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))


; set flymake to use pyflakes to check code (requires pyflakes installed and on $PATH)
(require 'flymake)

(defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/user-python/run-pyflakes" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init))

; now always load flymake-mode with python-mode
(add-hook 'python-mode-hook 'flymake-mode)


(defun flymake-can-syntax-check-file (file-name)
  "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can.
More rigorous than the default, excluding nil file names and unwritable files"
  (and file-name (file-writable-p file-name)))


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

(defun insert-init-function ()
  (interactive)
  
  (let* ((exact-position (split-string (which-function) (rx "."))) ; e.g. ("FooBar" "method")
         (class-name (car exact-position))
         (method-name (cdr exact-position)))

    (newline-and-indent)
    ; if we're currently after another method, we will end up one indent too deep
    ; (assuming autopair is in use, replace the backspace command with equivalent otherwise)
    (when method-name
      (delete-char -4))
    
    (insert "def __init__(self, *args, **kwargs):")
    (newline-and-indent)
    (insert (concat "super(" class-name ", self).__init__(*args, **kwargs)"))
    
    (newline-and-indent)))

(defun insert-break-point ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (insert "import gae_pdb; gae_pdb.set_trace()"))

(define-key python-mode-map [(f12)] 'insert-break-point)

(defun insert-logging-statement (statement)
  (interactive "sWhat to log: ")
  (move-end-of-line nil)
  (newline-and-indent)
  (let ((logging-statment
         (concat "import logging; logging.critical(" statement ")")))
    (insert logging-statment)))


(define-key python-mode-map [(f1)] 'insert-logging-statement)

(provide 'python-customisations)
