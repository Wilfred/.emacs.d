(require 'python)

;;; TODO: fix indentation for the following code:
;; def foo():
;;     try:
;;         pass
;;     except Artwork.DoesNotExist:
;;         bar = "A very very very long string, so we use backslash %s %s" % \
;;             (None,
;;              None)

;;             # this is four spaces more than correct indentation, but
;;             # Python mode doesn't like the proper indentation
;;             pass

;; TODO: also fix indentation for the following:

;; response = Client().post('admin/tasks/csv_checkflbjklj',
;;                          # the value of the ID is irrelevant:
;;                              {'csv_import_id': 123456})

;; (the problem is the colon in the comment)

;; TODO: also fix indentation for the following:

;; def foo():
;;     """foo bar foo bar foo bar foo bar foo bar foo bar(see #634), so we always try
;;     that first.

;;     """

;; (the problem is the # inside the docstring)

;; TODO: properly highlight differently named self arguments (often seen in nested classes):

;; class Foo(object):
;;     def __init__(_self, *args):
;;         pass


; indent python by 4 spaces by default
(setq-default python-indent 4)

; use autopair for Python, since it doesn't really suit paredit
(require 'autopair)
(setq autopair-autowrap t)
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

; ensure autopair handles triple-quoted strings correctly
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))


; set flymake to use pyflakes to check code (requires pyflakes installed and on $PATH)
(require 'flymake)

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))


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
  (outline-minor-mode t))

; load when we open a python file
(add-hook 'python-mode-hook 'python-outline-minor-mode)

(defun python-insert-init-function ()
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
    (python-insert-super-function)
    (move-end-of-line)
    (newline-and-indent)))

(defun python-insert-super-function ()
  "Insert a call to super for the current class and function."
  ; TODO: automatically add arguments to the superclass's function based on the current arguements
  ; FIXME: detecting function name is not very reliable
  ; suggestion: use (beginning-of-defun) and (python-beginning-of-block) to get class-name and method-name
  (interactive)

  (let* ((exact-position (split-string (which-function) (rx "."))) ; e.g. ("FooBar" "method")
         (class-name (first exact-position))
         (method-name (second exact-position)))
    (insert (concat "super(" class-name ", self)." method-name "()"))
    ; backward one char so the user can enter the argument for the superclass's function
    (backward-char)))

(defun python-insert-gae-pdb ()
  "Insert GAE pdb break point on this line."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (insert "import gae_pdb; gae_pdb.set_trace()"))

(defun python-insert-ipdb ()
  "Insert ipdb break point on this line."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (insert "import ipdb; ipdb.set_trace()"))

(define-key python-mode-map (kbd "<f12>") 'python-insert-gae-pdb)

(defun python-insert-logging-statement (statement)
  (interactive "sWhat to log: ")
  (let ((logging-statment
         (concat "import logging; logging.critical(" statement ")")))
    (insert logging-statment)))

(define-key python-mode-map (kbd "<f1>") 'python-insert-logging-statement)

(define-skeleton python-insert-docstring
  "Insert a Python docstring."
  "This string is ignored!"
  "\"\"\"" - "\"\"\"")

(define-key python-mode-map (kbd "C-c s") 'python-insert-docstring)

(provide 'python-customisations)
