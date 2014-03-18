
(defun export-init-org ()
  "Generate init.html and init.html from the current init.org file."
  (interactive)
  (call-interactively 'org-babel-tangle)
  (call-interactively 'org-export-as-html))

(setq org-export-html-style-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"init.css\" />")

(add-to-list 'load-path "~/.emacs.d/user-lisp/")

(add-to-list 'load-path "~/.emacs.d/third-party-lisp/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(require 'dash)
(require 'f)

(defun was-compiled-p (path)
  "Does the directory at PATH contain any .elc files?"
  (--any-p (f-ext? it "elc") (f-files path)))

(defun ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (--each (f-directories package-user-dir)
    (unless (was-compiled-p it)
      (byte-recompile-directory it 0))))

(ensure-packages-compiled)

;; todo: clean up orphaned .elc files

;; set exec-path according to the system's PATH
(exec-path-from-shell-initialize)

(add-to-list 'load-path "~/.emacs.d/third-party-lisp/color-theme")
(require 'color-theme)

(require 'color-theme-tangotango)
;; load theme when we are started with $ emacsclient -c
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f)
                 (color-theme-tangotango)))))
;; load theme when we are started with $ emacs
(when window-system
  (color-theme-tangotango))

;; hide toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; show x-position (ie column number) for point in buffer
(column-number-mode 1)

(setq enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode 1)

(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

(defun highlight-symbol-first ()
  "Jump to the first location of symbol at point."
  (interactive)
  (push-mark)
  (eval
   `(progn
      (goto-char (point-min))
      (search-forward-regexp
       (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
       nil t)
      (beginning-of-thing 'symbol))))

(global-set-key (kbd "M-P") 'highlight-symbol-first)

(defun highlight-symbol-last ()
  "Jump to the last location of symbol at point."
  (interactive)
  (push-mark)
  (eval
   `(progn
      (goto-char (point-max))
      (search-backward-regexp
       (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
       nil t))))

(global-set-key (kbd "M-N") 'highlight-symbol-last)

(defun beginning-of-line-dwim ()
  "Toggles between moving point to the first non-whitespace character, and
the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; see if going to the beginning of the line changes our position
    (move-beginning-of-line nil)

    (when (= (point) start-position)
        ;; we're already at the beginning of the line, so go to the
        ;; first non-whitespace character
        (back-to-indentation))))

(global-set-key (kbd "C-a") 'beginning-of-line-dwim)

(require 'jump-char)

(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)

(keyfreq-mode)

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "M-o") 'smart-open-line)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-O") 'smart-open-line-above)

(defadvice kill-line (around kill-line-remove-newline activate)
  (let ((kill-whole-line t))
    ad-do-it))

(defun kill-or-delete-region (beg end prefix)
  "Delete the region, storing it in the kill-ring.
If a prefix argument is given, don't change the kill-ring."
  (interactive "r\nP")
  (if prefix
      (delete-region beg end)
    (kill-region beg end)))

(global-set-key (kbd "C-w") 'kill-or-delete-region)

(require 'smartparens)

;; (foo bar) -> foo bar
(define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)

;; (foo bar) -> [foo bar]
(define-key smartparens-mode-map (kbd "M-S") 'sp-rewrap-sexp)

;; (foo) bar -> (foo bar)
(define-key smartparens-mode-map (kbd "<C-right>") 'sp-slurp-hybrid-sexp)

(require 'smartparens-config)
(require 'smartparens-html)
(add-hook 'prog-mode-hook 'smartparens-mode)

(setq sp-autoescape-string-quote nil)

(require 'recentf)

;; offer recently accessed files from the menu
(recentf-mode t)

;; remember this many files
(setq recentf-max-saved-items 200)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(require 'find-file-in-repository)

(defadvice find-file-in-repository (around disable-ido-flex-matching activate)
  (let ((ido-enable-flex-matching nil)
        (ido-case-fold t))
    ad-do-it))

(global-set-key (kbd "C-x C-g") 'find-file-in-repository)

(require 'dired+)

(setq delete-by-moving-to-trash t)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

(defun start-scratch-file (file-name)
  "Create a file in ~/scratch for the given file name."
  (interactive "sName of scratch file: ")
  (let ((path (expand-file-name (format "~/scratch/%s" file-name))))
    ;; create directories as necessary
    (when (s-contains-p "/" file-name)
      (make-directory (file-name-directory path) t))
    (find-file path)))

(defun start-scratch-html-file (file-name)
  "Create a test HTML file in ~/scratch to play around with."
  (interactive "sName of scratch HTML file: ")
  (start-scratch-file file-name)
  (erase-buffer)
  (insert "<!DOCTYPE html>
<html>
    <head>
        <title>
        </title>
        <style type=\"text/css\">
        </style>
    </head>
    <body>
        
    </body>
</html>")
  (forward-line -2)
  (move-end-of-line nil))

(require 'flymake)
(global-set-key (kbd "<f8>") 'flymake-goto-prev-error)
(global-set-key (kbd "<f9>") 'flymake-goto-next-error)

(defun flymake-error-at-point ()
  "Show the flymake error in the minibuffer when point is on an invalid line."
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'flymake-error-at-point)

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))

(setq flycheck-highlighting-mode 'lines)

(custom-set-faces
 '(flycheck-error ((((class color)) (:underline "Red"))))
 '(flycheck-warning ((((class color)) (:underline "Orange")))))

(require 'flycheck)
(define-key flycheck-mode-map (kbd "<f8>") 'flycheck-previous-error)
(define-key flycheck-mode-map (kbd "<f9>") 'flycheck-next-error)

(require 'undo-tree)
(global-undo-tree-mode)

(define-key emacs-lisp-mode-map (kbd "C-c e") 'eval-defun)

(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)

(define-key emacs-lisp-mode-map (kbd "C-c m") 'macrostep-expand)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (paredit-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "white")
(set-face-foreground 'rainbow-delimiters-depth-2-face "cyan")
(set-face-foreground 'rainbow-delimiters-depth-3-face "yellow")
(set-face-foreground 'rainbow-delimiters-depth-4-face "green")
(set-face-foreground 'rainbow-delimiters-depth-5-face "orange")
(set-face-foreground 'rainbow-delimiters-depth-6-face "purple")
(set-face-foreground 'rainbow-delimiters-depth-7-face "white")
(set-face-foreground 'rainbow-delimiters-depth-8-face "cyan")
(set-face-foreground 'rainbow-delimiters-depth-9-face "yellow")
(set-face-foreground 'rainbow-delimiters-unmatched-face "red")

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

(setq-default flycheck-emacs-lisp-initialize-packages t)

(require 'flycheck)
(setq flycheck-checkers (--remove (eq it 'emacs-lisp-checkdoc) flycheck-checkers))

(eval-after-load "dash" '(dash-enable-font-lock))

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(setenv "PYFLAKES_NODOCTEST" "y")

(require 'python)

(define-skeleton python-insert-docstring
  "Insert a Python docstring."
  "This string is ignored!"
  "\"\"\"" - "\"\"\"")

(define-key python-mode-map (kbd "C-c s") 'python-insert-docstring)

(add-hook 'haskell-mode-hook 'flycheck-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

(require 'sgml-mode)

; indent html with tabs only
(add-hook 'html-mode-hook
  (function
   (lambda ()
     (progn
       (setq indent-tabs-mode nil)
       (setq sgml-basic-offset 4)))))

(require 'smartparens-config)
(add-hook 'html-mode-hook 'smartparens-mode)

(add-to-list 'auto-mode-alist '("\\.dtml$" . html-mode))

;; Define coloured faces for Django syntax.
(defvar django-tag-face (make-face 'django-tag-face))
(set-face-foreground 'django-tag-face "Orange")
;
(defvar django-variable-face (make-face 'django-variable-face))
(set-face-foreground 'django-variable-face "Green")

(defvar django-comment-face (make-face 'django-comment-face))
(set-face-foreground 'django-comment-face "Gray")

;; Use these faces for Django syntax.  
(font-lock-add-keywords
 'html-mode
 '(
   ("\\({%[^%]*%}\\)" 1 django-tag-face prepend)
   ("\\({{[^}]*}}\\)" 1 django-variable-face prepend)
   ("\\({#[^}]*#}\\)" 1 django-comment-face prepend)
   ("\\({% comment %}\\(.\\|
\\)*{% endcomment %}\\)" 1 django-comment-face prepend)
   ))

; skeletons for Django template tags
(define-skeleton template-tag-skeleton
  "Insert a {% foo %} template tag"
  "Template tag name: "
  "{% " str " %}")
(define-skeleton template-variable-skeleton
  "Insert a {{ foo }} template variable"
  "Template variable: "
  "{{ " str " }}")
(define-skeleton template-comment-skeleton
  "Insert a {# foo #} template variable"
  "Comment: "
  "{# " str " #}")
(define-skeleton template-block-skeleton
  "Insert {% block foo %}{% endblock %}"
  "Block name: "
  "{% block " str " %}\n" - "\n{% endblock %}")
(define-skeleton template-if-else-skeleton
  "Insert {% if foo %}{% else %}{% endif %}"
  "If condition: "
  "{% if " str " %}\n" - "\n{% else %}\n\n{% endif %}")
(define-skeleton template-if-skeleton
  "Insert {% if foo %}{% endif %}"
  "If condition: "
  "{% if " str " %}" - "{% endif %}")
(define-skeleton underscore-skeleton
  "Insert <%= foo %>"
  "Contents: "
  "<%= " str " %>")

(defvar template-skeletons
  '(template-tag-skeleton
    template-variable-skeleton
    template-comment-skeleton
    template-block-skeleton
    template-if-skeleton
    template-if-else-skeleton
    underscore-skeleton))

(defun insert-django-skeleton ()
  (interactive)
  (let* ((skeleton-names (mapcar 'symbol-name template-skeletons))
        (skeleton-chosen (ido-completing-read "HTML skeleton: " skeleton-names)))
    (funcall (intern skeleton-chosen))))

(define-key html-mode-map "\C-ct" 'insert-django-skeleton)

(defun visit-parent-django-template ()
  "In a buffer containg {% extends \"foo.html\" %}, visit foo.html."
  (interactive)
  (let (start-pos end-pos template-name)
    (save-excursion
      (widen)
      (goto-char (point-min))
      ;; Find the extends tag
      (while (not (looking-at "{% ?extends"))
        (forward-char 1))
      ;; Find the opening " of the file name.
      (while (not (looking-at "\""))
        (forward-char 1))
      (forward-char)
      (setq start-pos (point))

      ;; Find the closing "
      (while (not (looking-at "\""))
        (forward-char 1))
      (setq end-pos (point))

      (setq template-name (buffer-substring-no-properties start-pos end-pos)))

    ;; Open this file, assuming it's in the same directory.
    ;; TODO: Search the current VCS checkout for it.
    (find-file template-name)))

(defun html-linkify-region (url)
  "Wraps the region in an <a> tag with href set to URL."
  (interactive "sURL: ")
  (let* (
         (initial-cursor-position (point))
         (beginning (region-beginning))
         (end (region-end))
         (first-replacement (concat "<a href=\"" url "\">"))
         (second-replacement "</a>"))
  (goto-char beginning)
  (insert first-replacement)
  (goto-char (+ end (length first-replacement)))
  (insert second-replacement)
  (goto-char (+ initial-cursor-position (length first-replacement)))
  ))

; zen coding: converts selector-style lines to tags
; e.g. table>tr*2 becomes <table><tr></tr><tr></tr></table>
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(add-hook 'css-mode-hook
          (function
           (lambda ()
             (progn
               (setq css-indent-offset 4)
               (setq indent-tabs-mode nil)))))

(add-hook 'css-mode-hook 'rainbow-mode)

(add-hook 'css-mode-hook 'smartparens-mode)

(require 'less-css-mode)
(add-hook 'less-css-mode-hook 'flymake-mode)

(add-hook 'markdown-mode-hook 'auto-fill-mode)

(setq gc-cons-threshold (* 10 1024 1024))

; always close the minibuffer on C-x o:
; <jlf> wilfredh: you could before-advise other-window to quit first
; if the minibuffer is active.. but it would be better to break that
; habit :)

(require 'ui-customisations)

(require 'file-customisations)
(require 'editing-customisations)
(require 'kill-ring-customisations)

(if (eq system-type 'darwin)
    (require 'os-x-fixes))

; TODO: increase kill ring size

(require 'ido-customisations)

;; make re-builder use the same regexp format as regexp-replace (no double escaping)
(setq reb-re-syntax 'string)

;; treat space charcters as matching space characters, not like PCRE's '\s+'
(setq search-whitespace-regexp nil)

(require 'completion-customisations)
(require 'structured-text-customisations)
(require 'isearch-customisations)
(require 'python-customisations)
(require 'javascript-customisations)
(require 'xml-customisations)
(require 'lisp-customisations)
(require 'c-customisations)
(require 'sh-customisations)
(require 'coffee-customisations)

(require 'startup-customisations)

(require 'git-customisations)
(require 'git-flow-release)
(require 'eshell-customisations)
(require 'swoop-customisations)

;; stolen from http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(setq ag-highlight-search 't)
(global-set-key (kbd "<f5>") 'ag-project-at-point)

;; stolen from http://whattheemacsd.com/setup-dired.el-02.html
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line (if dired-omit-mode 2 4)))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(require 'conflicts)

(require 'tags-utils)

;; crontab mode for files named
(require 'crontab-mode)
(add-to-list 'auto-mode-alist '("crontab.*?\\'" . crontab-mode))

(require 'wiki)
(setq major-mode 'text-mode)
(setq wiki-directories (list (expand-file-name "~/Dropbox/Wiki/")))

(setq ring-bell-function 'ignore)

; suspend seems to crash on Gnome 3, and I don't use it anyway, so just disable it
(setq cannot-suspend t)
(defun suspend-emacs (&rest)
  (interactive))
(defun suspend-frame (&rest)
  (interactive))

;; windmove allows S-<right> and S-<right> to switch between windows
;; instead of `C-x o'
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-plugin-defalt-face ((t nil)))
 '(ethan-wspace-face ((t (:background "#2e3434"))))
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange"))))
 '(hl-line ((t (:background "gray14"))))
 '(hl-sexp-face ((t (:background "gray14"))))
 '(js2-function-param-face ((((class color)) (:foreground "Green"))))
 '(smerge-refined-change ((t (:background "black")))))

;; (require 'timing)
;; (message "Spent %.2f seconds executing .emacs.d/init.el."
;;          (time-difference *emacs-load-start* (current-time)))
(put 'narrow-to-region 'disabled nil)

(defun indent-buffer ()
  "Indent the everything in the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(add-hook 'magit-log-edit-mode-hook 'auto-fill-mode)

(require 'f)
(require 's)

(defun download-file (url directory file-name)
  "Download the file at URL into DIRECTORY.
The FILE-NAME defaults to the one used in the URL."
  (interactive
   ;; We're forced to let-bind url here since we access it before
   ;; interactive binds the function parameters.
   (let ((url (read-from-minibuffer "URL: ")))
     (list
      url
      (read-directory-name "Destination dir: ")
      ;; deliberately not using read-file-name since that inludes the directory
      (read-from-minibuffer
       "File name: "
       (car (last (s-split "/" url)))))))
  (let ((destination (f-join directory file-name)))
    (url-copy-file url destination 't)
    (find-file destination)))

(setq-default dired-listing-switches "-alhv")

(global-anzu-mode +1)
