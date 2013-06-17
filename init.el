
(add-to-list 'load-path "~/.emacs.d/user-lisp/")

(add-to-list 'load-path "~/.emacs.d/third-party-lisp/")

; marmalade is a third party repo that anyone can submit to, so has
; many more packages
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; load the packages we've installed on another system but pulled in with git, so they aren't compiled
(eval-when-compile '(require 'cl))
(require 'dash)
(require 's)

(defun was-compiled-p (path)
  "Does the directory at PATH contain any .elc files?"
  (--any-p (s-ends-with-p ".elc" it) (directory-files path)))

(defun no-dot-directories (directories)
  "Exclude the . and .. directory from a list."
  (--remove (or (string= "." (file-name-nondirectory it))
                (string= ".." (file-name-nondirectory it)))
            directories))

(defun ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (let* ((package-files (no-dot-directories (directory-files package-user-dir t)))
         (package-directories (-filter 'file-directory-p package-files)))
    (dolist (directory package-directories)
      (unless (was-compiled-p directory)
        (byte-recompile-directory directory 0)))))

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

(require 'jump-char)

(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)

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

(require 'undo-tree)
(global-undo-tree-mode)

(require 'autopair)
(require 'python)
(setq autopair-autowrap t)
(add-hook 'python-mode-hook 'autopair-mode)

(setq jedi:setup-keys t)
(require 'jedi)
(setq jedi:server-command
      (list "python2" jedi:server-script))
(add-hook 'python-mode-hook 'jedi:setup)

(require 'flymake-python-pyflakes)
(setq flymake-python-pyflakes-executable "pyflakes")
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(setenv "PYFLAKES_NODOCTEST" "y")

(require 'less-css-mode)
(add-hook 'less-css-mode-hook 'flymake-mode)

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

(defun start-scratch-file (file-name)
  "Create a file in ~/scratch for the given file name."
  (interactive "sName of scratch file: ")
  (let ((path (expand-file-name (format "~/scratch/%s" file-name))))
    ;; create directories as necessary
    (when (s-contains-p "/" file-name)
      (make-directory (file-name-directory path) t))
    (find-file path)))

; TODO: increase kill ring size

(require 'ido-customisations)

;; make re-builder use the same regexp format as regexp-replace (no double escaping)
(setq reb-re-syntax 'string)

;; treat space charcters as matching space characters, not like PCRE's '\s+'
(setq search-whitespace-regexp nil)

(require 'completion-customisations)
(require 'structured-text-customisations)
(require 'isearch-customisations)
(require 'html-customisations)
(require 'css-customisations)
(require 'python-customisations)
(require 'javascript-customisations)
(require 'xml-customisations)
(require 'lisp-customisations)
(require 'haskell-customisations)
(require 'c-customisations)
(require 'sh-customisations)
(require 'coffee-customisations)

(require 'startup-customisations)

(require 'editd-customisations)

(require 'git-customisations)

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
