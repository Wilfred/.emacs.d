
(setq *emacs-load-start* (current-time))

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

;; set exec-path according to the system's PATH
(exec-path-from-shell-initialize)

; always close the minibuffer on C-x o:
; <jlf> wilfredh: you could before-advise other-window to quit first
; if the minibuffer is active.. but it would be better to break that
; habit :)

(require 'ui-customisations)
(require 'startup-customisations)

(require 'file-customisations)
(require 'editing-customisations)
(require 'kill-ring-customisations)

(if (eq system-type 'darwin)
    (require 'os-x-fixes))

(defun start-scratch-file (file-name)
  "Create a file in ~/scratch for the given file name."
  (interactive "sName of scratch file: ")
  (find-file
   (format "~/scratch/%s" file-name)))

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

(global-set-key (kbd "<f8>") 'flymake-goto-prev-error)
(global-set-key (kbd "<f9>") 'flymake-goto-next-error)

(defun flymake-error-at-point ()
  "Show the flymake error in the minibuffer when point is on an invalid line."
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'flymake-error-at-point)

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
