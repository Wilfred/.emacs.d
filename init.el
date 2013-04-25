(setq *emacs-load-start* (current-time))


(add-to-list 'load-path "~/.emacs.d/third-party-lisp/")
(add-to-list 'load-path "~/.emacs.d/user-lisp/")

; marmalade is a third party repo that anyone can submit to, so has
; many more packages
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; If any packages installed with package.el aren't compiled yet, compile them.
(byte-recompile-directory package-user-dir 0)

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

(setq ag-highlight-search 't)
(global-set-key (kbd "<f5>") 'ag-project-at-point)

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
(setq default-major-mode 'text-mode)
(setq wiki-directories (list (expand-file-name "~/Dropbox/Wiki/")))

(setq ring-bell-function 'ignore)

; something in the above has switched on debugging. Switch it off.
; TODO: find offending code
(setq debug-on-error nil)

; suspend seems to crash on Gnome 3, and I don't use it anyway, so just disable it
(setq cannot-suspend t)
(defun suspend-emacs (&rest)
  (interactive))
(defun suspend-frame (&rest)
  (interactive))

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
 '(js2-function-param-face ((((class color)) (:foreground "Green"))))
 '(smerge-refined-change ((t (:background "black")))))

;; (require 'timing)
;; (message "Spent %.2f seconds executing .emacs.d/init.el."
;;          (time-difference *emacs-load-start* (current-time)))
