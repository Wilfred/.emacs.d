; top of kill ring should also be in X clipboard
(setq x-select-enable-clipboard t)

; ido-mode -- fuzzy completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode)

; we will store all our 3rd party modes here
(add-to-list 'load-path "~/.emacs.d/user-lisp/")

; some plugins (at least w3) install themselves here:
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

; theme stuff
(require 'color-theme)
(require 'color-theme-tango)
(color-theme-tango)

; always spaces, never tabs
(setq-default indent-tabs-mode nil)

; note: to get tabs (use whitespace-mode to check and (tabify) to fix):
; use tabs rather than spaces:
; M-x set-variable indent-tabs-mode t
; indent to 8 spaces
; M-x set-variable js-indent-level 8

; remove extraneous GUI elements
(tool-bar-mode 0)
(scroll-bar-mode 0)

; csv mode stuff, since it's used extensively in GBBO
(require 'csv-mode)

; yaml mode stuff, since google app engine uses it
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

; show x-position (ie column number) for point in buffer
(column-number-mode 1)

; always highlight matching parentheses
(show-paren-mode 1)

; always highlight line that cursor is on
(global-hl-line-mode 1)

; always truncate lines
(setq-default truncate-lines t)

; Python
; ------
; indent python by 4 spaces by default
(setq-default python-indent 4)
; use pyflakes to check code (requires pyflakes installed and on $PATH)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook 'flymake-mode)

; indent JavaScript tabs (treating them as eight spaces)
(setq-default js-indent-level 8)
(add-hook 'js-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

; .dtml are our Django templates which are mostly HTML
(setq auto-mode-alist
      (append
       '(("\\.dtml\\'" . html-mode))
       auto-mode-alist))

; django template tags
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
(global-set-key "\C-ctt" 'template-tag-skeleton)
(global-set-key "\C-ctv" 'template-variable-skeleton)
(global-set-key "\C-ctc" 'template-comment-skeleton)

; zen coding in HTML mode
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

; indent html with tabs only
(add-hook 'html-mode-hook
  (function
   (lambda ()
     (progn
       (setq indent-tabs-mode t)
       (setq sgml-basic-offset 8)))))

(require 'magit)

; show contents of kill ring
(defun show-kill-ring ()
  "Show the contents of the kill ring in a pop-up"
  (interactive)
  (popup-menu 'yank-menu))
(global-set-key "\C-cy" 'show-kill-ring)

; clojure mode and other lisp necessities
(require 'clojure-mode)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
