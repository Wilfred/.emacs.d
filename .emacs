; Loading third party code
; ------------------------
; we will store all our 3rd party modes here
(add-to-list 'load-path "~/.emacs.d/user-lisp/")
; some plugins (at least w3) install themselves here:
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

; Interface
; ---------
; hide toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)
; show x-position (ie column number) for point in buffer
(column-number-mode 1)
; always highlight matching parentheses
(show-paren-mode 1)
; no startup screen
(setq-default inhibit-startup-screen t)
; always highlight line that cursor is on
(global-hl-line-mode 1)
; always truncate lines
(setq-default truncate-lines t)
; colour scheme
(require 'color-theme)
(require 'color-theme-tango)
(color-theme-tango)
; show file name in window title
(setq frame-title-format "%b - emacs")

; top of kill ring should also be in X clipboard
(setq x-select-enable-clipboard t)

; Editing conveniences
; --------------------
; ido-mode -- fuzzy completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode)
; zap-to-char but don't delete the character itself
(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
		 (progn
		   (forward-char direction)
		   (unwind-protect
		       (search-forward (char-to-string char) nil nil arg)
		     (backward-char direction))
		   (point)))))
; bind it to the usual zap-to-char shortcut
(global-set-key "\M-z" 'zap-up-to-char)


; always spaces, never tabs
(setq-default indent-tabs-mode nil)

; Text formatting modes
; ---------------------
; csv mode stuff, since it's used extensively in GBBO
(require 'csv-mode)
; yaml mode stuff, since google app engine uses it
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

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

; HTML modes / Django templates
; -----------------------------
; nXhtml, which includes Django template highlighting
(load "~/.emacs.d/user-lisp/nxhtml/autostart.el")
; no horrible background highlighting on html major mode
(custom-set-faces
 '(mumamo-background-chunk-major 
   ((((class color) (min-colors 88) (background dark)) (:background "*")))))

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

; better git handling
; note that you will still need to install magit.info manually
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

; something in the above has switched on debugging. Switch it off.
; TODO: find offending code
(setq debug-on-error nil)
