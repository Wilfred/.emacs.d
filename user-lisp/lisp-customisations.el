;; Use paredit for all the lisp modes I use.
(defun switch-on-paredit ()
  (turn-off-smartparens-mode)
  (smartparens-strict-mode -1)
  (paredit-mode 1))

(dolist (hook
         '(clojure-mode-hook
           emacs-lisp-mode-hook
           scheme-mode-hook
           lisp-mode-hook
           ielm-mode-hook))
  (add-hook hook 'switch-on-paredit))

;; Paredit's C-M-f and C-M-b are really handy but difficult to type.
(global-set-key (kbd "s-f") #'paredit-forward)
(global-set-key (kbd "s-b") #'paredit-backward)
;; Likewise for other sexp commands.
(global-set-key (kbd "s-u") #'paredit-backward-up)
(global-set-key (kbd "s-d") #'paredit-forward-down)
(global-set-key (kbd "<s-backspace>") #'backward-kill-sexp)
(global-set-key (kbd "s-k") #'kill-sexp)

(require 'highlight-quoted)
;; Highlight the ' character itself in the same colour
;; as the quoted symbol.
(set-face-attribute 'highlight-quoted-quote nil
  :inherit 'highlight-quoted-symbol)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq use-hl-line nil)
            (hl-sexp-mode)
            (highlight-quoted-mode)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq use-hl-line nil)
            (hl-sexp-mode)))

;; rather than using TAGS, jump to function definitions that we have
;; loaded
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(require 'diminish)
(diminish 'elisp-slime-nav-mode)

(add-hook 'ielm-mode-hook #'company-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(require 'cider)

;; Trifle configuration
(require 'trifle-mode)

;; convenience function for Trifle indentation
;; todo: full-blown indentation functionality
(defun remove-indent ()
  (interactive)
  (back-to-indentation)
  (let ((indent-size (- (point) (line-beginning-position))))
    (delete-backward-char indent-size)))

;; Common Lisp configuration
(setq inferior-lisp-program "/usr/bin/sbcl")

(require 'slime)
(define-key slime-mode-map (kbd "C-c e") 'slime-eval-defun)

;; Ensure elisp code is continuously indented.
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(require 'company)
(require 'company-elisp)

;; Use hungry-delete in elisp too.  There is also
;; `turn-on-hungry-delete-mode', but it seems that paredit's mode map
;; is coming first.
(define-key paredit-mode-map (kbd "C-d") 'hungry-delete-forward)

(provide 'lisp-customisations)
