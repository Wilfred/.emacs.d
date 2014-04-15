;; paredit for both clojure and elisp
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook 'turn-on-paredit)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq use-hl-line nil)
            (hl-sexp-mode)))
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

(provide 'lisp-customisations)
