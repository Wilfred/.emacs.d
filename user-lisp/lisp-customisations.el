;; paredit for both clojure and elisp
(autoload 'paredit-mode "paredit")
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)

;; eldoc displays the arguments to the function under cursor in the minibuffer
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; C-M-x is swallowed by Gnome, so bind C-c e instead
(define-key emacs-lisp-mode-map (kbd "C-c e") 'eval-defun)
;; toggle-debug-on-error is too useful to not have a keybinding
(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)
;; evaluate macros step-by-step
(define-key emacs-lisp-mode-map (kbd "C-c m") 'macrostep-expand)

(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; brighter colours, snaffled from solarized mode
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

(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(require 'nrepl)

(provide 'lisp-customisations)
