; paredit for both clojure and elisp

(autoload 'paredit-mode "paredit")
(defun turn-on-paredit () (paredit-mode 1))

(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(provide 'lisp-customisations)
