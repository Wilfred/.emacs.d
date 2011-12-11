; use js2-mode from https://github.com/mooz/js2-mode
; todo: include a copy of the source, just in case
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; indent with tab characters (treating them as eight spaces)
(setq-default js-indent-level 8)
(setq tab-width 8)

(add-hook 'js-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)
	     (setq js2-basic-offset 8)))

(require 'flymake-jshint)

(custom-set-variables
 '(jshint-configuration-path "~/.emacs.d/user-js/jshint.json"))

(add-hook 'js-mode-hook 'flymake-mode)
(add-hook 'js2-mode-hook 'flymake-mode)


(provide 'javascript-customisations)