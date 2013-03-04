(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; indent with tab characters (treating them as eight spaces)
(setq-default js-indent-level 4)
(setq tab-width 4)

;; these are the default settings, but it's nice to be explicit for ease of customisation
(add-hook 'js-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
	     (setq js2-basic-offset 4)))

; fix js2-mode's function parameter colour, which is too dark for a dark theme
(custom-set-faces
 '(js2-function-param-face ((((class color)) (:foreground "Green")))))


(require 'flymake-jshint)

(custom-set-variables
 '(jshint-configuration-path "~/.emacs.d/user-js/jshint.json")
 '(js2-strict-missing-semi-warning nil))

(add-hook 'js-mode-hook 'flymake-mode)
(add-hook 'js2-mode-hook 'flymake-mode)


(provide 'javascript-customisations)
