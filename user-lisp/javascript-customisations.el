(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))

;; for js-mode, which we use when editing JSON, use two space indents
(setq js-indent-level 2)

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

;; js2-mode offers a variety of warnings, but jshint is better at
;; this, so we switch of those in js2-mode.
(setq js2-strict-inconsistent-return-warning nil)
(setq js2-strict-missing-semi-warning 't)
(setq js2-strict-trailing-comma-warning nil)

(require 'flymake-jshint)

(custom-set-variables
 '(jshint-configuration-path "~/.emacs.d/user-js/jshint.json")
 '(js2-strict-missing-semi-warning t))

(add-hook 'js-mode-hook 'flymake-jshint-load)
(add-hook 'js2-mode-hook 'flymake-jshint-load)

(require 'company)
(add-to-list 'company-backends 'company-tern)

(defun wh/company-in-js2-mode ()
  (set (make-local-variable 'company-backends)
       (list #'company-tern #'company-dabbrev-code #'company-keywords )))

(add-hook 'js2-mode-hook #'wh/company-in-js2-mode)

(js2r-add-keybindings-with-prefix "C-c C-r")

(provide 'javascript-customisations)
