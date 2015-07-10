(require 'coffee-mode)

(setq coffee-tab-width 2)

(defun wh/company-in-coffee-mode ()
  (setq company-backends (list #'company-dabbrev-code)))

(add-hook 'coffee-mode-hook #'wh/company-in-coffee-mode)

(require 'flycheck)
(add-hook 'coffee-mode-hook #'flycheck-mode)

(provide 'coffee-customisations)
