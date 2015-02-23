(require 'coffee-mode)

(setq coffee-tab-width 2)

(require 'flycheck)
(add-hook 'coffee-mode-hook #'flycheck-mode)

(provide 'coffee-customisations)
