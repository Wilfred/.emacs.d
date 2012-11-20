(require 'highlight-symbol)

;; use highlight-symbol-mode in all programming modes
(add-hook 'prog-mode-hook '(lambda () (highlight-symbol-mode 1)))

(global-set-key (kbd "<f12>") 'highlight-symbol-next)

(eval-after-load "isearch" '(require 'isearch+))

(provide 'isearch-customisations)
