(require 'highlight-symbol)

;; todo: switch on highlight-symbol mode automatically
(highlight-symbol-mode 1)

(global-set-key (kbd "<f12>") 'highlight-symbol-next)

(eval-after-load "isearch" '(require 'isearch+))

(provide 'isearch-customisations)
