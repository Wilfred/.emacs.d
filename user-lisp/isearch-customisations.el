(require 'highlight-symbol)

;; use highlight-symbol-mode in all programming modes
(add-hook 'prog-mode-hook '(lambda () (highlight-symbol-mode 1)))

(global-set-key (kbd "<f12>") 'highlight-symbol-next)
;; no delay before highlighting
(setq highlight-symbol-idle-delay 0)

(eval-after-load "isearch" '(require 'isearch+))

(provide 'isearch-customisations)
