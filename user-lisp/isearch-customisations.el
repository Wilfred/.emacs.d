(require 'highlight-symbol)

;; use highlight-symbol-mode in all programming modes
(add-hook 'prog-mode-hook '(lambda () (highlight-symbol-mode 1)))

;; no delay before highlighting
(setq highlight-symbol-idle-delay 0)

(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

(eval-after-load "isearch" '(require 'isearch+))

(provide 'isearch-customisations)
