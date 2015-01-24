;; todo: show eldoc in modeline, since the minibuffer is used by
;; flycheck.
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(add-hook 'c-mode-hook #'which-function-mode)

(provide 'c-customisations)
