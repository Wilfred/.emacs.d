
(add-hook 'typescript-mode-hook #'tide-setup)

(add-hook 'typescript-mode-hook #'prettier-js-mode)

(remove-hook 'typescript-mode-hook #'prettier-js-mode)

(add-hook 'tide-mode-hook #'flycheck-mode)
(setq typescript-indent-level 2)
