(use-package sgml-mode
  :init
  ;; Don't bind C-c C-n, I use it for renaming files.
  (define-key html-mode-map (kbd "C-c C-n") nil))

(add-hook 'mhtml-mode-hook #'prettier-js-mode)

(provide 'html-customisations)
