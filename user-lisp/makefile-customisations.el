(use-package make-mode
  :config
  (define-key makefile-mode-map (kbd "M-n") #'highlight-symbol-next)
  (define-key makefile-mode-map (kbd "M-p") #'highlight-symbol-next))

(provide 'makefile-customisations)
