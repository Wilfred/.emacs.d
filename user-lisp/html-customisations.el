(use-package sgml-mode
  :init
  ;; Don't bind C-c C-n, I use it for renaming files.
  (define-key html-mode-map (kbd "C-c C-n") nil))

(provide 'html-customisations)
