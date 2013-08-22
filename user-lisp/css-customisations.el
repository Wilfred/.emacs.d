(require 'css-mode)

;; indent CSS with tabs
(add-hook 'css-mode-hook
          (function
           (lambda ()
             (progn
               (setq css-indent-offset 4)
               (setq indent-tabs-mode nil)))))

;; highlight colour values to show the colour they represent
(add-hook 'css-mode-hook 'rainbow-mode)

(provide 'css-customisations)
