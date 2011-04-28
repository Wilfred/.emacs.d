; indent CSS with tabs
(add-hook 'css-mode-hook
          (function
           (lambda ()
             (progn
               (setq css-indent-offset 8)
               (setq indent-tabs-mode t)))))

(provide 'css-customisations)