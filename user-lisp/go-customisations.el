(add-hook 'go-mode-hook #'flycheck-mode)

;; Go is indented with tabs, so set the tab size in those buffers.
(defun wh/set-go-tab-width ()
  (setq tab-width 4))
(add-hook 'go-mode-hook #'wh/set-go-tab-width)

(provide 'go-customisations)
