
(use-package lsp-mode
  :config
  ;; For some reason icons aren't currently rendering, so don't show them.
  (setq lsp-modeline-code-actions-segments '(count name))

  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-clippy-preference "on")

  (set-face-attribute 'lsp-lsp-flycheck-warning-unnecessary-face nil :foreground nil)

  ;; Don't override my navigation bindings.
  (define-key lsp-signature-mode-map (kbd "M-n") nil)
  (define-key lsp-signature-mode-map (kbd "M-p") nil)

  (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)

  (add-hook 'rust-mode-hook #'lsp-mode))

(provide 'lsp-customisations)
