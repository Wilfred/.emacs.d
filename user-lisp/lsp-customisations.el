
(setq lsp-modeline-code-actions-segments '(count name))

(setq lsp-rust-clippy-preference "on")

(set-face-attribute 'lsp-lsp-flycheck-warning-unnecessary-face nil :foreground nil)

;; Don't override my navigation bindings.
(define-key lsp-signature-mode-map (kbd "M-n") nil)
(define-key lsp-signature-mode-map (kbd "M-p") nil)

(define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
