
(use-package lsp-mode
  :config
  ;; For some reason icons aren't currently rendering, so don't show them.
  (setq lsp-modeline-code-actions-segments '(count name))

  ;; Setting this to "clippy" can suppress errors in a crate if there
  ;; are any errors in build.rs.
  ;; https://github.com/rust-lang/rust-clippy/issues/9534
  (setq lsp-rust-analyzer-cargo-watch-command "check")

  (setq lsp-rust-clippy-preference "on")

  (set-face-attribute 'lsp-lsp-flycheck-warning-unnecessary-face nil :foreground nil)

  ;; Don't override my navigation bindings.
  (define-key lsp-signature-mode-map (kbd "M-n") nil)
  (define-key lsp-signature-mode-map (kbd "M-p") nil)

  (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)
  ;; Mnemonic: show references in Xref.
  (define-key lsp-mode-map (kbd "C-c x") #'lsp-find-references)

  ;; Mnemonic: action.
  (define-key lsp-mode-map (kbd "C-c a") #'lsp-execute-code-action)

  ;; Disable intrusive signature popup
  ;; https://github.com/emacs-lsp/lsp-mode/issues/1535
  (setq lsp-signature-auto-activate nil)

  ;; Disable icons in the headerline: I find them redundant.
  (setq lsp-headerline-breadcrumb-icons-enable
        nil)

  ;; Not sure I want this.
  (setq lsp-ui-doc-enable t)
  ;; Maybe a bigger delay?
  (setq lsp-ui-doc-delay 3.0)

  ;; Code actions are already in the modeline.
  (setq lsp-ui-sideline-show-code-actions nil)

  ;; Limit the headerline to things inside the current file.
  ;; https://emacs-lsp.github.io/lsp-mode/page/main-features/#breadcrumb-on-headerline
  (setq lsp-headerline-breadcrumb-segments '(symbols))

  (add-hook 'rust-mode-hook #'lsp))

;; eglot
;; 
;; (load "package") ; workaround on Emacs 28

(use-package 'eglot
  :config
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  (global-flycheck-eglot-mode)

  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format-buffer)

  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  ;; Mnemonic: show references in Xref.
  (define-key eglot-mode-map (kbd "C-c x") #'xref-find-references)

  ;; Mnemonic: action.
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions))

(provide 'lsp-customisations)
