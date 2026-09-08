;;; -*- lexical-binding: nil; -*-

(declare-function eglot-code-actions "eglot")
(declare-function eglot-find-typeDefinition "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot-rename "eglot")

;; eglot
;; 
;; (load "package") ; workaround on Emacs 28

(use-package eglot
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))
  (global-flycheck-eglot-mode)

  ;; Rendering in the margin (default) uses an emoji, making the line
  ;; to too tall and causing flicker.
  (setq eglot-code-action-indications '(eldoc-hint mode-line))

  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format-buffer)

  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  ;; Mnemonic: show references in Xref.
  (define-key eglot-mode-map (kbd "C-c x") #'xref-find-references)

  ;; Mnemonic: action.
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)

  (define-key eglot-mode-map (kbd "C-c C-t") #'eglot-find-typeDefinition))

(let ((rust-analyzer-path
       (if (eq system-type 'gnu/linux)
           ;; Prefer the system installed rust-analyzer over the
           ;; ~/.cargo/bin/rust-analyzer provided by rustup. On
           ;; Arch Linux, the system package is much newer.
           "/usr/bin/rust-analyzer"
         "~/.cargo/bin/rust-analyzer")))
  (add-to-list 'eglot-server-programs
               `((rust-ts-mode rust-mode) .
                 (,rust-analyzer-path
                  ;; Use clippy for the check command in rust-analyzer, so we get yellow squiggles.
                  :initializationOptions (:check (:command "clippy"))))))

(provide 'lsp-customisations)
