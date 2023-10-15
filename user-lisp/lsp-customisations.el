
;; eglot
;; 
;; (load "package") ; workaround on Emacs 28

(use-package eglot
  :config
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  (global-flycheck-eglot-mode)

  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format-buffer)

  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  ;; Mnemonic: show references in Xref.
  (define-key eglot-mode-map (kbd "C-c x") #'xref-find-references)

  ;; Mnemonic: action.
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions))

;; Use clippy for the check command in rust-analyzer, so we get yellow squiggles.
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

(provide 'lsp-customisations)
