
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

;; Doc comments are verbose, so only show the signature in the minibuffer.
;;
;; https://www.reddit.com/r/emacs/comments/11faie2/how_can_i_make_eglot_shut_up_in_the_minibuffer/jg0io6p/
(defun wh/eglot-managed-mode-initialize ()
  (setq-local
   eldoc-documentation-functions
   (list
    #'eglot-signature-eldoc-function
    ;; #'eglot-hover-eldoc-function
    ;; #'flymake-eldoc-function
    )))

(add-hook 'eglot-managed-mode-hook #'wh/eglot-managed-mode-initialize)

(provide 'lsp-customisations)
