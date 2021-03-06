(use-package go-mode
  :config
  ;; Jump to definitions.
  (setq godef-command "~/go/bin/godef")
  (define-key go-mode-map (kbd "M-.") #'godef-jump)

  ;; Run flycheck as you type.
  (add-hook 'go-mode-hook #'flycheck-mode)

  ;; Go is indented with tabs, so set the tab size in those buffers.
  (defun wh/set-go-tab-width ()
    (setq tab-width 4))
  (add-hook 'go-mode-hook #'wh/set-go-tab-width)
  ;; Use gocode for completion.

  (defun wh/setup-company ()
    (setq company-backends (list #'company-go)))
  (add-hook 'go-mode-hook #'wh/setup-company)

  ;; Show type of variable at point in the minibuffer.
  (add-hook 'go-mode-hook #'go-eldoc-setup)

  ;; Using -s with goimports is not supported with upstream goimports.
  ;; See https://github.com/golang/go/issues/8759. Instead, use
  ;; $ go get github.com/jzelinskie/tools/cmd/goimports
  (setq gofmt-command "~/go/bin/goimports")
  (setq gofmt-args (list "-s"))

  ;; TODO: send PR for this.
  (defun wh/gofmt-before-save ()
    (set (make-local-variable 'before-save-hook)
         (append before-save-hook (list #'gofmt-before-save))))
  (add-hook 'go-mode-hook #'wh/gofmt-before-save)

  (add-hook 'go-mode-hook #'rats-mode))

(use-package rats
  :config
  (define-key rats-mode-map (kbd "C-c r") #'rats-run-test-under-point))


(provide 'go-customisations)
