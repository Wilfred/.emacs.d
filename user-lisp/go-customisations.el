(add-hook 'go-mode-hook #'flycheck-mode)

;; Go is indented with tabs, so set the tab size in those buffers.
(defun wh/set-go-tab-width ()
  (setq tab-width 4))

(add-hook 'go-mode-hook #'wh/set-go-tab-width)

(add-hook 'go-mode-hook #'go-eldoc-setup)

(require 'go-mode)

(setq gofmt-command "~/go/bin/goimports")
;; Using -s with goimports is not supported with upstream goimports.
;; See https://github.com/golang/go/issues/8759 . Instead, use
;; $ go get github.com/jzelinskie/tools/cmd/goimports
(setq gofmt-args (list "-s"))

;; TODO: send PR for this.
(defun wh/gofmt-before-save ()
  (set (make-local-variable 'before-save-hook)
       (append before-save-hook (list #'gofmt-before-save))))

(add-hook 'go-mode-hook #'wh/gofmt-before-save)

(setq godef-command "~/go/bin/godef")
(provide 'go-customisations)
