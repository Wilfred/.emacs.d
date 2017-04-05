(add-hook 'go-mode-hook #'flycheck-mode)

;; Go is indented with tabs, so set the tab size in those buffers.
(defun wh/set-go-tab-width ()
  (setq tab-width 4))

(add-hook 'go-mode-hook #'wh/set-go-tab-width)

(require 'go-mode)

(setq gofmt-command "~/go/bin/goimports")
;; Using -s with goimports is not supported with upstream goimports.
;; See https://github.com/golang/go/issues/8759 . Instead, use
;; $ go get github.com/jzelinskie/tools/cmd/goimports
(setq gofmt-args (list "-s"))

(setq godef-command "~/go/bin/godef")
(provide 'go-customisations)
