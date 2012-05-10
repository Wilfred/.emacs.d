(add-to-list 'load-path (expand-file-name "~/.emacs.d/third-party-lisp/emacs-eclim"))
(require 'eclim)

(setq eclim-executable (expand-file-name "~/.eclipse/org.eclipse.platform_3.7.0_155965261/eclim"))

(global-eclim-mode)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(add-hook 'java-mode-hook
	  '(lambda ()
             (camelCase-mode)
	     (define-key java-mode-map (kbd "<f6>") 'eclim-java-find-declaration)))

(provide 'java-customisations)