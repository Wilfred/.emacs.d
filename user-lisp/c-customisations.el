(setq-default c-default-style "linux"
              c-basic-offset 4)

(add-hook 'c-mode-hook
	  '(lambda ()
	     (define-key c-mode-map (kbd "<f12>") 'recompile)))

(provide 'c-customisations)