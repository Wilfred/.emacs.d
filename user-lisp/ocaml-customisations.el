(use-package tuareg
  :config
  ;; Don't move past the expression after evaluating. I usually want
  ;; to repeatedly eval the same expression.
  (setq tuareg-skip-after-eval-phrase nil))

(use-package merlin
  :config
  (define-key merlin-mode-map (kbd "M-.") #'merlin-locate)
  (define-key merlin-mode-map (kbd "M-,") #'merlin-pop-stack))

(add-hook 'tuareg-mode-hook #'merlin-mode)

;; Show types in eldoc, and highlight other references to the same
;; symbol.
(add-hook 'merlin-mode-hook #'merlin-eldoc-setup)

;; merlin-eldoc is more precise than highlight-symbol-mode, because it
;; understands shadowing.
(add-hook 'merlin-mode-hook
          (function (lambda () (highlight-symbol-mode -1))))
