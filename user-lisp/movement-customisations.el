

;; avy -- quickly jump to an arbitrary word or line
(require 'avy)
(setq avy-case-fold-search nil)
(setq avy-keys
      (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
(define-key global-map (kbd "<f11>") #'avy-goto-word-or-subword-1)
(define-key global-map (kbd "C-c <f11>") #'avy-goto-line)

(provide 'movement-customisations)
