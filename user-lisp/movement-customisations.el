;; avy -- quickly jump to an arbitrary word or line
(require 'avy)
(setq avy-case-fold-search nil)
;; Only consider the current window (i.e. the current buffer we're
;; focused in).
(setq avy-all-windows nil)
(setq avy-keys
      (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
(define-key global-map (kbd "<f11>") #'avy-goto-word-or-subword-1)
(global-set-key (kbd "<f10>") #'avy-goto-line)

;; Use `n' and `p' as movement keys in *compilation* buffers.
;; M-n and M-p are already bound, but n/p are less typing and ag.el
;; has trained me to expect those bindings.
(require 'compile)
(define-key compilation-mode-map (kbd "n") #'compilation-next-error)
(define-key compilation-mode-map (kbd "p") #'compilation-previous-error)

(require 'modalka-customisations)

(provide 'movement-customisations)
