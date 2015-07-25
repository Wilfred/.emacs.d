;; avy -- quickly jump to an arbitrary word or line
(require 'avy)
(setq avy-case-fold-search nil)
(setq avy-keys
      (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
(define-key global-map (kbd "<f11>") #'avy-goto-word-or-subword-1)
(define-key global-map (kbd "C-c <f11>") #'avy-goto-line)

;; I use next/previous-line and forward/backward-char an awful lot
;; according to `keyfreq-show'. Allow these commands to be repeated,
;; so 'C-n n' is equivalent to 'C-n C-n' and so on.
;;
;; Similar to god-mode, but specialised to just a whitelist of
;; movement commands.
(global-set-key
 (kbd "C-n")
 (defhydra hydra-move
   (:body-pre (next-line))
   "move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line-dwim)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom)))


(provide 'movement-customisations)
