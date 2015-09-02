(require 'magit)

;; Use F2 to open magit.
(global-set-key (kbd "<f2>") 'magit-status)

;; Don't prompt when first line of commit is over 50 chars.
(setq git-commit-finish-query-functions '())

;; Disabling magit nagging when pushing.
(setq magit-push-always-verify nil)

(setq magit-branch-arguments (remove "--track" magit-branch-arguments))

;; Highlight new/removed/changed lines relative to the last commit in VCS.
(global-diff-hl-mode)

;; Include 'x' in the magit popup.
;; From https://github.com/magit/magit/issues/2141
(magit-define-popup-action 'magit-dispatch-popup
  ?x "Reset" 'magit-reset ?!)

;; Default colours are too subtle, make them obvious.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "blue3" :foreground "blue3"))))
 '(diff-hl-delete ((t (:inherit diff-removed :background "red3" :foreground "red3"))))
 '(diff-hl-insert ((t (:inherit diff-added :background "green4" :foreground "green4")))))

(provide 'git-customisations)

