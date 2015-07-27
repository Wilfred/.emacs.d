(require 'magit)

;; Use F2 to open magit.
(global-set-key (kbd "<f2>") 'magit-status)

;; Don't prompt when first line of commit is over 50 chars.
(setq git-commit-finish-query-functions '())

(setq magit-branch-arguments (remove "--track" magit-branch-arguments))

(provide 'git-customisations)

