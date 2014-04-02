(require 'magit)

(global-set-key (kbd "<f2>") 'magit-status)

(setq magit-completing-read-function 'magit-ido-completing-read)
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(setq magit-turn-on-auto-revert-mode nil)

(provide 'git-customisations)

