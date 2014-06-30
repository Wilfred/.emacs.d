(require 'magit)

(global-set-key (kbd "<f2>") 'magit-status)

(setq magit-completing-read-function 'magit-ido-completing-read)
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(setq magit-turn-on-auto-revert-mode nil)

;; Add `N' to the magit popup shown when you press `?'.
;; See https://github.com/magit/magit/issues/1396
(magit-key-mode-insert-action 'dispatch "N" "SVN" 'magit-key-mode-popup-svn)

(provide 'git-customisations)

