(add-to-list 'load-path "~/.emacs.d/third-party-lisp/magit")
(autoload 'magit-status "magit")

(global-set-key (kbd "<f2>") 'magit-status)

(setq magit-completing-read-function 'magit-ido-completing-read)
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(provide 'git-customisations)

