

(use-package rcirc
  :config
  (setq rcirc-default-nick "wilfredh")
  (add-to-list 'rcirc-server-alist '("irc.mozilla.org" :channels ("#rust"))))
