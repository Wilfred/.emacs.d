

(use-package rcirc
  :config
  (setq rcirc-default-nick "wilfredh")
  (setq rcirc-server-alist
        '(("irc.freenode.net" :channels ("#emacs"))
          ("irc.mozilla.org" :channels ("#rust")))))

(provide 'irc-customisations)
