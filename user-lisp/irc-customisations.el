

(use-package rcirc
  :config
  (setq rcirc-default-nick "wilfredh")
  (setq rcirc-server-alist
        '(("irc.freenode.net" :channels ("#emacs" "#guile"))
          ("irc.mozilla.org" :channels ("#rust"))))
  (setq rcirc-log-flag t)
  (setq rcirc-log-directory "~/irc_logs"))

(provide 'irc-customisations)
