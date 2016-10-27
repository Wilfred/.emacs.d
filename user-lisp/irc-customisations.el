

(use-package rcirc
  :config
  (setq rcirc-default-nick "wilfredh")
  (setq rcirc-server-alist
        '(("irc.freenode.net" :channels ("#emacs" "#guile"))
          ("irc.mozilla.org" :channels ("#rust"))))
  ;; Keep history.
  (setq rcirc-log-flag t)
  (setq rcirc-log-directory "~/irc_logs")
  ;; Ignore away/join/part messages from lurkers.
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (require 'rcirc-color))

(provide 'irc-customisations)
