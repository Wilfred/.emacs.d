;; Taken from
;; https://github.com/s1n4/dotfiles/blob/master/emacs.d/config/rcirc-config.el
(defun wh/log-filename-with-date (process target)
  (format
   "%s_%s.log"
   (if target
       (rcirc-generate-new-buffer-name process target)
     (process-name process))
   (format-time-string "%Y-%m-%d")))

(use-package rcirc
  :config
  (setq rcirc-default-nick "wilfredh")
  (setq rcirc-server-alist
        '(("irc.freenode.net" :channels ("#emacs" "#guile"))
          ("irc.mozilla.org" :channels ("#rust"))))
  ;; Keep history.
  (setq rcirc-log-flag t)
  (setq rcirc-log-directory "~/irc_logs")
  (setq rcirc-log-filename-function #'wh/log-filename-with-date)
  ;; Ignore away/join/part messages from lurkers.
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (require 'rcirc-color))

(provide 'irc-customisations)
