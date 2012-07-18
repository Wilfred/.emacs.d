(defun conflicts-list ()
  "Show all the conflicts in the current buffer using occur-mode."
  (interactive)
  (occur "<<<<<<< ")

  ; put mark on the first result in the occur window
  (other-window 1)
  (next-line))

(global-set-key (kbd "<f10>") 'conflicts-list)

(defalias 'conflicts-keep-current 'smerge-keep-current)

;; TODO: it would be nice if these jumped to the first conflict
(defalias 'conflicts-keep-first 'smerge-keep-base)
(defalias 'conflicts-keep-second 'smerge-keep-other)

;; TODO: a function to find all files containing merge conflicts in a repo

;; TODO: SVN conflicts seem to enable smerge-mode, but not git conflicts

(provide 'conflicts)
