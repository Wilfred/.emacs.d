;;; conflicts.el -- Conveniences for handling files with conflicts

;; This is just a wrapper around smerge, but with more memorable
;; command names and more convenient key bindings.

(defun conflicts-list ()
  "Show all the conflicts in the current buffer using occur-mode."
  (interactive)
  (occur "<<<<<<< ")

  ; put mark on the first result in the occur window
  (other-window 1)
  (forward-line))

(global-set-key (kbd "<f10>") 'conflicts-list)

(defalias 'conflicts-keep-current 'smerge-keep-current)

;; TODO: it would be nice if these jumped to the first conflict
(defalias 'conflicts-keep-first 'smerge-keep-mine)
(defalias 'conflicts-keep-second 'smerge-keep-other)

(require 'smerge-mode)
(define-key smerge-mode-map (kbd "<C-return>") 'conflicts-keep-current)
(define-key smerge-mode-map (kbd "<f8>") 'smerge-prev)
(define-key smerge-mode-map (kbd "<f9>") 'smerge-next)

;; TODO: a function to find all files containing merge conflicts in a repo

;; TODO: SVN conflicts seem to enable smerge-mode, but not git conflicts

(provide 'conflicts)
