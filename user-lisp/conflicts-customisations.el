;;; conflicts-customisations.el -- Conveniences for handling files with conflicts

(defun wh/conflicts-list ()
  "Show all the conflicts in the current buffer using occur-mode."
  (interactive)
  (occur "<<<<<<< ")

  ;; put mark on the first result in the occur window
  (other-window 1)
  (forward-line))

(use-package smerge-mode
  :config
  ;; TODO: it would be nice if this jumped to the next conflict
  (define-key smerge-mode-map (kbd "<C-return>") #'smerge-keep-current)
  (define-key smerge-mode-map (kbd "<M-return>") #'smerge-keep-current)
  (define-key smerge-mode-map (kbd "C-c <C-return>") #'smerge-keep-all)
  (define-key smerge-mode-map (kbd "<f8>") #'smerge-prev)
  (define-key smerge-mode-map (kbd "<f9>") #'smerge-next))

(provide 'conflicts-customisations)
