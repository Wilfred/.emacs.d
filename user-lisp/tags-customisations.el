;;; tags-customisations.el

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 1 October 2012

(use-package etags-select
  :bind (("<f6>" . etags-select-find-tag-at-point)
         ("M-." . etags-select-find-tag)
         ("M-," . pop-tag-mark)
         ("C-c M-." . etags-select-find-tag-other-window))
  :config
  (defadvice etags-select-find-tag (around case-sensitive-matching activate)
    (let ((ido-case-fold nil))
      ad-do-it))
  
  (defun etags-select-find-tag-other-window ()
    "Equivalent to `etags-select-find-tag-at-point' but
opening another window so the call site is still visible."
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (etags-select-find-tag-at-point))

  ;; finding tags should be case sensitive
  (setq tags-case-fold-search nil)

  ;; Always append tags to the tags table without prompting.
  (setq tags-add-tables t))


(provide 'tags-customisations)
