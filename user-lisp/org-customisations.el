(use-package org
  :config
  ;; When clocking in, just use the time from the last clocked out
  ;; item.
  (setq org-clock-continuously t)

  ;; Show drawers, e.g. :PROPERTIES:, when we expand a heading.
  ;; See http://emacs.stackexchange.com/a/22540/304
  (remove-hook 'org-cycle-hook #'org-cycle-hide-drawers)

  ;; When creating or completing a TODO, record the timestamps.
  (setq org-log-done 'time)

  ;; Syntax highlight org code snippets.
  (setq org-src-fontify-natively t)

  ;; Show *foo* and /foo/ without org markers, just the formatting.
  (setq org-hide-emphasis-markers t)

  ;; Allow running code in sh and python
  (require 'ob-sh)
  (require 'ob-python)

  ;; Don't underline dates, it's distracting.
  (custom-set-faces
   '(org-date ((((class color)) (:underline nil))) t)))

(define-key org-mode-map (kbd "C-c t") #'counsel-org-tag)

(require 'org-expiry)

(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "TODO")
              (save-excursion
                (org-back-to-heading)
                (org-expiry-insert-created)))))

(setq deft-directory "~/scratch")
(setq deft-default-extension "org")

(defun wh/org-today-time-stamp ()
  "Insert today's date as a time stamp."
  (interactive)
  (org-insert-time-stamp (current-time)))

(define-key org-mode-map (kbd "C-c .") #'wh/org-today-time-stamp)

(defun wh/org-today-subheading ()
  "Insert a subheading suitable for done.org."
  (interactive)
  (org-insert-heading-respect-content)
  (wh/org-today-time-stamp)
  (goto-char (line-beginning-position))
  (while (not (looking-at " "))
    (forward-char))
  (insert " "))

(define-key org-mode-map (kbd "C-c n") #'wh/org-today-subheading)

(provide 'org-customisations)
