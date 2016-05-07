(require 'org)

;; Allow running code in sh and python
(require 'ob-sh)
(require 'ob-python)

(use-package org
  :config
  ;; When clocking in, just use the time from the last clocked out
  ;; item.
  (setq org-clock-continuously t)

  ;; When creating or completing a TODO, record the timestamps.
  (setq org-log-done 'time)

  ;; Syntax highlight org code snippets.
  (setq org-src-fontify-natively t))

(custom-set-faces
 '(org-date ((((class color)) (:underline nil))) t))

(provide 'org-customisations)
