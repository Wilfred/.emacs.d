(require 'org)

(setq org-return-follows-link t)

;; Syntax highlight org code snippets.
(setq org-src-fontify-natively t)

;; Allow running code in sh and python
(require 'ob-sh)
(require 'ob-python)

(use-package org
  :config
  (setq org-clock-continuously t))

(custom-set-faces
 '(org-date ((((class color)) (:underline nil))) t))

(provide 'org-customisations)
