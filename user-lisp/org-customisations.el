(require 'org)

(setq org-return-follows-link t)

;; Syntax highlight org code snippets.
(setq org-src-fontify-natively t)

;; Allow running code in sh and python
(require 'ob-sh)
(require 'ob-python)

(provide 'org-customisations)
