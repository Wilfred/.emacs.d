(require 'org)

(setq org-return-follows-link t)

;; Syntax highlight org code snippets.
(setq org-src-fontify-natively t)

;; Allow running code in sh and python
(require 'ob-sh)
(require 'ob-python)

(custom-set-faces
 '(org-date ((((class color)) (:underline nil))) t))

(provide 'org-customisations)
