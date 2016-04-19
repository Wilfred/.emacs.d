;; top of kill ring should also be in X clipboard
(setq x-select-enable-clipboard t)

;; If the current value is the same as the head of the kill ring,
;; don't push the duplicate.
(setq kill-do-not-save-duplicates t)

(setq kill-ring-max 500)

(provide 'kill-ring-customisations)
