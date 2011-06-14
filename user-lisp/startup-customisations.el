; no startup screen
(setq-default inhibit-startup-screen t)

; scratch buffer should be empty on startup
(setq-default initial-scratch-message nil)

; Python mode is more useful for the scratch buffer
(setq-default initial-major-mode 'python-mode)

(provide 'startup-customisations)