; no startup screen
(setq-default inhibit-startup-screen t)

; scratch buffer should be empty on startup
(setq-default initial-scratch-message nil)

; rename the scratch buffer since I only use it for elisp
(rename-buffer "*scratch-elisp*")

; create a scratch Python buffer too
(get-buffer-create "*scratch-python*")

; Python mode is more useful for the scratch buffer
(setq-default initial-major-mode 'python-mode)

(provide 'startup-customisations)