; no startup screen
(setq-default inhibit-startup-screen t)

; scratch buffer should be empty on startup
(setq-default initial-scratch-message nil)

; rename the scratch buffer since I only use it for elisp
(switch-to-buffer (get-buffer-create "*scratch-elisp*"))
(emacs-lisp-mode)

; create a scratch Python buffer too
(switch-to-buffer  (get-buffer-create "*scratch-python*"))
(python-mode)

; start in the scratch buffer
(switch-to-buffer  (get-buffer-create "*scratch*"))

(provide 'startup-customisations)