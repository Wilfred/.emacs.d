(require 'copilot)
(global-set-key (kbd "C-<return>") #'copilot-accept-completion)
(global-set-key (kbd "S-<return>") #'copilot-accept-completion-by-line)

;; Don't trigger copilot on idle.
(setq copilot-idle-delay 999)

;; Instead, use it explicitly.
(global-set-key (kbd "<backtab>") #'copilot-complete)

;; (add-hook 'prog-mode-hook 'copilot-mode)
