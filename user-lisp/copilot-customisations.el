(add-to-list 'load-path "/home/wilfred/src/copilot.el")
(require 'copilot)

(setq copilot-indent-offset-warning-disable t)

(global-set-key (kbd "C-<return>") #'copilot-accept-completion)
(global-set-key (kbd "S-<return>") #'copilot-accept-completion-by-line)

;; Don't trigger copilot on idle too aggressively.
(setq copilot-idle-delay 1)

;; Add a keybinding to trigger it explicitly.
(global-set-key (kbd "<backtab>") #'copilot-complete)

(add-hook 'prog-mode-hook 'copilot-mode)
;; (remove-hook 'prog-mode-hook 'copilot-mode)
