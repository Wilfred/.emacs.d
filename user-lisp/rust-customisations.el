(require 'flycheck)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'rust-mode-hook #'flycheck-mode)

(require 'racer)
(setq racer-rust-src-path "/home/wilfred/src/rust/src")
(setq racer-cmd "/home/wilfred/src/racer/target/release/racer")

(require 'rust-mode)
(add-hook 'rust-mode-hook #'racer-activate)
(define-key rust-mode-map (kbd "M-.") #'racer-find-definition)

(provide 'rust-customisations)
