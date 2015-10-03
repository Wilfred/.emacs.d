(require 'flycheck)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'rust-mode-hook #'flycheck-mode)

(require 'racer)
(setq racer-rust-src-path "/home/wilfred/src/rust/src")
(setq racer-cmd "/home/wilfred/src/racer/target/release/racer")

(add-to-list 'company-backends #'racer-company-complete)

(require 'rust-mode)
(add-hook 'rust-mode-hook #'racer-turn-on-eldoc)
(define-key rust-mode-map (kbd "M-.") #'racer-find-definition)

(defun wh/rust-toggle-visibility ()
  "Toggle the public visibility of the function at point."
  (interactive)
  (save-excursion
    ;; If we're already at the beginning of the function definition,
    ;; `beginning-of-defun' moves to the previous function, so move elsewhere.
    (end-of-line)

    (beginning-of-defun)
    (if (looking-at "pub ")
        (delete-char 4)
      (insert "pub "))))

(define-key rust-mode-map (kbd "C-c v") #'wh/rust-toggle-visibility)

(require 'projectile)
(require 'which-func)

(defun wh/rust-run-test ()
  "Run the test at point."
  (interactive)
  (let ((defualt-directory (projectile-project-root)))
    (compilation-start
     (format "cargo test %s" (which-function)))))

(define-key rust-mode-map (kbd "C-c t") #'wh/rust-run-test)

;; Use 'cargo check' as a build command, as it's much faster.
(projectile-register-project-type 'rust-cargo '("Cargo.toml") "cargo check" "cargo test")


(provide 'rust-customisations)
