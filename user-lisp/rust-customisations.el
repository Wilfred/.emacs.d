(use-package rust-mode
  :config
  (require 'f)
  (add-to-list 'exec-path (f-expand "~/.cargo/bin"))

  (defun wh/rust-company-settings ()
    "Set up company settings for rust buffers."
    (setq-local company-idle-delay 0.1))

  (add-hook 'rust-mode-hook #'wh/rust-company-settings)

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

  (defun wh/rust-vec-as-slice ()
    "Convert the vector expression at point to a slice.
foo -> &foo[..]"
    (interactive)
    (insert "&")
    (forward-symbol 1)
    (insert "[..]"))

  (define-key rust-mode-map (kbd "C-c s") #'wh/rust-vec-as-slice)

  (defun wh/rust-wrap-dbg (start end)
    "Wrap the current selection in dbg!(..)."
    (interactive "r")
    (save-excursion
      (goto-char end)
      (insert ")")
      (goto-char start)
      (insert "dbg!(")))

  (define-key rust-mode-map (kbd "C-c d") #'wh/rust-wrap-dbg)

  (require 'projectile)

  (defun wh/rust-run-test ()
    "Run the test at point."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (compilation-start
       (format "cargo test %s" (which-function))))

    (define-key rust-mode-map (kbd "C-c t") #'wh/rust-run-test)

    ;; Use 'cargo check' as a build command, as it's much faster.
    (projectile-register-project-type 'rust-cargo '("Cargo.toml") "cargo check" "cargo test")

    (define-key rust-mode-map (kbd "C-c C-c") #'multi-compile-run)))

;; For some reason icons aren't currently rendering, so don't show them.
(setq lsp-modeline-code-actions-segments '(count))

(provide 'rust-customisations)
