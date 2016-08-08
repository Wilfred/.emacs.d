
(require 'racer)
(setq racer-rust-src-path "/home/wilfred/src/rustc-1.5.0/src")

(add-to-list 'company-backends #'racer-company-complete)

(require 'company)

(defun wh/rust-company-settings ()
  "Set up company settings for rust buffers."
  (setq-local company-idle-delay 0.1))

(add-hook 'rust-mode-hook #'wh/rust-company-settings)

(require 'rust-mode)
(add-hook 'rust-mode-hook #'eldoc-mode)
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

(defun wh/rust-toggle-mutability ()
  "Toggle the mutability of the variable at point."
  (interactive)
  (save-excursion
    (racer-find-definition)
    (back-to-indentation)
    (forward-char 4)
    (if (looking-at "mut ")
        (delete-char 4)
      (insert "mut "))))

(define-key rust-mode-map (kbd "C-c m") #'wh/rust-toggle-mutability)

(defun wh/rust-vec-as-slice ()
  "Convert the vector expression at point to a slice.
foo -> &foo[..]"
  (interactive)
  (insert "&")
  (forward-symbol 1)
  (insert "[..]"))

(define-key rust-mode-map (kbd "C-c s") #'wh/rust-vec-as-slice)

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

(require 'multi-compile)
(setq multi-compile-alist
      '((rust-mode . (("rust-build" "cargo build" (projectile-project-root))
                      ("rust-check" "cargo check" (projectile-project-root))
                      ("rust-test" "cargo test" (projectile-project-root))))))

(define-key rust-mode-map (kbd "C-c C-c") #'multi-compile-run)

(provide 'rust-customisations)
