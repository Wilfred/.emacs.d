;;; rust-mode-treesitter.el --- use native rust-ts-mode -*-lexical-binding: t-*-
;;; Commentary:

;; Derive from rust-ts-mode instead of prog-mode

;;; Code:

(when (version<= "29.1" emacs-version)
  ;; We have the when macro because of
  ;; https://github.com/rust-lang/rust-mode/issues/520
  (require 'treesit)
  (require 'rust-ts-mode)
  (require 'rust-common)

  (define-derived-mode rust-mode rust-ts-mode "Rust"
    "Major mode for Rust code.

\\{rust-mode-map}"
    :group 'rust-mode

    (add-hook 'before-save-hook rust-before-save-hook nil t)
    (add-hook 'after-save-hook rust-after-save-hook nil t)))

(provide 'rust-mode-treesitter)
;;; rust-mode-treesitter.el ends here
