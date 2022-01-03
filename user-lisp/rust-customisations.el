(defun wh/rust-wrap-dbg (start end)
  "Wrap the current selection in dbg!(..)."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert ")")
    (goto-char start)
    (insert "dbg!(")))

(defun wh/rust-unwrap-dbg ()
  "Remove dbg!(foo) to just foo on the line at point."
  (interactive)

  (save-excursion
    (beginning-of-line)
    (search-forward "dbg!(")
    (sp-splice-sexp)
    (backward-delete-char 4)))

(defun wh/rust-dbg-dwim ()
  "Insert dbg!() if region is active, otherwise remove."
  (interactive)
  (if (region-active-p)
      (wh/rust-wrap-dbg (region-beginning) (region-end))
    (wh/rust-unwrap-dbg)))

(use-package rust-mode
  :config
  (require 'f)
  (add-to-list 'exec-path (f-expand "~/.cargo/bin"))

  (defun wh/rust-company-settings ()
    "Set up company settings for rust buffers."
    (setq-local company-idle-delay 0.3))

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

  (define-key rust-mode-map (kbd "C-c d") #'wh/rust-dbg-dwim))

(require 'lsp-customisations)

(provide 'rust-customisations)
