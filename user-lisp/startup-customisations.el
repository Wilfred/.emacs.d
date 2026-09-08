;;; -*- lexical-binding: nil; -*-
; no startup screen
(setq-default inhibit-startup-screen t)

;; Use *scratch* for plain text.
(setq initial-scratch-message "Ready.")
(setq initial-major-mode #'text-mode)

;; Create separate scratch buffers for languages I commonly use.
(with-current-buffer (get-buffer-create "*scratch-elisp*")
  (emacs-lisp-mode))
(with-current-buffer (get-buffer-create "*scratch-python*")
  (python-mode))
(with-current-buffer (get-buffer-create "*scratch-rust*")
  (rust-mode))

;; start in the scratch buffer
(switch-to-buffer (get-buffer-create "*scratch*"))

(provide 'startup-customisations)
