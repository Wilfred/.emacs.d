;;; -*- lexical-binding: nil; -*-
(require 'em-cmpl)

;; Auto-complete in eshell should stop at the first ambiguity.
(setq eshell-cmpl-cycle-completions nil)

(define-advice eshell (:around (old-function &rest args) eshell-append-cwd)
  "New eshell buffers should be named according to their working directory."
  (interactive "P")
  (let ((arg (car args)))
    (if (and arg (not (numberp arg)))
        ;; Non-numeric prefix arg given, change the eshell buffer name.
        (let ((eshell-buffer-name (format "*eshell*<%s>" default-directory)))
          (apply old-function args))
      ;; Otherwise, continue as normal
      (apply old-function args))))

(provide 'eshell-customisations)
