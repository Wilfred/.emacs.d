(require 'em-cmpl)

;; Auto-complete in eshell should stop at the first ambiguity.
(setq eshell-cmpl-cycle-completions nil)

(defadvice eshell (around eshell-append-cwd (&optional arg) activate)
  "New eshell buffers should be named according to their working directory."
  (interactive "P")
  (if (and arg (not (numberp arg)))
      ;; Non-numeric prefix arg given, change the eshell buffer name.
      (let* ((current-dir (f-parent (buffer-file-name)))
             (eshell-buffer-name (format "*eshell*<%s>" current-dir)))
        ad-do-it)
    ;; Otherwise, continue as normal
    ad-do-it))

(provide 'eshell-customisations)
