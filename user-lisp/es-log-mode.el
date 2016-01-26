;;; es-log-mode --- Basic highlighting for elasticsearch log files

(defvar es-log-mode-font-lock-keywords
  `(
    (,(rx line-start
          (group "[" (*? not-newline) "]")
          "[" (group (*? not-newline)) "]"
          "[" (group (*? not-newline)) "]"
          (0+ space)
          (group "[" (*? not-newline) "]")
          )
     (1 font-lock-comment-face)
     (2 font-lock-type-face)
     (3 font-lock-variable-name-face)
     (4 font-lock-comment-face))))

;;;###autoload
(define-derived-mode es-log-mode text-mode "ES log"
  "Major mode for view elasticsearch log files."
  (setq-local font-lock-defaults '(es-log-mode-font-lock-keywords)))

(provide 'es-log-mode)
