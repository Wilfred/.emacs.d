(require 'ansi-color)

;; http://stackoverflow.com/q/3072648/509706
(defun wh/colorize-compilation-buffer ()
  "If a build tool outputs ANSI terminal colours, highlight them correctly
rather than showing ^[[31m garbage."
  (when (eq major-mode 'compilation-mode)
    (let ((read-only-mode t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(add-hook 'compilation-filter-hook 'wh/colorize-compilation-buffer)

(provide 'compilation-customisations)
