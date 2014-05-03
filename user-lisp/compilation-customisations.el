(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (let ((read-only-mode t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'compilation-customisations)
