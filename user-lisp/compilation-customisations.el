(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (let ((read-only-mode t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;; FIXME: This works wonderfully for coloured output during make jobs, but
;; breaks ag.el
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'compilation-customisations)
