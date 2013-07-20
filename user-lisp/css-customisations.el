(require 'css-mode)

;; indent CSS with tabs
(add-hook 'css-mode-hook
          (function
           (lambda ()
             (progn
               (setq css-indent-offset 4)
               (setq indent-tabs-mode nil)))))

; highlight colours as themselves
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)

(provide 'css-customisations)
