;; relative-line-numbers.el -- show relative line number and allow offset jumping

;; taken from http://stackoverflow.com/a/7882046

;;; relative line numbers, vim style

;; enable:
;; (global-linum-mode t)
;; disable:
;; (global-linum-mode 0)
;; enable locally:
;; (linum-mode 1)


(defvar my-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-relative-line-numbers)

(defun my-linum-relative-line-numbers (line-number)
  (let ((offset (- line-number my-linum-current-line-number)))
    (propertize (format my-linum-format-string offset) 'face 'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(defun jump-n-lines (offset)
  (interactive "nLine offset: ")
  (forward-line offset))

(global-set-key (kbd "C-S-n") 'jump-n-lines)

(provide 'relative-linum)
