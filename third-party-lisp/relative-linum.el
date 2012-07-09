;; relative-line-numbers.el -- show relative line number and allow offset jumping

;; taken from http://stackoverflow.com/a/7882046

;;; relative line numbers, vim style

;; enable:
;; (global-linum-mode t)
;; disable:
;; (global-linum-mode 0)
;; enable locally:
;; (linum-mode 1)


(defvar relative-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'relative-linum-get-format-string)

(defun relative-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d")))
    (setq relative-linum-format-string format)))

(defvar relative-linum-current-line-number 0)

(setq linum-format 'relative-linum-relative-line-numbers)

(defun relative-linum-relative-line-numbers (line-number)
  (let ((offset (- line-number relative-linum-current-line-number)))
    (propertize (format relative-linum-format-string offset) 'face 'linum)))

(defadvice linum-update (around relative-linum-update)
  (let ((relative-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(defun relative-linum-jump (offset)
  "Move OFFSET lines up or down from the current line."
  (interactive "nLine offset: ")
  (forward-line offset))

(global-set-key (kbd "C-S-n") 'relative-linum-jump)

(provide 'relative-linum)
