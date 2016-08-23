(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))

;; these are the default settings, but it's nice to be explicit for ease of customisation
(add-hook 'js-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
	     (setq js2-basic-offset 4)))

; fix js2-mode's function parameter colour, which is too dark for a dark theme
(custom-set-faces
 '(js2-function-param-face ((((class color)) (:foreground "Green")))))

;; js2-mode offers a variety of warnings, but eslint is better at
;; this, so we switch of those in js2-mode.
(setq js2-strict-inconsistent-return-warning nil)
(setq js2-strict-missing-semi-warning nil)
(setq js2-strict-trailing-comma-warning nil)

(add-hook 'js2-jsx-mode-hook #'flycheck-mode)
(add-hook 'js2-mode-hook #'flycheck-mode)

(require 'company)
(add-to-list 'company-backends 'company-tern)

(defun wh/company-in-js2-mode ()
  (set (make-local-variable 'company-backends)
       (list #'company-tern #'company-dabbrev-code #'company-keywords )))

(add-hook 'js2-mode-hook #'wh/company-in-js2-mode)

(js2r-add-keybindings-with-prefix "C-c C-r")

;; TODO: there are better pre-existing tools that do this, with
;; convenient cleanup commands.
(defun wh/insert-console-log ()
  "Insert a log statement at point for VARIABLE.
Variable is a taken from the current selection, or suggested from
variables near point."
  (interactive)
  (let ((variable (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (completing-read "Variable: " (wh/nearby-symbols)))))
    (crux-smart-open-line-above)
    (insert (format "console.log(['%s', %s]);" variable variable))))

(require 'dash)
(require 's)

(defun wh/nearby-symbols ()
  (->>
   (apply #'buffer-substring-no-properties (wh/js-surrounding-region))
   (s-split "[^[:word:]0-9]+")
   (--filter (> (length it) 1))
   (-distinct)))

(defun wh/js-surrounding-region ()
  "Return the region surrounding point, as a string.
Purely a heuristic, intended for finding symbols that might be useful."
  (interactive)
  (save-mark-and-excursion
   (let ((current-prefix-arg '(5)))
     (call-interactively #'er/expand-region)
     (list (region-beginning) (region-end)))))

(provide 'javascript-customisations)
