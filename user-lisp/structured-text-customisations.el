; csv mode stuff, since it's used extensively in GBBO
(autoload 'csv-mode "csv-mode")

; yaml mode stuff, since google app engine uses it
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

; markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

; XML position utility:
(autoload 'nxml-ensure-scan-up-to-date "nxml-rap")
(autoload 'nxml-backward-up-element "nxml-mode")
(autoload 'xmltok-start-tag-qname "xmltok")

; taken from http://osdir.com/ml/emacs.nxml.general/2006-05/msg00013.html
(defun show-xml-path (&optional print-message)
  "Return all the elements in the ancestor axis of the current
element.  If called interactively, show it in the echo area."
  (interactive "p")
  (nxml-ensure-scan-up-to-date)
  (let ((path ""))
    (save-excursion
      (condition-case ()
          (while t
            (nxml-backward-up-element)
            (setq path (concat
                        " / " (xmltok-start-tag-qname) path)))
        (error nil)))
    (when print-message
      (message "%s" path))
    path))


(provide 'structured-text-customisations)
