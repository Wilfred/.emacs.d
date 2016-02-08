; csv mode stuff, since it's used extensively in GBBO
(autoload 'csv-mode "csv-mode")

; yaml mode stuff, since google app engine uses it
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'yaml-mode-hook #'highlight-symbol-mode)

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(require 's)
(require 'dash)

;; TODO: submit as a useful command to markdown-toc.
(defconst commonmark-anchor-rx (rx "<a name=\"" (group (1+ (char alnum "-"))) "\"></a>"))
(defconst commonmark-heading-rx (rx line-start (1+ "#") (group (+? anything)) line-end))

(defun wh/heading-anchor-p ()
  "Return non-nil if there's an anchor before the heading at point."
  (let ((start-pos (point)))
    (save-excursion
      ;; Move to the previous line
      (forward-line -1)
      ;; See if there's an anchor tag, not searching beyond our start point.
      (search-forward-regexp commonmark-anchor-rx start-pos t))))

(defun wh/insert-anchor (name)
  "Insert a named anchor on the heading at point."
  (save-excursion
    (when (wh/heading-anchor-p)
      (forward-line -1)
      (kill-whole-line))
    (crux-smart-open-line-above)
    (insert (format "<a name=\"%s\"></a>" name))))

(defun wh/slugify (heading-text)
  "Convert 'Foo Bar/Baz?' to 'foo-barbaz'."
  (->> heading-text
       s-downcase
       s-trim
       (s-replace " " "-")
       (replace-regexp-in-string "[^a-z0-9-]" "")))

;;;###autoload
(defun wh/add-commonmark-headings ()
  "Ensure there's an <a name=''> on each heading in a commonmark file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp commonmark-heading-rx nil t)
      (let* ((heading-properties (text-properties-at (point)))
             (heading-text (match-string-no-properties 1)))
        (unless (-contains? heading-properties 'markdown-pre-face)
          (wh/insert-anchor (wh/slugify heading-text)))))))

;; XML position utility:
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
