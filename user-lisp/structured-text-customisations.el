;; csv mode stuff, since it's used extensively in GBBO
(autoload 'csv-mode "csv-mode")

;; latex
(add-hook 'latex-mode-hook #'smartparens-mode)

;; JSON
(add-hook 'json-mode-hook
          (lambda ()
            ;; Prettier uses 2-space indents, so match that.
            (setq-local js-indent-level 2)))
(add-hook 'json-mode-hook #'flycheck-mode)

;; yaml mode stuff, since google app engine uses it
(add-hook 'yaml-mode-hook #'highlight-symbol-mode)

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
;; Don't try to be smart with ```, I find it rarely useful.
(setq markdown-gfm-use-electric-backquote nil)

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

(add-to-list 'auto-mode-alist
             (cons (rx ".jelly" eos) #'nxml-mode))

;; taken from http://osdir.com/ml/emacs.nxml.general/2006-05/msg00013.html
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

;; https://www.reddit.com/r/emacs/comments/6w9ecn/anyone_using_hyperbole/dmdp0y9/
(add-hook 'find-file-hook
          (lambda ()
            (when (and (featurep 'hyperbole) (eq major-mode 'rust-mode))
              (hproperty:but-clear)))
          t)

(use-package css-mode
  :config
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook #'prettier-js-mode))

;; Port of http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=900ede244c886c56579dcbfabd04cf4f144275a1
(defun css--font-lock-keywords (&optional sassy)
  `((,(concat "!\\s-*"
              (regexp-opt (append (if sassy scss-bang-ids)
                                  css-bang-ids)))
     (0 font-lock-builtin-face))
    ;; Atrules keywords.  IDs not in css-at-ids are valid (ignored).
    ;; In fact the regexp should probably be
    ;; (,(concat "\\(@" css-ident-re "\\)\\([ \t\n][^;{]*\\)[;{]")
    ;;  (1 font-lock-builtin-face))
    ;; Since "An at-rule consists of everything up to and including the next
    ;; semicolon (;) or the next block, whichever comes first."
    (,(concat "@" css-ident-re) (0 font-lock-builtin-face))
    ;; Variables.
    (,(concat (rx symbol-start) "--" css-ident-re) (0 font-lock-variable-name-face))
    ;; Selectors.
    ;; FIXME: attribute selectors don't work well because they may contain
    ;; strings which have already been highlighted as f-l-string-face and
    ;; thus prevent this highlighting from being applied (actually now that
    ;; I use `keep' this should work better).  But really the part of the
    ;; selector between [...] should simply not be highlighted.
    (,(concat
       "^[ \t]*\\("
       (if (not sassy)
           ;; We don't allow / as first char, so as not to
           ;; take a comment as the beginning of a selector.
           "[^@/:{}() \t\n][^:{}()]+"
         ;; Same as for non-sassy except we do want to allow { and }
         ;; chars in selectors in the case of #{$foo}
         ;; variable interpolation!
         (concat "\\(?:" scss--hash-re
                 "\\|[^@/:{}() \t\n#]\\)"
                 "[^:{}()#]*\\(?:" scss--hash-re "[^:{}()#]*\\)*"))
       ;; Even though pseudo-elements should be prefixed by ::, a
       ;; single colon is accepted for backward compatibility.
       "\\(?:\\(:" (regexp-opt (append css-pseudo-class-ids
                                       css-pseudo-element-ids) t)
       "\\|\\::" (regexp-opt css-pseudo-element-ids t) "\\)"
       "\\(?:([^)]+)\\)?"
       (if (not sassy)
           "[^:{}()\n]*"
         (concat "[^:{}()\n#]*\\(?:" scss--hash-re "[^:{}()\n#]*\\)*"))
       "\\)*"
       "\\)\\(?:\n[ \t]*\\)*{")
     (1 'css-selector keep))
    ;; In the above rule, we allow the open-brace to be on some subsequent
    ;; line.  This will only work if we properly mark the intervening text
    ;; as being part of a multiline element (and even then, this only
    ;; ensures proper refontification, but not proper discovery).
    ("^[ \t]*{" (0 (save-excursion
                     (goto-char (match-beginning 0))
                     (skip-chars-backward " \n\t")
                     (put-text-property (point) (match-end 0)
                                        'font-lock-multiline t)
                     ;; No face.
                     nil)))
    ;; Properties.  Again, we don't limit ourselves to css-property-ids.
    (,(concat "\\(?:[{;]\\|^\\)[ \t]*\\("
              "\\(?:\\(" css-proprietary-nmstart-re "\\)\\|"
              css-nmstart-re "\\)" css-nmchar-re "*"
              "\\)\\s-*:")
     (1 (if (match-end 2) 'css-proprietary-property 'css-property)))
    ;; Make sure the parens in a url(...) expression receive the
    ;; default face. This is done because the parens may sometimes
    ;; receive generic string delimiter syntax (see
    ;; `css-syntax-propertize-function').
    (,css--uri-re
     (1 'default t) (2 'default t))))

(setq css-font-lock-keywords (css--font-lock-keywords))

(provide 'structured-text-customisations)
