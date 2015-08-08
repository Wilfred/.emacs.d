(require 'company)
(require 'ht)
(require 'dash)

(defvar company-jit-prefixes
  (ht
   (#'python-mode '("import " "from "))
   (#'rust-mode '("::" "."))
   (#'css-mode '(": ")))
  "Prefix strings that trigger company completion.")

(defun company-jit-match-p (prefix)
  "Does the text before point match PREFIX?"
  (save-excursion
    (save-match-data
      (backward-char (length prefix))
      (looking-at (regexp-quote prefix)))))

(defun company-jit-should-complete-p ()
  "Return nil if point is in a comment or string."
  (-let [(_ _ _ in-string in-comment &rest _) (syntax-ppss)]
    (and (not in-string) (not in-comment))))

(defun company-jit-post-self-insert ()
  "Open company in situations where the user probably wants completions."
  (when (let ((prefixes (ht-get company-jit-prefixes major-mode '())))
          (and
           (--any-p (company-jit-match-p it) prefixes)
           (company-jit-should-complete-p)))
    (company-complete)))

(add-hook 'post-self-insert-hook #'company-jit-post-self-insert)

(provide 'company-jit)
