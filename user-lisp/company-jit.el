(require 'company)
(require 'dash)

(defvar company-jit-prefixes
  '(
    (python-mode ("import " "from ") company-anaconda)
    (rust-mode ("::" ".") racer-company-complete)
    (css-mode (": ") company-css))
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
  (catch 'break
    (--each company-jit-prefixes
      (-let [(mode prefixes backend) it]
        ;; If we're in the mode listed in `company-jit-prefixes'
        (when (and (eq mode major-mode)
                   ;; and we're not in a string or comment
                   (company-jit-should-complete-p)
                   ;; and we're looking at one of the prefixes mentioned
                   (--any-p (company-jit-match-p it) prefixes))
          (company-begin-backend backend)
          (throw 'break nil))))))

(add-hook 'post-self-insert-hook #'company-jit-post-self-insert)

(provide 'company-jit)
