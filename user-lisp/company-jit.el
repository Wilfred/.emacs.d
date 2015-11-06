(require 'company)
(require 'dash)

(defvar company-jit-prefixes
  `(
    (python-mode
     (,(rx symbol-start "import ") ,(rx symbol-start "from ")
      ,(rx symbol-end "."))
     company-anaconda)
    (rust-mode
     (,(rx "::") ,(rx "."))
     racer-company-complete)
    (css-mode
     (,(rx ": "))
     company-css))
  "Prefix strings that trigger company completion.")

(defun company-jit-match-p (prefix)
  "Does the text before point match PREFIX?"
  (looking-back prefix))

(defun company-jit-post-self-insert ()
  "Open company in situations where the user probably wants completions."
  (catch 'break
    (--each company-jit-prefixes
      (-let [(mode prefixes backend) it]
        ;; If we're in the mode listed in `company-jit-prefixes'
        (when (and (eq mode major-mode)
                   ;; and we're in code
                   (not (company-in-string-or-comment))
                   ;; and we're looking at one of the prefixes mentioned
                   (--any-p (company-jit-match-p it) prefixes))
          (company-begin-backend backend)
          (throw 'break nil))))))

(add-hook 'post-self-insert-hook #'company-jit-post-self-insert)

(provide 'company-jit)
