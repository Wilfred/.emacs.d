(require 'company)
(require 'dash)

(defvar company-jit-prefixes
  `(
    (python-mode
     (,(rx symbol-start "import ") ,(rx symbol-start "from "))
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

;;;###autoload
(define-minor-mode company-jit-mode
  "Automatically start company when it looks like it'd be useful."
  :lighter " CompJit"
  (if company-jit-mode
      (add-hook 'post-self-insert-hook #'company-jit-post-self-insert)) nil t)

(defun company-jit-post-self-insert ()
  "Open company in situations where the user probably wants completions."
  (when company-jit-mode
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
            (throw 'break nil)))))))

(provide 'company-jit)
