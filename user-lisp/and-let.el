(eval-when-compile (require 'cl)); declare, first, rest

;;;; and-let*, based on Scheme's SRFI 2
;; Contrived usage example:
;; (and-let* ((buffer-path (buffer-file-name))
;;            (file-name (file-name-directory)))
;;           (message "You are visiting a file called %s in this buffer."
;;                    file-name))
(defmacro and-let* (varlist &rest body)
  "Equivalent to let*, but terminates early and returns nil if
any variable is bound to nil. BODY is only evaluated if all
variables were non-nil."
  (declare (debug (form &rest form)))
  (if (eq varlist '()) `(progn ,@body)
    (let* ((first-binding (first varlist))
           (var-name (first first-binding)))
      `(let (,first-binding)
         (if ,var-name
             (and-let* ,(rest varlist) ,@body))))))

(provide 'and-let)
