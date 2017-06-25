(defun insert-hash ()
  (interactive)
  (insert "#"))

;; set Meta-3 to insert a # character
(global-set-key "\263" 'insert-hash)

(provide 'os-x-fixes)
