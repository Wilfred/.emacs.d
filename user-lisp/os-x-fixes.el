;;; -*- lexical-binding: nil; -*-
(defun wh/insert-hash ()
  (interactive)
  (insert "#"))

;; set Meta-3 to insert a # character
(global-set-key "\263" #'wh/insert-hash)


(defun wh/toggle-meta-key-for-macos ()
  "Toggle whether the option or command key is treated as meta."
  (interactive)
  (if (eq mac-command-modifier 'meta)
      (setq mac-command-modifier 'super
            mac-option-modifier 'meta)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super))
  (message "Meta key is now: %s"
           (if (eq mac-command-modifier 'meta) "command" "option")))

(provide 'os-x-fixes)
