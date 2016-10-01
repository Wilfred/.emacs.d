(defun insert-hash ()
  (interactive)
  (insert "#"))

;; set Meta-3 to insert a # character
(global-set-key "\263" 'insert-hash)

;; In OS X, starting Emacs in GUI mode doesn't inherit the shell's
;; environment. We set up Emacs' exec-path based on PATH in a shell,
;; so any command we can call from a shell, we can call inside Emacs.
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(provide 'os-x-fixes)
