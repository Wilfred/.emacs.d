(defun insert-hash ()
  (interactive)
  (insert "#"))

;; set Meta-3 to insert a # character
(global-set-key "\263" 'insert-hash)

;; same PATH as bash from Terminal
(setenv
 "PATH"
 "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/bin/g4bin:/usr/local/git/bin:/usr/local/sbin:/usr/X11/bin:/Users/wilfredhughes/homebrew/bin:/Users/wilfredhughes/homebrew/Cellar/fish/1.23.1/bin:/usr/X11R6/bin")

;; and add these to exec-path too, so Emacs can find git and so on
(setq exec-path (append exec-path
                        (split-string (getenv "PATH") ":")))

(provide 'os-x-fixes)
