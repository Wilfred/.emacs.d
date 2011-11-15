(defvar gae-email-address "" "Email addressed use to authenticate with App Engine.")
(defvar gae-appcfg-path  "~/bin/google_appengine/appcfg.py")

; TODO: Django equivalent of the below
; TODO: handle the password being incorrect

(defun gae-run-appcfg (email password arguments)
  "Run appcfg.py in DIR with login credentials EMAIL and
PASSWORD, passing ARGUMENTS to appcfg.py."
  (let ((process (apply 'start-process "gae_deploy" "*GAE-deploy*" gae-appcfg-path
                        (append arguments (list "-e" email)))))
    ; this shouldn't be necessary. But the SDK just doesn't accept early
    ; input, and it's slow to give the password prompt:
    (sleep-for 1)

    ; TODO: check where all these \n characters are necessary
    (process-send-string process (concat "\n\n\n\n" password "\n\n"))))

(defun gae-deploy-application (email password)
  "Deploy the current GAE application."
  (interactive (list (read-from-minibuffer "GAE email: " gae-email-address)
                     (read-from-minibuffer "Password: ")))
  (setq last-gae-email-used email)

  (let ((project-root (expand-file-name (vc-git-root (buffer-file-name)))))
    (gae-run-appcfg email password (list "update" project-root))
    (switch-to-buffer "*GAE-deploy*")))

(require 'yaml-mode)
(define-key yaml-mode-map (kbd "<f12>") 'gae-deploy-application)

(defun gae-deploy-backend (email password)
  "Deploy all the backends of the current application."
  (interactive (list (read-from-minibuffer "GAE email: " gae-email-address)
                     (read-from-minibuffer "Password: ")))
  (setq last-gae-email-used email)
  (let ((project-root (expand-file-name (vc-git-root (buffer-file-name)))))
    (gae-run-appcfg email password (list "backends" project-root "update"))
    (switch-to-buffer "*GAE-deploy*")))

(provide 'gae-utils)