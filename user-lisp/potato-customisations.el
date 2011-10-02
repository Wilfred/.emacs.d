(defvar last-email-address "" "Email address previously used")

(defun deploy-gae-application (email password)
  "Deploy the current GAE application."
  (interactive "sGAE email: \nsGAE password: ")
  
  (let ((project-root (vc-git-root (buffer-file-name)))
        (process (start-process "gae_deploy" "*GAE-deploy*"
                                "~/bin/google_appengine/appcfg.py"
                                "update" project-root
                                "-e" email)))
    (process-send-string process (concat conf "\n"))
    (switch-to-buffer "*GAE-deploy*")))

(require 'yaml-mode)
(define-key yaml-mode-map [(f12)] 'deploy-gae-application)

(defun deploy-gae-backend (email password)
  "Deploy all the backends of the current application."
  (interactive "sGAE email: \nsGAE password: ")
  
  (let ((project-root (vc-git-root (buffer-file-name)))
        (process (start-process "gae_deploy" "*GAE-deploy*"
                                "/opt/google_appengine/appcfg.py"
                                "backends" project-root "update")))
    (process-send-string process (concat email "\n"))
    (process-send-string process (concat password "\n"))))


(provide 'potato-customisations)