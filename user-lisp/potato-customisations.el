
(defun set-gxbo-market (str)
  "Write the GxBO market into app.yaml."
  (interactive "sMarket: ")
  (goto-char 19)
  (delete-region (point) (line-end-position))
  (insert str)
  (save-buffer))

(define-key yaml-mode-map [(f12)] 'set-gxbo-market)

(defun deploy-gae-backend (email password market)
  "Set app.yaml market as appropriate, then deploy the business-progress backend."
  (interactive "sGAE email: \nsGAE password: \nsMarket: ")
  (let ((project-root (find-gae-project-root (buffer-file-name)))
        (app-yaml-path (concat project-root "/app.yaml")))
    
    (find-file app-yaml-path)
    (set-gxbo-market market))
  
  (let ((project-root (find-gae-project-root (buffer-file-name)))
        (process (start-process "gae_deploy" "*GAE-deploy*"
                                "/opt/google_appengine/appcfg.py"
                                "backends" project-root "update" "business-progress")))
    (process-send-string process (concat email "\n"))
    (process-send-string process (concat password "\n"))))


(provide 'potato-customisations)