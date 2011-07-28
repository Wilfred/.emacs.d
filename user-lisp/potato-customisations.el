
(defun set-gxbo-market (str)
  "Write the GxBO market into app.yaml."
  (interactive "sMarket: ")
  (goto-char 19)
  (delete-region (point) (line-end-position))
  (insert str)
  (save-buffer))

(define-key yaml-mode-map [(f12)] 'set-gxbo-market)

(defun is-gae-project-root (path)
  "Does this project contain a app.yaml file?"
  (setq app-yaml-path (concat path "/app.yaml"))
  (file-exists-p app-yaml-path))

(defun find-gae-project-root (path)
  "If we're in a GAE project, find the directory that contains the app.yaml file."
  (setq abs-path (expand-file-name path))
  (if (string= abs-path "/") 'nil
    (if (is-gae-project-root abs-path)
        abs-path
      (find-gae-project-root (concat path "/..")))))

(require 'thingatpt) ; provides symbol-at-point

(defun grep-gae-project (search-term)
  "Search a GAE project using git grep. The default search term is the symbol at point."
  (interactive (list (read-from-minibuffer "Search for: "
                                           (symbol-name (symbol-at-point)))))
  (setq project-root (find-gae-project-root "."))
  (if project-root
      (vc-git-grep search-term "*" project-root)
    (message "Couldn't find project root.")))

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

(global-set-key [(f5)] 'grep-gae-project)

(provide 'potato-customisations)