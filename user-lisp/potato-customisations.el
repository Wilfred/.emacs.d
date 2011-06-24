
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

(defun grep-gae-project (search-term)
  "Search a GAE project using git grep."
  (interactive "sSearch for: ")
  (setq project-root (find-gae-project-root "."))
  (if project-root
      (vc-git-grep search-term "*" project-root)))

(global-set-key [(f5)] 'grep-gae-project)

(provide 'potato-customisations)