(setq projectile-svn-command
      "find . -type f | grep -v '/.svn/' | tr '\\n' '\\0'")

;; For large files, projectile-find-file can be a little sluggish. Cache the result.
(setq projectile-enable-caching t)

(diminish-undo #'projectile-mode)
(setq projectile-mode-line
      '(:eval
        (if
            (file-remote-p default-directory)
            " Prj"
          (format " Prj:%s"
                  (projectile-project-name)))))

;; I like this command so much that it's nice to have it available
;; globally.
(global-set-key (kbd "C-c p p") #'projectile-switch-project)

(provide 'projectile-customisations)
