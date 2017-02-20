(setq projectile-svn-command
      "find . -type f | grep -v '/.svn/' | tr '\\n' '\\0'")

(diminish-undo #'projectile-mode)
(setq projectile-mode-line
      '(:eval
        (if
            (file-remote-p default-directory)
            " Prj"
          (format " Prj:%s"
                  (projectile-project-name)))))

(provide 'projectile-customisations)
