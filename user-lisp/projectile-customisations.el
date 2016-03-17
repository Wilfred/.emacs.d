(setq projectile-svn-command
      "find . -type f | grep -v '/.svn/' | tr '\\n' '\\0'")

(provide 'projectile-customisations)
