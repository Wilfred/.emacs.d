;; FIXME: make upstream shell-command-to-string not die when
;; default-directory has value "/foo/bar/baz"
(defun shell-command-to-string-in-dir (command directory)
  "The function shell-command-to-string runs COMMAND in the
current buffer's default-directory. This function allows us to
specify the directory whilst preserving default-directory."
  (let ((normalized-path (file-name-as-directory directory))
        (original-default-directory default-directory)
        (command-output))
    (when (not (file-exists-p normalized-path))
      (error "Directory %s doesn't exist" normalized-path))
    (setq default-directory normalized-path)
    (setq command-output (shell-command-to-string command))
    (setq default-directory original-default-directory)
    command-output))

(provide 'shell-utils)
