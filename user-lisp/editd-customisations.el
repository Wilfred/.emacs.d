(require 's)
(require 'execute-commands)

(defun get-latest-version (repo-path)
  "Find the last tagged version. Assumes GNU sort (BSD sort lacks --version-sort)."
  (let ((default-directory repo-path))
    (s-trim
     (shell-command-to-string "git tag --list | grep -v vv | grep -v version | sort --version-sort | tail -n 1"))))

(defun get-next-version (repo-path)
  "Increment the minor part of the current version."
  (let* ((current-version (get-latest-version repo-path))
         (minor-version-part (car (last (s-split "\\." current-version))))
         (major-version-parts (s-join "." (butlast (s-split "\\." current-version))))
         (minor-version-number (string-to-number minor-version-part)))
    (format "%s.%s" major-version-parts (1+ minor-version-number))))

(defun git-flow-release-start ()
  "Use gitflow to mark a new release."
  (interactive)
  ;; todo: check we're in an Editd buffer
  (let* ((project-root (vc-git-root default-directory))
         (output-buffer (get-buffer-create (format "*New release %s*" project-root)))
         (next-version (get-next-version project-root))
         (next-command (format "git flow release finish %s" next-version)))
    (switch-to-buffer output-buffer)
    (setq default-directory project-root)
    (erase-buffer)
    (execute-commands output-buffer (format "git flow release start %s" next-version))

    ;; copy the finish command to the kill ring and clipboard to save typing
    (message "Please run the command '%s' in your terminal.
It has been copied to your clipboard for convenience." next-command)
    (kill-new next-command)))

(defun git-flow-release-push ()
  "After finishing a gitflow release, push it and move back to develop."
  (interactive)
  (let* ((project-root (vc-git-root default-directory))
         (output-buffer (get-buffer-create (format "*New release %s*" project-root)))
         (next-version (get-next-version project-root)))
    (unless project-root
       (error "Not in a git project"))
    (switch-to-buffer output-buffer)
    (setq default-directory project-root)
    (erase-buffer)
    (execute-commands output-buffer
                      "git push"
                      "git push --tags"
                      "git checkout develop")))

(provide 'editd-customisations)
