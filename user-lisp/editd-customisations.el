(require 's)
(require 'vc-git)
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

(defun git-flow-release (tag-message)
  "Use gitflow to mark a new release."
  (interactive "sTag message: ")
  ;; todo: check we're in an Editd buffer
  (let* ((project-root (vc-git-root default-directory))
         (output-buffer (get-buffer-create (format "*New release %s*" project-root)))
         (next-version (get-next-version project-root))
         (process-environment process-environment)) ;; temporary environment change
    (switch-to-buffer output-buffer)
    (setq default-directory project-root)
    (erase-buffer)

    (setenv "GIT_MERGE_AUTOEDIT" "no")

    (execute-commands output-buffer
                      (format "git flow release start %s" next-version)
                      (format "git flow release finish %s -m \"%s\"" next-version tag-message)
                      "git push"
                      "git push --tags"
                      "git checkout develop")))

(provide 'editd-customisations)
