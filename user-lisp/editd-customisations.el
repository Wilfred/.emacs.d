(require 's)

(defun execute-in-buffer (command-with-args buffer)
  "Execute a string COMMAND-WITH-ARGS representing a shell command with arguments,
inserting the results in BUFFER."
  (switch-to-buffer buffer)
  (insert (format ">>> %s\n" command-with-args))
  (let* ((command-args-list (s-split " " command-with-args))
         (command (car command-args-list))
         (args (cdr command-args-list)))
    (apply 'call-process command nil output-buffer t args)))

(defun get-latest-version (repo-path)
  "Find the last tagged version. Assumes GNU sort (BSD sort lacks --version-sort)."
  (let ((default-directory repo-path))
    (s-trim
     (shell-command-to-string "git tag --list | sort --version-sort | tail -n 1"))))

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
  (let* ((output-buffer (get-buffer-create "*New release*"))
         (project-root (vc-git-root default-directory))
         (next-version (get-next-version project-root)))
    (switch-to-buffer output-buffer)
    (delete-region (point-min) (point-max))
    (execute-in-buffer (format "git flow release start %s" next-version) output-buffer)

    ;; copy the finish command to the kill ring and clipboard to save typing
    (kill-new (format "git flow release finish %s" next-version))))

(defun git-flow-release-push ()
  "After finishing a gitflow release, push it and move back to develop."
  (interactive)
  (let* ((output-buffer (get-buffer-create "*New release*"))
         (project-root (vc-git-root default-directory))
         (next-version (get-next-version project-root)))
    (switch-to-buffer output-buffer)
    (delete-region (point-min) (point-max))
    (execute-in-buffer "git push" output-buffer)
    (execute-in-buffer "git push --tags" output-buffer)
    (execute-in-buffer "git checkout develop" output-buffer)))

(provide 'editd-customisations)
