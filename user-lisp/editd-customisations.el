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

(defun cut-new-release ()
  "Increment the version number, use gitflow to mark a new release, and push it."
  (interactive)
  ;; todo: get version number and increment
  ;; todo: check we're in an Editd buffer
  (let ((output-buffer (get-buffer-create "*New release*")))
    (switch-to-buffer output-buffer)
    (delete-region (point-min) (point-max))
    (execute-in-buffer "git push" output-buffer)
    (execute-in-buffer "git push --tags" output-buffer)))
