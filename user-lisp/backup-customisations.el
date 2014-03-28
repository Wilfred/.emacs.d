(require 'f)
(require 'backup-each-save)

(defun backup-each-save-show ()
  "Show the directory (using dired) containing backups of the current buffer."
  (interactive)
  (dired
   (f-dirname (f-expand (backup-each-save-compute-location (buffer-file-name))))))
