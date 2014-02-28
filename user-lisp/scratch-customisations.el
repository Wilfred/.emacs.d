

;; Persistent scratch and other scratch conveniences, based on
;; http://dorophone.blogspot.co.uk/2011/11/how-to-make-emacs-scratch-buffer.html
;; and http://stackoverflow.com/questions/234963/re-open-scratch-buffer-in-emacs

;; todo: namespace, polish, release on melpa

(defvar persistent-scratch-filename 
  (expand-file-name "~/.emacs-persistent-scratch")
    "Location of *scratch* file contents for persistent-scratch.")
(defvar persistent-scratch-backup-directory 
  (expand-file-name "~/.emacs-persistent-scratch-backups/")
    "Location of backups of the *scratch* buffer contents for
    persistent-scratch.")


(defun make-persistent-scratch-backup-name ()
  "Create a filename to backup the current scratch file by
concatenating PERSISTENT-SCRATCH-BACKUP-DIRECTORY with the
current date and time."
  (concat
   persistent-scratch-backup-directory
   (format-time-string "%y-%m-%d_%H-%M-%S_%s" (current-time))))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME, making a backup copy in
  PERSISTENT-SCRATCH-BACKUP-DIRECTORY."
  (with-current-buffer (get-buffer "*scratch*")
    (if (file-exists-p persistent-scratch-filename)
        (copy-file persistent-scratch-filename
                   (make-persistent-scratch-backup-name)))
    (write-region (point-min) (point-max) 
                  persistent-scratch-filename)))

(defun load-persistent-scratch ()
  "Load the contents of PERSISTENT-SCRATCH-FILENAME into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (shell-command (format "cat %s" persistent-scratch-filename) (current-buffer)))))

(push #'save-persistent-scratch kill-emacs-hook)

(defun recreate-scratch-buffer nil
  "Create or recreate the scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)) 

(defadvice kill-buffer (around kill-buffer-unless-scratch activate)
  "Bury *scratch* buffer instead of killing it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(provide 'scratch-customisations)
