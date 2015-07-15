(require 'ido)

;; ido-mode -- fuzzy completion
(setq ido-enable-flex-matching t)

; reduce how often we get 'directory too big' problems:
(setq ido-max-directory-size 100000)

;; Show killed buffers at end when using ido for switching buffers.
(setq ido-use-virtual-buffers 'auto)

;; When opening files, create their parent directories if they don't exist.
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(provide 'ido-customisations)
