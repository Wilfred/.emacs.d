(defun buffer-contains-string-p (string)
  "Does the current buffer contain STRING? Case sensitive."
  (let ((case-fold-search nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (search-forward string nil t)))))

(defun replace-in-buffer (from-string to-string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward from-string nil t)
        (replace-match to-string nil t)))))

(defcustom potato-path "~/work/potato"
  "Path to directory containing all Potato projects."
  :group 'potato-customisations)

(defun gap2-cycle-application ()
  "Cycle through gap2-staging, gap2-production and gap2-proto in app.yaml"
  (interactive)
  (save-excursion
    (set-buffer (find-file (concat potato-path
                                   "/gap2/app.yaml")))
    
    (cond ((buffer-contains-string-p "application: gap2-staging")
           (replace-in-buffer "application: gap2-staging" "application: gap2-production"))
          ((buffer-contains-string-p "application: gap2-production")
           (replace-in-buffer "application: gap2-production" "application: gap2-proto"))
          (t
           (replace-in-buffer "application: gap2-proto" "application: gap2-staging")))))


(define-key yaml-mode-map (kbd "<f11>") 'gap2-cycle-application)

(provide 'potato-customisations)