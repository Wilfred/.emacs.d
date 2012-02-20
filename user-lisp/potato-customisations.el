(defun buffer-contains-string-p (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun replace-in-buffer (from-string to-string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward from-string nil t)
        (replace-match to-string nil t)))))

(defun gap2-toggle-application ()
  "Toggle between gap2-staging and gap2-production in app.yaml"
  (interactive)
  (save-excursion
    (set-buffer "app.yaml")
    (if (buffer-contains-string-p "application: gap2-staging")
        (replace-in-buffer "application: gap2-staging" "application: gap2-production")
      (replace-in-buffer "application: gap2-production" "application: gap2-staging"))))

(define-key yaml-mode-map (kbd "<f11>") 'gap2-toggle-application)

(provide 'potato-customisations)