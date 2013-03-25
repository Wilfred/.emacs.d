(defun buffer-contains-string-p (string)
  "Does the current buffer contain STRING? Case sensitive."
  (let ((case-fold-search nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (search-forward string nil t)))))

(defun replace-in-buffer (from-string to-string)
  "Replace string FROM-STRING with TO-STRING."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward from-string nil t)
        (replace-match to-string nil t)))))

(provide 'buffer-utils)
