(defun buffer-contains-string-p (string)
  "Does the current buffer contain STRING? Case sensitive."
  (let ((case-fold-search nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (search-forward string nil t)))))

(defun replace-in-buffer (from-string to-string)
  "Replace string FROM-STRING with TO-STRING."
  (let ((case-fold-search nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward (regexp-quote from-string) nil t)
          (replace-match to-string t t))))))

(provide 'buffer-utils)
