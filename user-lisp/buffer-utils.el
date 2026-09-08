;;; -*- lexical-binding: nil; -*-
(defun buffer-contains-string-p (string)
  "Does the current buffer contain STRING? Case sensitive."
  (let ((case-fold-search nil))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (search-forward string nil t)))))

(provide 'buffer-utils)
