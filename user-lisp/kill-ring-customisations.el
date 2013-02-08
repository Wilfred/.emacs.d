;; top of kill ring should also be in X clipboard
(setq x-select-enable-clipboard t)

(defun remove-all-text-properties (string)
  "Return a copy of STRING without any text properties."
  (let ((cleaned-string (copy-sequence string)))
    (set-text-properties 0 (length string) nil cleaned-string)
    cleaned-string))

(defun kill-ring-choose ()
  "Pick any previously killed item (using ido) to insert into the buffer."
  (interactive)
  (insert (ido-completing-read "Previously killed: "
           (mapcar 'remove-all-text-properties kill-ring))))

(provide 'kill-ring-customisations)
