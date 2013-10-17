;; top of kill ring should also be in X clipboard
(setq x-select-enable-clipboard t)

(defun remove-all-text-properties (string)
  "Return a copy of STRING without any text properties."
  (let ((cleaned-string (copy-sequence string)))
    (set-text-properties 0 (length string) nil cleaned-string)
    cleaned-string))

(require 's)
(require 'ht)
(eval-when-compile '(require 'cl))

(defun kill-ring-choose ()
  "Pick any previously killed item (using ido) to insert into the buffer."
  (interactive)
  (let ((abbrevs (ht)))
    (dolist (item kill-ring)
      (setq item (remove-all-text-properties item))
      (ht-set abbrevs
              (s-truncate 40 (s-replace "\n" " " item))
              item))
    (insert
     (ht-get
      abbrevs
      (ido-completing-read "Previously killed: " (ht-keys abbrevs))))))

(provide 'kill-ring-customisations)
