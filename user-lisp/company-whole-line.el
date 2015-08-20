(require 'cl-lib)
(require 'company)
(require 's)

(defun cwl--empty-line-p ()
  "Return non-nil if the current line is empty."
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at (rx (0+ whitespace) line-end))))

(defun cwl--last-line-p ()
  "Return non-nil if the current line is the last in the buffer."
  (looking-at (rx (0+ not-newline) buffer-end)))

(defun cwl--current-line ()
  "Return the whole line at point."
  (save-excursion
    (let ((line-start (progn (beginning-of-line) (point)))
          (line-end (progn (end-of-line) (point))))
      (buffer-substring line-start line-end))))

(defun cwl--matching-lines (prefix buffer)
  "Return all the lines in BUFFER that start with PREFIX."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (cl-loop
       if (looking-at
           ;; The current prefix followed some additional characters.
           (rx-to-string `(seq (0+ whitespace) ,prefix (1+ not-newline))))
       collect (propertize
                (cwl--current-line)
                'buffer buffer
                'line-number (line-number-at-pos))
       until (cwl--last-line-p)
       do (forward-line)))))

(defun cwl--buffers-in-mode (mode)
  "Return a list of all the buffers with major mode MODE."
  (cl-loop for buffer in (buffer-list)
           if (with-current-buffer buffer
                (eq major-mode mode))
           collect buffer))

(defun company-whole-line (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  ;; TODO: sort.
  (cl-case command
    (interactive (company-begin-backend 'company-whole-line))
    ;; We can complete if we're at the end of a non-empty line.
    (prefix (when (and (eolp) (not (cwl--empty-line-p)))
              (cwl--current-line)))
    (candidates
     (cl-loop for buffer in (cwl--buffers-in-mode major-mode)
              append (cwl--matching-lines (s-trim arg) buffer)))
    (duplicates t)
    (meta
     (format "Line %d from %s"
             (get-text-property 0 'line-number arg)
             (get-text-property 0 'buffer arg)))))

(add-to-list 'company-backends 'company-whole-line)

(provide 'company-whole-line)
   
