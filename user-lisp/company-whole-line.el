;; TODO: proper header.

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
      (s-trim-left (buffer-substring line-start line-end)))))

(defun cwl--matching-lines (prefix buffer)
  "Return all the lines in BUFFER that start with PREFIX."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (cl-loop
       if (and
           (looking-at
            ;; The current prefix followed some additional characters.
            (rx-to-string `(seq (0+ whitespace) ,prefix (1+ not-newline))))
           ;; The line is different to the prefix (so we don't offer
           ;; the current prefix as an option).
           (not (equal (cwl--current-line) prefix)))
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

(defun cwl--candidates (prefix)
  "Return all the lines in all buffers matching this major mode,
where the line starts with PREFIX."
  (cl-loop for buffer in (cwl--buffers-in-mode major-mode)
           append (cwl--matching-lines (s-trim-left prefix) buffer)))

(defun company-whole-line (command &optional arg &rest ignored)
  "A company backend that finds lines in all buffers (in the same major mode)
that start with the current line at point."
  (interactive (list 'interactive))
  ;; TODO: sort.
  (cl-case command
    (interactive (company-begin-backend 'company-whole-line))
    (prefix
     ;; We can complete if we're at the end of a non-empty line,
     (when (and (eolp) (not (cwl--empty-line-p))
                ;; and we're not in a comment/string.
                (not (company-in-string-or-comment)))
       ;; Note that company only searches other completion backends
       ;; using the prefix found by the first backend.
       ;; https://github.com/company-mode/company-mode/issues/47#issuecomment-33472005
       ;; You should ensure `company-whole-line' is near the end of
       ;; `company-backends' or use the package company-try-harder.
       (cwl--current-line)))
    (candidates
     (cwl--candidates arg))
    (duplicates t)
    (meta
     (format "Line %d from %s"
             (get-text-property 0 'line-number arg)
             (get-text-property 0 'buffer arg)))))

(provide 'company-whole-line)
   
