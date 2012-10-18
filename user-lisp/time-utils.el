(defun time-list-to-float (most-sig-bits least-sig-bits microseconds)
  "Return a float of seconds since 1st of January 1970."
  (+ (lsh most-sig-bits 16) least-sig-bits
     (/ microseconds 1000000.0)))

(defun time-float-to-list (float)
  "Inverse of `time-list-to-float'."
  (let ((high (lsh (floor float) -16))
        (low (floor (mod float (expt 2 16))))
        (microseconds (floor (* (- float (floor float)) 1000000))))
    (list high low microseconds)))

(defun time-iso-8601 (time-in-seconds)
  "Convert a unix timestamp to ISO 8601 format. We assume the timestamp is in UTC."
  (concat
   (format-time-string "%Y-%m-%dT%T" (list (rsh time-in-seconds )) t)
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

(defun time-readable-format (timestamp)
  "Given a timestamp in seconds or milliseconds, display
this date/time in a readable format."
  (interactive "nTimestamp: ")
  (if (> timestamp 2000000000)
      ;; very big; must be milliseconds
      (setq timestamp (/ timestamp 1000)))
  (message (time-iso-8601 (time-float-to-list timestamp))))
