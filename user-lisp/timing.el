(defun time-as-unixtime (most-sig-bits least-sig-bits microseconds)
  "Return the number of seconds since 1st of January 1970."
  (+ (lsh most-sig-bits 16) least-sig-bits
     (/ microseconds 1000000.0)))

(defun time-difference (from-time to-time)
  "Calculate the time difference in seconds between FROM-TIME and
TO-TIME. Parameters are assumed to be in the format returned
by (current-time)."
  (let* ((start-time (apply 'time-as-unixtime from-time))
      (finish-time (apply 'time-as-unixtime to-time)))
    (- finish-time start-time)))

(defmacro timed-require (feature)
  (let ((timing-var (make-symbol "start-time")))
    `(let ((,timing-var (current-time)))
       (require ,feature)
       (message "(require \'%s) took %.2f seconds."
                (symbol-name ,feature)
                (time-difference ,timing-var (current-time))))))

