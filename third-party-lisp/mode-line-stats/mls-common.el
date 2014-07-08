;; Various utilities

(require 'cl)

(defun mls-format-expand (formatters format &optional arg)
  "Formats `format' according to `formatters' passing `arg' as an optional argument."
  (let ((regex (concat "%\\("
                       (reduce (lambda (a b)
                                 (concat a "\\|" b))
                               (mapcar #'car formatters))
                       "\\)")))
    (replace-regexp-in-string regex
                              (lambda (str)
                                (let ((fun (assoc (substring str 1)
                                                  formatters)))
                                  (if fun
                                      (funcall (cdr fun) arg)
                                      (error "Unrecognized format sequence: %s" str))))
                              format t t)))

(defun mls-mapcar* (f &rest xs)
  "MAPCAR for multiple sequences"
  (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
            (apply 'mls-mapcar* f (mapcar 'cdr xs)))))

(defun mls-cancel-timer (module-timer)
  "Cancel the MODULE-TIMER."
  (let ((timer-value (symbol-value module-timer)))
    (set module-timer
       (and timer-value (cancel-timer timer-value)))))

(defun mls-set-timer (module-timer module-interval module-function)
  "Set the timer MODULE-TIMER with MODULE-INTERVAL and MODULE-FUNCTION."
  (mls-cancel-timer module-timer)
  (set module-timer (run-at-time module-interval
                                 module-interval
                                 module-function)))

(provide 'mls-common)
