(require 'dash)

(defun full-stack--format (fn args)
  (let* ((formatted-args
          (--map
           (cond
            ((memq it '(nil t))
             (prin1-to-string it))
            ((keywordp it)
             (prin1-to-string it))
            ((symbolp it)
             (format "'%s" it))
            (t
             (prin1-to-string it)))
           args)))
    (cond
     ((byte-code-function-p fn)
      "<bytecode>")
     (args
      (format "(%s %s)"
              fn
              (s-join " " formatted-args)))
     (t
      (format "(%s)" fn)))))

(defun full-stack--buffer ()
  (let ((buf (get-buffer-create "*full-stack*")))
    (switch-to-buffer buf)))

(defun full-stack ()
  (interactive)
  (full-stack--buffer)
  (erase-buffer)
  (let (frames)
    (let* ((i 0)
           (frame (backtrace-frame i)))
      (catch 'done
        (while frame
          (push frame frames)
          (setq i (1+ i))
          (setq frame (backtrace-frame i)))))
    (let ((last-func nil))
      (dolist (frame frames)
        (-let [(evalled func . args) frame]
          (if evalled
              (insert (format "  %s\n"
                              (full-stack--format func args)))
            (insert (format "  (%s ...)\n" func)))
          (when (functionp func)
            (insert (format "%s:\n" func))))))
    ))
