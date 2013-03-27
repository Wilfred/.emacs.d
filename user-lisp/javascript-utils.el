(require 'buffer-utils)

(defun indent-json ()
  "Run Python's JSON indenter on the buffer"
  (interactive)
  (save-excursion
    (shell-command-on-region
     (point-min) (point-max) "python2 -m json.tool"
     (current-buffer)
     t)))
