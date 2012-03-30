(autoload 'symbol-at-point "thingatpt" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)

(defun string-nonempty-p (string)
  "Return t if STRING is non-null and non-empty."
  (if (stringp string)
      (if (> (length string) 0) t
          nil)
      nil))

; todo: get directory from vc-git-root
(defun ack-project ()
  "Search the current project's directory for SEARCH-TERM, or the
symbol under point."
  (interactive)
  (let* ((current-symbol (symbol-name (symbol-at-point)))
         (search-message (if current-symbol
                             (concat "Search for (default '" current-symbol "'): ")
                           "Search for: "))
        (search-term (read-from-minibuffer search-message)))

    (ack-and-a-half 
     (if (string-nonempty-p search-term) search-term current-symbol))))

(global-set-key (kbd "<f5>") 'ack-project)
