(defun url-get (url)
  "HTTP GET to URL."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (buffer-string)))

(provide 'url-utils)
