(require 'f)

;;;; files related to buffers
(defun path-for-current-buffer ()
  "Find a directory path associated with the current buffer, if
possible. No trailing slash. Returns nil otherwise."
  (let ((file-name (buffer-file-name)))
    (if file-name (directory-file-name (file-name-directory file-name))
      default-directory)))

;;;; path searching

(defun find-path-parent-directory (path file-name)
  "Search PATH and all parent directories for file FILE-NAME,
returning the path where FILE-NAME can be found."
  (let ((directory-path (locate-dominating-file path file-name)))
    (when directory-path
        (f-join directory-path file-name))))

(provide 'file-utils)
