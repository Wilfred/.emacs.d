;;;; files related to buffers
(defun path-for-current-buffer ()
  "Find a directory path associated with the current buffer, if
possible. No trailing slash. Returns nil otherwise."
  (let ((file-name (buffer-file-name)))
    (if file-name (directory-file-name (file-name-directory file-name))
      default-directory)))

;;;; path manipulation
(defun file-path-join (directory-name file-name)
  "Join the relative FILE-NAME to DIRECTORY-NAME, adding slashes where appropriate."
  (concat (file-name-as-directory directory-name) file-name))

(defun parent-directory (path)
  (directory-file-name (file-name-directory path)))

;;;; path searching
(defun find-containing-parent-directory (path file-name)
  "Search PATH and all parent directories for file FILE-NAME,
returning the first containing diretory found or nil."
  (let* ((canonical-path (expand-file-name path))
         (absolute-file-path (file-path-join canonical-path file-name)))
    (cond ((file-exists-p absolute-file-path)
           ;; success -- we've found it!
           canonical-path)
          ((string= canonical-path "/")
           ;; reached root, the file doesn't exist in any ancestor directory
           nil)
          (t
           ;; recurse
           (find-containing-parent-directory
            (parent-directory canonical-path) file-name)))))

(defun find-path-parent-directory (path file-name)
  "Search PATH and all parent directories for file FILE-NAME,
returning the path where FILE-NAME can be found."
  (let ((directory-path (find-containing-parent-directory path file-name)))
    (when directory-path
        (file-path-join directory-path file-name))))

(defun file-find-project-root (path)
  "Find the probable root of the project for the current buffer.
TODO: svn"
  (or
   (find-containing-parent-directory path ".git")
   (find-containing-parent-directory path "pom.xml")))

(provide 'file-utils)