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

;;;; file creation
(defun file-create-file (path content)
  "Create a file at PATH with CONTENT as its contents."
  (with-temp-buffer
    (insert content)
    (write-region (point-min) (point-max) path)))

;;;; path searching

(defun find-path-parent-directory (path file-name)
  "Search PATH and all parent directories for file FILE-NAME,
returning the path where FILE-NAME can be found."
  (let ((directory-path (locate-dominating-file path file-name)))
    (when directory-path
        (file-path-join directory-path file-name))))

(autoload 'vc-git-root "vc-git")
(autoload 'vc-svn-root "vc-svn")

(defun file-find-project-root (path)
  "Find the probable root of the project for the current buffer."
  (or
   (vc-git-root path)
   (vc-svn-root path)
   (locate-dominating-file path "pom.xml")
   (error "%s doesn't seem to be part of a project" path)))

(autoload '--remove "dash" nil t)

(defun no-dot-directories (directories)
  "Exclude the . and .. directory from a list."
  (--remove (or (string= "." (file-name-nondirectory it))
                (string= ".." (file-name-nondirectory it)))
            directories))

(provide 'file-utils)
