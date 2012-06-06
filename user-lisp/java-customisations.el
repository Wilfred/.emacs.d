; configure eclim, which lets use use eclispe as a server
(add-to-list 'load-path (expand-file-name "~/.emacs.d/third-party-lisp/emacs-eclim"))
(require 'eclim)
(require 'cc-mode)

(setq eclim-executable (expand-file-name "~/.eclipse/org.eclipse.platform_3.7.0_155965261/eclim"))

(global-eclim-mode)

; show eclim errors in minibuffer
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

; eclim key bindings
(define-key java-mode-map (kbd "<f6>") 'eclim-java-find-declaration)


; treat camelCaseWords as different words with M-f and M-b
(add-hook 'java-mode-hook 'camelCase-mode)

(defun find-in-parent-directory (path file-name)
  "Search PATH and all parent directories for file FILE-NAME,
returning the first path found or nil."
  (let* ((abs-directory-path (expand-file-name path))
         (abs-file-path (concat abs-directory-path file-name)))
    (if (file-exists-p abs-file-path)
        ; success -- we've found it!
        abs-directory-path
      (if (string= abs-directory-path "/")
          nil
        (find-in-parent-directory (concat abs-directory-path "../") file-name)))))

(defun project-find-root ()
  "Find the probable root of the project for the current buffer.
TODO: svn"
  (let ((current-directory (expand-file-name ".")))
    (or
     (find-in-parent-directory current-directory ".git")
     (find-in-parent-directory current-directory "pom.xml"))))

(require 'potato-customisations); buffer-contains-string-p
(autoload 'dolist "cl")
(autoload 'ido-completing-read "ido")

(defun java-show-test-failures ()
  "Show the failures from the last maven test run."
  (interactive)
  (let* ((current-directory (expand-file-name "."))
         (target-directory (find-in-parent-directory current-directory "target"))
         (test-results-directory (concat target-directory "target/surefire-reports"))
         (result-files (directory-files test-results-directory))
         (failed-tests nil))
    ;; iterate over all the files, open and read them, then kill them
    (dolist (file-name result-files)
      (when (string-match ".txt$" file-name)
        (with-temp-buffer
          (insert-file-contents (concat test-results-directory "/" file-name))
          (if (buffer-contains-string-p "FAILURE")
              (progn
                (setq failed-tests (cons file-name failed-tests)))))))
    ;; let the user choose which failure they want to see
    (if failed-tests
        (progn
          (find-file (concat test-results-directory "/"
                             (ido-completing-read "Pick a failed class: " failed-tests)))
          (compilation-mode))
      (message (format "No failed tests in %s." test-results-directory)))))

(defun eclim-maven-run-tests ()
  "Run the test goal for the current project with maven."
  (interactive)
  (eclim-maven-run "test"))

(provide 'java-customisations)