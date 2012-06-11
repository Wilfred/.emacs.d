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

(require 'potato-customisations); buffer-contains-string-p
(autoload 'dolist "cl")
(autoload 'ido-completing-read "ido")

(autoload 'find-in-parent-directory "file-customisations")
(autoload 'path-for-current-buffer "file-customisations")

(defun java-show-test-failures ()
  "Show the failures from the last maven test run."
  (interactive)
  (let* ((target-directory (find-in-parent-directory (path-for-current-buffer) "target"))
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

(defun java-run-maven-tests ()
  "Run the test goal for the current project with maven."
  (interactive)
  (let ((pom-path (find-in-parent-directory (path-for-current-buffer) "pom.xml")))
    (compile (format "mvn -f %s test" pom-path))))

(provide 'java-customisations)