;;; eclim, which lets us use eclispe as a server

(setq eclim-executable (expand-file-name "~/.eclipse/org.eclipse.platform_3.7.0_155965261/eclim"))

(defvar eclim-in-use nil)
(defun eclim-switch-on ()
  "Turn on eclim mode. We wrap this stuff in a function as eclim
is quite invasive, messing with (amongst others) after-save-hook."
  (unless eclim-in-use
    (setq eclim-in-use t)
    (require 'eclim)

    ;; show eclim errors in minibuffer
    (setq help-at-pt-display-when-idle t)
    (setq help-at-pt-timer-delay 0.3)
    (help-at-pt-set-timer)))


;; use eclim-mode only for Java
(add-hook 'java-mode-hook '(lambda () (progn (eclim-switch-on) (eclim-mode 1))))

;; eclim key bindings
(use-package cc-mode
  :config
  (define-key java-mode-map (kbd "<f6>") 'eclim-java-find-declaration))

; treat camelCaseWords as different words with M-f and M-b
(add-hook 'java-mode-hook '(lambda () (subword-mode 1)))

(require 'f)

(defun java-show-test-failures ()
  "Show the failures from the last maven test run."
  (interactive)
  (let* ((target-directory (locate-dominating-file (path-for-current-buffer) "target"))
         (test-results-directory (f-join target-directory "target/surefire-reports"))
         (result-files (directory-files test-results-directory))
         (failed-tests nil))
    ;; iterate over all the files, open and read them, then kill them
    (dolist (file-name result-files)
      (when (string-match ".txt$" file-name)
        (with-temp-buffer
          (insert-file-contents (f-join test-results-directory file-name))
          (if (buffer-contains-string-p "FAILURE")
              (progn
                (setq failed-tests (cons file-name failed-tests)))))))
    ;; let the user choose which failure they want to see
    (if failed-tests
        (progn
          (find-file (f-join test-results-directory
                             (completing-read "Pick a failed class: " failed-tests)))
          (compilation-mode))
      (message (format "No failed tests in %s." test-results-directory)))))

(defun java-maven-run-tests ()
  "Run the test goal for the current project with maven."
  (interactive)
  (let ((pom-path (find-path-parent-directory (path-for-current-buffer) "pom.xml")))
    (compile (format "mvn -f %s test" pom-path))))

(provide 'java-customisations)
