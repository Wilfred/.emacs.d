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

(require 'regexp-utils); re-find-all

; java convenience functions
; TODO: write a Python-flavoured set of regexp utils
(defun java-variable-to-constant (variable-name)
  "Convert a string \"fooBar\" to \"FOO_BAR\"."
  (mapconcat 'upcase (re-find-all "[A-Z]?[a-z]+" variable-name) "_"))

(defun java-constant-to-variable (constant-name)
  "Convert a string  \"FOO_BAR\" to \"fooBar\"."
  (let* ((camelcase-variable-name
          (apply 'concat
                 (mapcar 'capitalize (split-string constant-name "_"))))
         (first-char
          (downcase
           (substring camelcase-variable-name 0 1))))
    (concat
     first-char
     (substring camelcase-variable-name 1))))


(defun java-toggle-case ()
  "Convert toggle symbol at mark between variable and constant formatting."
  (interactive)
  (let* ((symbol (symbol-name (symbol-at-point)))
        (symbol-bounds (bounds-of-thing-at-point 'symbol))
        (bound-start (car symbol-bounds))
        (bound-end (cdr symbol-bounds)))
    (when symbol-bounds
      (goto-char bound-start)
      (kill-forward-chars (- bound-end bound-start))
      (if (re-search-p "[a-z]" symbol)
          (progn
            (message (java-constant-to-variable symbol))
            (insert (java-variable-to-constant symbol)))
        (insert (java-constant-to-variable symbol))))))

(define-key java-mode-map (kbd "C-M-c") 'java-toggle-case)

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
(require 'cl); dolist
(require 'ido); ido-completing-read

(defun java-show-test-failures ()
  "Show the failures from the last maven test run."
  (interactive)
  (let* ((test-results-directory (concat (project-find-root) "target/surefire-reports"))
        (result-files (directory-files test-results-directory))
        (failed-tests nil))
    ; iterate over all the files, open and read them, then kill them
    (dolist (file-name result-files)
      (when (string-match ".txt$" file-name)
        (with-temp-buffer
          (insert-file-contents (concat test-results-directory "/" file-name))
          (if (buffer-contains-string-p "FAILURE")
              (progn
                (setq failed-tests (cons file-name failed-tests)))))))
    ; let the user choose which failure they want to see
    (if failed-tests
        (progn
          (find-file (concat test-results-directory "/"
                             (ido-completing-read "Pick a failed class: " failed-tests)))
          (compilation-mode))
      (message "No failed tests!"))))

(provide 'java-customisations)