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

(defun java-variable-to-type (variable-name)
  "Convert a string \"fooBar\" to \"FooBar\"."
  (let ((first-char (substring variable-name 0 1))
        (rest (substring variable-name 1)))
    (concat (upcase first-char) rest)))

(defun java-type-to-constant (type-name)
  "Convert a string \"FooBar\" to \"FOO_BAR\"."
  (mapconcat 'upcase (re-find-all "[A-Z][a-z]+" type-name) "_"))

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

(defun toggle-case-first-char (string)
  (let ((first-char (substring string 0 1))
        (rest (substring string 1)))
    (if (re-match-p "[a-z]" first-char)
        (setq first-char (upcase first-char))
      (setq first-char (downcase first-char)))
    (concat first-char rest)))

(defun underscores-to-camelcase (variable-name)
  "Convert a string  \"foo_bar\" to \"fooBar\"."
  (toggle-case-first-char
   (mapconcat 'toggle-case-first-char (split-string variable-name "_") "")))

(require 'cl); first, second

(defun java-cycle-case ()
  "Convert toggle symbol at mark between the forms \"fooBar\",
\"FooBar\", \"FOO_BAR\" and \"foo_bar\"."
  (interactive)
  (let* ((symbol (symbol-name (symbol-at-point)))
        (symbol-bounds (bounds-of-thing-at-point 'symbol))
        (bound-start (first symbol-bounds))
        (bound-end (second symbol-bounds)))
    (when symbol-bounds
      (goto-char bound-start)
      (kill-forward-chars (- bound-end bound-start))
      (cond
       ((re-match-p "[a-z_]+$" symbol) (message (insert (underscores-to-camelcase symbol))))
       ((re-match-p "[a-z]+" symbol) (insert (java-variable-to-type symbol)))
       ((re-match-p "[A-Z][a-z]+" symbol) (insert (java-type-to-constant symbol)))
       (t (insert (downcase symbol)))))))

(define-key java-mode-map (kbd "C-M-c") 'java-cycle-case)

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

(defun eclim-maven-run-tests ()
  "Run the test goal for the current project with maven."
  (interactive)
  (eclim-maven-run "test"))

(provide 'java-customisations)