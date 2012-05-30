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

(defun string-contains-p (substring string)
  "Returns t if STRING contains SUBSTRING."
  (re-search-p (regexp-quote substring) string))

(defun format-symbol (string format)
  "Convert a given string to a specified formatting convention.

(format-symbol \"fooBar\" 'constant) => \"FOO_BAR\""
  (let ((components))
    ;; split the string into its word components
    (if (string-contains-p "_" string)
        (setq components (split-string string "_"))
      (setq components (re-find-all "[A-Z]?[a-z]+" string)))
    ;; format each component as a lowercase string
    (setq components (mapcar 'downcase components))
    (cond
     ((eq format 'constant) (mapconcat 'upcase components "_"))
     ((eq format 'camelcase) (mapconcat 'toggle-case-first-char components ""))
     ((eq format 'camelcase-lower) (toggle-case-first-char
                                    (mapconcat 'toggle-case-first-char components "")))
     ((eq format 'variable-underscore) (mapconcat (lambda (x) x) components "_"))
     ((eq format 'variable-hyphen) (mapconcat (lambda (x) x) components "-"))
     (t (error "Unknown symbol format")))))

(defun toggle-case-first-char (string)
  (let ((first-char (substring string 0 1))
        (rest (substring string 1)))
    (if (re-match-p "[a-z]" first-char)
        (setq first-char (upcase first-char))
      (setq first-char (downcase first-char)))
    (concat first-char rest)))

(require 'cl); first, second

(defun java-cycle-case ()
  "Convert toggle symbol at mark between the forms \"fooBar\",
\"FooBar\", \"FOO_BAR\" and \"foo_bar\"."
  (interactive)
  (let* ((symbol (symbol-name (symbol-at-point)))
        (symbol-bounds (bounds-of-thing-at-point 'symbol))
        (bound-start (car symbol-bounds))
        (bound-end (cdr symbol-bounds)))
    (when symbol-bounds
      (goto-char bound-start)
      (kill-forward-chars (- bound-end bound-start))
      (cond
       ((re-match-p "[a-z_]+$" symbol) (insert (format-symbol symbol 'camelcase-lower)))
       ((re-match-p "[a-z]+" symbol) (insert (format-symbol symbol 'camelcase)))
       ((re-match-p "[A-Z][a-z]+" symbol) (insert (format-symbol symbol 'constant)))
       (t (insert (format-symbol symbol 'variable-underscore)))))))

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