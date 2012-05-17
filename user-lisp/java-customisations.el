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
(add-hook 'java-mode-hook
	  '(lambda ()
             (camelCase-mode)))

; java convenience functions
; TODO: write a Python-flavoured set of regexp utils
(defun string-match-all (regexp string &optional offset)
  "Find all the match positions of REGEXP in STRING, starting from
OFFSET (default 0). Case sensitive."
  (let* ((string-offset (if offset offset 0))
        (case-fold-search nil)
        (last-match-position (string-match regexp string string-offset)))
    (if last-match-position
     (cons
      (match-beginning 0)
      (string-regexp-positions regexp string (match-end 0)))
     nil)))

(defun list-pair-up (list)
  "Convert a list of the form (1 2 3 4) to ((1 2) (2 3) (3 4))"
  (let ((firsts (butlast list))
        (seconds (cdr list)))
    (mapcar* 'list firsts seconds)))

(defun split-string-with-sep (string separator)
  "Split STRING into substrings based on regexp SEPARATOR.
The separator is included in the subsequent substring

Example: (split-string-with-sep \"bazBoxBar\" \"B\")
    -> (\"baz\" \"Box\" \"Bar\"
"
  (let* ((substring-ends (string-match-all separator string))
        (substring-positions
         (list-pair-up (append '(0) substring-ends (list (length string))))))
    (mapcar (lambda (pos)
              (let ((start (first pos))
                    (end (second pos)))
                (substring string start end)))
            substring-positions)))

(defun java-variable-to-constant (variable-name)
  "Convert a string \"fooBar\" to \"FOO_BAR\"."
  (mapconcat 'upcase (split-string-with-sep variable-name "[A-Z]") "_"))

(defun java-constant-to-variable (constant-name)
  "Convert a string  \"FOO_BAR\" to \"fooBar\"."
  (let* ((camelcase-variable-name
          (apply 'concat
                 (mapcar 'capitalize (split-string constant-name "_")))
          )
         (first-char
          (downcase
           (substring camelcase-variable-name 0 1))))
    (concat
     first-char
     (substring capitalised-variable-name 1))))

(provide 'java-customisations)