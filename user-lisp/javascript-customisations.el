; indent with tab characters (treating them as eight spaces)
(setq-default js-indent-level 8)
(setq tab-width 8)
(add-hook 'js-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)))

(require 'flymake)

; use jshint to check code (use $ npm install -g jshint)
(defun flymake-jshint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
         (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
   (list "jshint" (list local-file "--config" (expand-file-name "~/.emacs.d/user-js/jshint.json")))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js$"
	      flymake-jshint-init
	      flymake-simple-cleanup
	      flymake-get-real-file-name)
	    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns 
      (cons '("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$"
	      1 2 3 4)
	    flymake-err-line-patterns))

; load flymake-mode with js-mode
(add-hook 'js-mode-hook 'flymake-mode)

(provide 'javascript-customisations)