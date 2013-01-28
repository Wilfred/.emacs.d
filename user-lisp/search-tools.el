(autoload 'symbol-at-point "thingatpt")
(autoload 'ack-and-a-half "ack-and-a-half")

;; ack configuration
(setq ack-and-a-half-executable "ack")

(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(defalias 'ack 'ack-and-a-half)


(autoload 'file-find-project-root "file-utils")

(defun ack-at-point-everything (search-term)
  "Run ack searching for string SEARCH-TERM, defaulting to the
current symbol at point. This searches all files, whether or
not they look like source files."
 (interactive (list (read-from-minibuffer "Search all files for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
 (let ((ack-and-a-half-arguments '("--all-types")))
   (ack search-term nil (file-find-project-root default-directory))))

(defun ack-at-point-source (search-term)
  "Run ack searching for string SEARCH-TERM, defaulting to the
current symbol at point. This only searches files that look
like source files."
 (interactive (list (read-from-minibuffer "Search source files for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
 (ack search-term nil (file-find-project-root default-directory)))

(setq virtualenv-base-path "/home/wilfred/.py_envs")

(require 'ag)
(global-set-key (kbd "<f5>") 'ag-project-at-point)

(require 'file-utils)
(defun ag-in-virtualenv ()
  "Search the soruce code in a virtual environment for
a specific search string."
  (interactive)
  (let* ((virtualenv-names
          (no-dot-directories (directory-files virtualenv-base-path)))
         (virtualenv-name (ido-completing-read "Virtualenv: " virtualenv-names))
         (virtualenv-path (concat virtualenv-base-path "/" virtualenv-name "/lib/python2.7/site-packages"))
         (search-term (read-from-minibuffer "Search virtualenv for: "
                                            (if (symbol-at-point)
                                                (symbol-name (symbol-at-point))))))
    (ag/search search-term virtualenv-path)))

(provide 'search-tools)
