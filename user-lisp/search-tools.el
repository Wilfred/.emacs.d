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

(provide 'search-tools)
