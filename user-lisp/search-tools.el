(autoload 'symbol-at-point "thingatpt")
(autoload 'ack-and-a-half "ack-and-a-half")

(autoload 'project-find-root "file-customisations")

(defun ack-at-point (search-term)
  "Run ack searching for string SEARCH-TERM, defaulting to the
current symbol at point."
 (interactive (list (read-from-minibuffer "Search with ack for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
 (ack search-term nil (project-find-root)))

(global-set-key (kbd "<f5>") 'ack-at-point)

(provide 'search-tools)