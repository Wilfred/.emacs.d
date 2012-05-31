(autoload 'symbol-at-point "thingatpt")
(autoload 'ack-and-a-half "ack-and-a-half")

(defun ack-at-point (search-term)
  "Run ack searching for string SEARCH-TERM, defaulting to the
current symbol at point."
 (interactive (list (read-from-minibuffer "Search for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
 (ack search-term nil))

(global-set-key (kbd "<f5>") 'ack-at-point)

(provide 'search-tools)