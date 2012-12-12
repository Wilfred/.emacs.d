(autoload 'symbol-at-point "thingatpt")
(autoload 'ack-and-a-half "ack-and-a-half")

;; ack configuration
(setq ack-and-a-half-arguments '("--all-types"))
(setq ack-and-a-half-executable "ack")

(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)


(autoload 'file-find-project-root "file-utils")

(defun ack-at-point (search-term)
  "Run ack searching for string SEARCH-TERM, defaulting to the
current symbol at point."
 (interactive (list (read-from-minibuffer "Search with ack for: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
 (ack search-term nil (file-find-project-root default-directory)))

(global-set-key (kbd "<f5>") 'ack-at-point)


(autoload '--remove "dash" nil t)

(setq virtualenv-base-path "/home/wilfred/.envs")
(defun ack-in-virtualenv ()
  "Search the source code in a virtual environment for
a specific search string."
 (interactive)
 (let* ((virtualenv-names
        (--remove
         (or (equal "." it) (equal ".." it))
         (directory-files virtualenv-base-path)))
       (virtualenv-name (ido-completing-read "Virtualenv: " virtualenv-names))
       (virtualenv-path (concat virtualenv-base-path "/" virtualenv-name "/lib/python2.7/site-packages"))
       (search-term (read-from-minibuffer "Search virtualenv for: "
                                          (if (symbol-at-point)
                                              (symbol-name (symbol-at-point))))))
   (ack search-term nil virtualenv-path)))

(provide 'search-tools)
