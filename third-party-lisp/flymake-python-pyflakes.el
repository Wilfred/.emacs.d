;;; flymake-python-pyflakes.el --- A flymake handler for python-mode files using pyflakes as the backend checker

;; Copyright (C) 2012 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/flymake-python-pyflakes

;;; Commentary:

;; Usage:
;;   (require 'flymake-python-pyflakes)
;;   (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;;; Code:

(defvar flymake-python-pyflakes-allowed-file-name-masks '(("\\.py\\'" flymake-python-pyflakes-init)))
(defvar flymake-python-pyflakes-executable "pyflakes")

(defun flymake-python-pyflakes-init ()
  (list flymake-python-pyflakes-executable
        (list
         (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))

;;;###autoload
(defun flymake-python-pyflakes-load ()
  (interactive)
  (set (make-local-variable 'flymake-allowed-file-name-masks)
       flymake-python-pyflakes-allowed-file-name-masks)
  (if (executable-find flymake-python-pyflakes-executable)
    (flymake-mode t)
    (message "not enabling flymake: pyflakes executable '%s' not found"
             flymake-python-pyflakes-executable)))


(provide 'flymake-python-pyflakes)
;;; flymake-python-pyflakes.el ends here
