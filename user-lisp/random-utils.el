;;; random-utils.el --- Missing random functions in elisp

;; Copyright (C) 2013 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 3 December 2013
;; Version: 1.0

(defun random-choice (list)
  "Return a random element from LIST."
  (let ((random-index (random (length list))))
    (nth random-index list)))

(defun random-string (length chars)
  "Generate a random string of LENGTH using characters in list CHARS."
  (let ((string ""))
    (while (< (length string) length)
      (setq string (concat string (random-choice chars))))
    string))

(defun random-reseed-securely ()
  "Securely reseed Emacs' random number generator.
By default, the random number generator is only seeded with the
current time the current Emacs PID."
  (random (shell-command "cat < /dev/urandom | fold -w32 | head -n1")))

(defun random-password ()
  "Generate a random 32 character string and "
  (interactive)
  (random-reseed-securely)
  (let ((password (random-string
                   32
                   ;; split the string into a list of of one-character strings
                   (mapcar 'string
                           "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !&.|@"))))
    (message "%s (also copied to clipboard)" password)
    (let ((x-select-enable-clipboard t))
      (kill-new password))))

(provide 'random-utils)


