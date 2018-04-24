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
  "Securely reseed Emacs' random number generator."
  (random (shell-command "cat < /dev/urandom | fold -w32 | head -n1")))

(defun wh/words ()
  (let ((buf (find-file-noselect "~/.emacs.d/user-lisp/words.txt")))
    (with-current-buffer buf
      (s-lines (s-trim (buffer-string))))))

(defun random-xkcd-password (&optional word-count)
  (interactive)
  (unless word-count
    (setq word-count 4))
  (let ((all-words (wh/words))
        words
        password)
    (dotimes (_ word-count)
      (push (random-choice all-words) words))
    (setq password (s-join " " words))
    (message "%s (also copied to clipboard)" password)
    (let ((select-enable-clipboard t))
      (kill-new password))))

(defun random-password ()
  "Generate a random 32 character string."
  (interactive)
  (random-reseed-securely)
  (let ((password
         (random-string
          32
          (split-string
           "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !&.|@"
           "" t))))
    (message "%s (also copied to clipboard)" password)
    (let ((x-select-enable-clipboard t))
      (kill-new password))))

(provide 'random-utils)
