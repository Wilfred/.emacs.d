;;; tube.el --- get the London Tube status

;; Copyright (C) 2012 Wilfred Hughes <me@wilfred.me.uk>

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 26 June 2011
;; Version: 1.0
;; Keywords: london, tube

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;;; Commentary

;; This is was written for ErBot, but could easily be used for other
;; purposes.

(defun tube-get-status-json-string ()
  "Query the unofficial Tube status API and return the raw JSON response."
  (save-excursion
    (switch-to-buffer
     (url-retrieve-synchronously "http://api.tubeupdates.com/?method=get.status"))
    ;; for some reason, getting an application/x-json response means
    ;; we get the headers in our buffer too, so remove them:
    (goto-char (point-max))
    (move-beginning-of-line nil)
    (buffer-substring (point) (point-max))))

(defun tube-get-status ()
  "Query the unofficialy Tube status API and return a list of
strings, one per Tube line."
  (let* ((json-object-type 'hash-table)
         (status-json (json-read-from-string (tube-get-status-json-string)))
         (lines-vector (gethash "lines" (gethash "response" status-json))))
    (mapconcat
     (lambda (line-hash)
       (format "%s: %s"
               (replace-regexp-in-string "&amp;" "&" (gethash "name" line-hash))
               (gethash "status" line-hash)))
     lines-vector "\n")))

(defun fs-tube ()
  (tube-get-status))
