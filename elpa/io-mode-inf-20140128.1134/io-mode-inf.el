;;; io-mode-inf.el --- Interaction with an Io interpreter.

;; Copyright (C) 2010 Seth Mason
;; Authors: Seth Mason
;; Keywords: io languages
;; Package-Version: 20140128.1134
;; URL: https://github.com/slackorama/io-emacs

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Run an Io interpreter in Emacs.

;;; Code:

(require 'comint)

(defgroup io-mode-inf
  nil
  "Mode to interact with a Io interpreter."
  :group 'io
  :tag "Inferior Io")

(defcustom io-interpreter "io"
  "The interpreter that `io-run-io' should run. This should
 be a program in your PATH or the full pathname of the io interpreter."
  :type 'string
  :group 'io-mode-inf)

(defconst io-inf-buffer-name "*inferior-io*")

(add-to-list 'same-window-buffer-names io-inf-buffer-name)

(define-derived-mode io-mode-inf comint-mode "Inferior io"
  "Major mode for interacting with a Io interpreter.

\\{inferior-io-mode-map\\}"
  (define-key io-mode-inf-map [(meta return)] 'comint-accumulate)

  ;; Comint configuration
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'io-input-sender))

(defun io-input-sender (proc string)
  (comint-send-string proc string)
  ;; (comint-send-string proc "\nemacs:end\n")) ;; Heineman's contrib (06/03/2007)
  (comint-send-string proc "\n"))

(defun io-interpreter-running-p-1 ()
  ;; True iff a io interpreter is currently running in a buffer.
  (comint-check-proc io-inf-buffer-name))

(defun io-check-interpreter-running ()
  (unless (io-interpreter-running-p-1)
    (error "Io interpreter not running")))

;;;###autoload
(defun io-run-io (&optional cmd-line)
  "Run a Io interpreter in an Emacs buffer"
  (interactive (list (if current-prefix-arg
			 (read-string "Io interpreter: " io-interpreter)
                       io-interpreter)))
  (unless (io-interpreter-running-p-1)
    (setq io-interpreter cmd-line)
    (let ((cmd/args (split-string cmd-line)))
      (set-buffer
       (apply 'make-comint "inferior-io" (car cmd/args) nil (cdr cmd/args))))
    (io-mode-inf)
    (pop-to-buffer io-inf-buffer-name)))

(defun io-send-string (str &rest args)
  ;; Send string to interpreter
  (comint-send-string io-inf-buffer-name (apply 'format str args))
  ;; (comint-send-string io-inf-buffer-name "\nemacs:end\n")) Heineman's contrib (06/03/2007)
  (comint-send-string io-inf-buffer-name "\n"))

;;;###autoload
(defun io-switch-to-interpreter ()
  "Switch to buffer containing the interpreter"
  (interactive)
  (io-check-interpreter-running)
  (switch-to-buffer io-inf-buffer-name))

;;;###autoload
(defun io-eval-region (start end)
  "Send current region to io interpreter."
  (interactive "r")
  (io-check-interpreter-running)
  (comint-send-region io-inf-buffer-name start end)
  (comint-send-string io-inf-buffer-name "\n"))

;;;###autoload
(defun io-eval-buffer ()
  "Send whole buffer to Io interpreter."
  (interactive)
  (io-eval-region (point-min) (point-max)))

(defvar io-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last io-load-file.
Used for determining the default in the next one.")

;;;###autoload
(defun io-load-file (file-name)
  "Load a file in the Io interpreter."
  (interactive (comint-get-source "Load Io file: " io-prev-l/c-dir/file
				  '(io-mode) t))
  (io-check-interpreter-running)
  (comint-check-source file-name)
  (setq io-prev-l/c-dir/file (cons (file-name-directory file-name)
                                      (file-name-nondirectory file-name)))
  (io-send-string "doFile(\"%s\")" file-name))

;;;###autoload
(defun io-quit-interpreter ()
  "Quit io interpreter."
  (interactive)
  (io-check-interpreter-running)
  (io-send-string "\nexit"))

(provide 'io-mode-inf)

;;; io-mode-inf.el ends here
