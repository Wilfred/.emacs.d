;;; gae-utils.el --- deploying to Google App Engine from within Emacs

;; Copyright (C) 2011 Wilfred Hughes <me@wilfred.me.uk>

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 12 July 2011
;; Version: 0.2
;; Keywords: google, app engine, deploy

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary

;; If you get the error "The Google App Engine SDK could not be
;; found!", your path is set up incorrectly. In a shell, do:

;; $ which appcfg.py
;; /home/wilfred/bin/google_appengine/appcfg.py

;; then append this to your Emacs PATH by doing:

;; (setenv "PATH" "$PATH:/home/wilfred/bin/google_appengine" t)

(defvar gae-email-address "" "Email addressed use to authenticate with App Engine.")
(defvar gae-deploy-buffer-name "*GAE-deploy*")

; TODO: handle the password being incorrect
; TODO: handle multiple accounts

(defun gae-run-deploy-command (command command-args interactive-args)
  "Run COMMAND with arguments COMMAND-ARGS, passing
INTERACTIVE-ARGS in order interactively."
  ; kill old deploy buffer
  (when (get-buffer gae-deploy-buffer-name)
    (kill-buffer gae-deploy-buffer-name))

  (let ((process (apply 'start-process "gae_deploy" gae-deploy-buffer-name
                        command command-args)))

    (switch-to-buffer gae-deploy-buffer-name)

    (dolist (arg interactive-args)
      (sleep-for 1)
      ; TODO: check whether all these \n characters are necessary
      (process-send-string process (concat "\n" arg "\n")))))

(process-get) (get-buffer gae-deploy-buffer-name)

(defun gae-deploy-application (email)
  "Deploy the current GAE application."
  (interactive (list (read-from-minibuffer "GAE email: " gae-email-address)))
  (setq gae-email-address email)

  (let ((password (read-passwd "Password: "))
        (project-root (expand-file-name (vc-git-root (buffer-file-name)))))
    (gae-run-deploy-command "appcfg.py" (list "update" project-root "-e" email) (list password))))

(defun gae-deploy-backend (email)
  "Deploy all the backends of the current application."
  (interactive (list (read-from-minibuffer "GAE email: " gae-email-address)))
  (setq gae-email-address email)

  (let ((password (read-passwd "Password: "))
        (project-root (expand-file-name (vc-git-root (buffer-file-name)))))
    (gae-run-deploy-command "appcfg.py" (list "backends" "update" project-root "-e" email) (list password))))

(defun gae-deploy-django (email)
  "Run manage.py deploy in DIR, using credentials EMAIL and PASSWORD."
  (interactive (list (read-from-minibuffer "GAE email: " gae-email-address)))
  (setq gae-email-address email)

  (let* ((project-root (expand-file-name (vc-git-root (buffer-file-name))))
         (manage-py-path (concat project-root "manage.py"))
         (password (read-passwd "Password: ")))

    (gae-run-deploy-command "python2.5" (list manage-py-path "deploy") (list email password))))

(provide 'gae-utils)