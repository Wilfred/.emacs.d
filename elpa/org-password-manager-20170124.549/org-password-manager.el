;;; org-password-manager.el --- Password manager for Org Mode.

;; Copyright (C) 2015 - Leandro Facchinetti <me@leafac.com>

;; Author: Leandro Facchinetti <me@leafac.com>
;; Version: 0.0.1
;; Package-Version: 20170124.549
;; Keywords: password
;; URL: https://git.leafac.com/org-password-manager
;; Package-Requires: ((org "8.2.10") (s "1.9.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Password Manager

;; Leandro Facchinetti <me@leafac.com>

;; Password manager for Org Mode.

;; Version         0.0.1                                               
;; --------------------------------------------------------------------
;; Documentation   https://www.leafac.com/software/org-password-manager
;; --------------------------------------------------------------------
;; License         GNU General Public License Version 3                
;; --------------------------------------------------------------------
;; Code of Conduct Contributor Covenant v1.4.0                         
;; --------------------------------------------------------------------
;; Distribution    MELPA                                               
;; --------------------------------------------------------------------
;; Source          https://git.leafac.com/org-password-manager         
;; --------------------------------------------------------------------
;; Bug Reports     Write emails to org-password-manager@leafac.com.    
;; --------------------------------------------------------------------
;; Contributions   Send patches and pull requests via email to         
;;                 org-password-manager@leafac.com.                    
;; --------------------------------------------------------------------

;; 1. Overview

;; Use GnuPG to encrypt the Org Mode files that contains credentials
;; instead of storing sensitive information in plain text.

;; Use Org Mode files to store credentials and retrieve them securely.
;; Integrate with pwgen to generate passwords.

;; 2. Installation

;; Available from MELPA, add the repository to Emacs and install with
;; M-x package-install. Password creation requires pwgen.

;; 3. Usage

;; This section assumes the default configuration.

;; Add credentials as properties named USERNAME and PASSWORD to headings in
;; Org Mode files. For example:

;;   * [[http://example.com][My favorite website]]
;;     :PROPERTIES:                               
;;     :USERNAME: leandro                         
;;     :PASSWORD: chunky-tempeh                   
;;     :END:                                      
;;                                                
;;   * SSH key                                    
;;     :PROPERTIES:                               
;;     :PASSWORD: tofu                            
;;     :END:                                      

;; Passwords are cleared from the clipboard after 30 seconds.

;; Retrieve usernames with C-c C-p u (org-password-manager-get-username)
;; and passwords with C-c C-p p (org-password-manager-get-password). If
;; point is not under a heading that contains credentials, Org Password
;; Manager asks for a heading. To force this behavior even when the point
;; is under a heading that contains credentials, use the C-u argument (for
;; example, C-u C-c C-p u).

;; Generate passwords with C-c C-p g
;; (org-password-manager-generate-password). To customize the parameters to
;; pwgen, use the C-u argument (C-u C-c C-p g).

;; 4. Configuration

;; For the default configuration with the keybindings covered in the Usage
;; section, add the following to the Emacs configuration:

;;   (add-hook 'org-mode-hook 'org-password-manager-key-bindings)

;; To customize the key bindings, start with the following code:

;;   (defun org-password-manager-key-bindings ()                           
;;     "Binds keys for org-password-manager."                              
;;     (local-set-key (kbd "C-c C-p u") 'org-password-manager-get-username)
;;     (local-set-key (kbd "C-c C-p p") 'org-password-manager-get-password)
;;     (local-set-key (kbd "C-c C-p                                        
;;   g") 'org-password-manager-generate-password))                         

;; For Interactive Do (ido) support, add the following to the Emacs
;; configuration:

;;   (setq org-completion-use-ido t)

;; For advanced configuration, refer to
;; M-x customize-group org-password-manager.

;; 5. Changelog

;; This section documents all notable changes to Org Password Manager. It
;; follows recommendations from Keep a CHANGELOG and uses Semantic
;; Versioning. Each released version is a Git tag.

;; 5.1. 0.0.1 · 2015-07-29

;; 5.1.1. Added

;; * Core functionality.

;;; Code:

(require 'org)
(require 's)

(defgroup org-password-manager nil
  "Minimal password manager for Emacs Org Mode."
  :group 'org)

(defcustom org-password-manager-default-pwgen-command "pwgen --secure --symbols --capitalize --numerals 25 1"
  "The default `pwgen' command to use when generating passwords."
  :group 'org-password-manager)

(defcustom org-password-manager-default-password-wait-time "30 sec"
  "The default period to wait before erasing the password from the clipboard.

Must be compatible with `run-at-time'."
  :group 'org-password-manager)

(defcustom org-password-manager-scope 'agenda
  "The scope used to search in org buffers.

Supported scopes are defined in `org-map-entries'. By default,
the `agenda' scope searches through all agenda files."
  :group 'org-password-manager)

(defvar org-password-manager-history ()
  "The history of headings that were chosen for `org-password-manager'.")

(defun org-password-manager-get-property (property-name &optional ask-for-input?)
  "Get PROPERTY-NAME.

If ASK-FOR-INPUT? is t, will ask for input even if point is on a
heading that contains the property."
  (let ((display-property-name (capitalize property-name))
        (property (org-entry-get (point) property-name t))
        output-message heading)
    (if (and property (not ask-for-input?))
        (setq heading (org-link-display-format (org-get-heading t t)))
      (let* ((property-entries
              (org-map-entries
               (lambda ()
                 (list
                  (org-link-display-format (org-get-heading t t))
                  (org-entry-get (point) property-name)))
               (concat property-name "={.+}") org-password-manager-scope))
             (chosen-heading (funcall 'org-completing-read
                                      (concat display-property-name " for: ")
                                      property-entries
                                      nil
                                      nil
                                      nil
                                      'org-password-manager-history
                                      (car org-password-manager-history)))
             (header-property-list (assoc chosen-heading property-entries)))
        (if header-property-list
            (setq heading (nth 0 header-property-list)
                  property (nth 1 header-property-list))
          (setq output-message (concat display-property-name
                                       " for `"
                                       chosen-heading
                                       "' not found!")))))
    (if (and heading property)
        (if (string= property-name "PASSWORD")
            (progn
              (funcall interprogram-cut-function property)
              (run-at-time org-password-manager-default-password-wait-time nil (lambda () (funcall interprogram-cut-function "")))
              (setq output-message
                    (concat display-property-name " for `" heading "' securely copied to system's clipboard avoiding kill ring and will be removed in " org-password-manager-default-password-wait-time ".")))
          (progn (kill-new property)
                 (setq output-message
                       (concat display-property-name " for `" heading "' copied to clipboard."))))
      (add-to-history 'org-password-manager-history heading))
    (message output-message)))

;;;###autoload
(defun org-password-manager-get-username (&optional ask-for-input?)
  "Get username.

If ASK-FOR-INPUT? is t, will ask for input even if point is on a
heading that contains the username property."
  (interactive "P")
  (org-password-manager-get-property "USERNAME" ask-for-input?))

;;;###autoload
(defun org-password-manager-get-password (&optional ask-for-input?)
  "Get password.

If ASK-FOR-INPUT? is t, will ask for input even if point is on a
heading that contains the password property."
  (interactive "P")
  (org-password-manager-get-property "PASSWORD" ask-for-input?))

;;;###autoload
(defun org-password-manager-generate-password (&optional edit-pwgen-string?)
  "Generate password.

If EDIT-PWGEN-STRING? is t, let the user edit the pwgen command
line before running it."
  (interactive "P")
  (let* ((pwgen-string (if edit-pwgen-string?
                           (read-from-minibuffer "pwgen command to run: " org-password-manager-default-pwgen-command)
                         org-password-manager-default-pwgen-command))
         (generated-password (s-trim (shell-command-to-string pwgen-string))))
    (insert generated-password)
    (funcall interprogram-cut-function generated-password)
    (run-at-time org-password-manager-default-password-wait-time nil (lambda () (funcall interprogram-cut-function "")))
    (message (concat "Generated password inserted on buffer, securely copied to system's clipboard avoiding kill ring and will be removed in " org-password-manager-default-password-wait-time "."))))

;; Key bindings.

;;;###autoload
(defun org-password-manager-key-bindings ()
  "Binds keys for org-password-manager."
  (local-set-key (kbd "C-c C-p u") 'org-password-manager-get-username)
  (local-set-key (kbd "C-c C-p p") 'org-password-manager-get-password)
  (local-set-key (kbd "C-c C-p g") 'org-password-manager-generate-password))

(provide 'org-password-manager)

;;; org-password-manager.el ends here
