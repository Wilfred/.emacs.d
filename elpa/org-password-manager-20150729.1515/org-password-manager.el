;;; org-password-manager.el --- Minimal password manager for Emacs Org Mode.

;; Copyright (C) 2015 - Leandro Facchinetti <me@leafac.com>

;; Author: Leandro Facchinetti <me@leafac.com>
;; Version: 0.0.1
;; Package-Version: 20150729.1515
;; Keywords: password
;; URL: https://github.com/leafac/org-password-manager
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

;; Features
;; --------

;; 1. Use Org mode as password manager.
;; 2. Retrieve passwords in a practical and secure manner.
;; 3. Generate secure passwords.
;; 4. No configuration required.

;; Usage
;; -----

;; ### Store passwords in Org mode files

;; Follow the example:

;; ```org-mode
;; * [[http://example.com][My favorite website]]
;;   :PROPERTIES:
;;   :USERNAME: leandro
;;   :PASSWORD: chunky-tempeh
;;   :END:
;; * SSH key
;;   :PROPERTIES:
;;   :PASSWORD: tofu
;;   :END:
;; ```

;; ### Get username

;; Type `C-c C-p u` (`org-password-manager-get-username`) and search for the title
;; of the entry containing the `USERNAME` property (e.g. "My favorite
;; website"). Then it's going to be copied to your clipboard.

;; If the point is at an entry that contains the `USERNAME` property, it's copied
;; without querying you for the heading. If you still want to be queried (because
;; you want the username for a different entry) use the `C-u` argument typing `C-u
;; C-c C-p u`.

;; ### Get password

;; Type `C-c C-p u` (`org-password-manager-get-password`) and search for the title
;; of the entry containing the `PASSWORD` property (e.g. "My favorite
;; website"). Then it's going to be copied to your clipboard. It tries to increase
;; the security by skipping the kill ring and copying the password directly to the
;; system's clipboard and by erasing it after 30 seconds (this period is
;; customizable, refer to the Configuration section on the README).

;; If the point is at an entry that contains the `PASSWORD` property, it's copied
;; without querying you for the heading. If you still want to be queried (because
;; you want the password for a different entry) use the `C-u` argument typing `C-u
;; C-c C-p u`.

;; ### Generate password

;; Type `C-c C-p g` (`org-password-manager-generate-password`) and the generated
;; password will be inserted under the point on the buffer. It's also copied to
;; your clipboard. It tries to increase the security by skipping the kill ring
;; and copying the password directly to the system's clipboard and by erasing it
;; after 30 seconds (this period is customizable, refer to the Configuration
;; section on the README).

;; If you want to customize the `pwgen` command before running it (e.g. you want a
;; shorter password), use the `C-u` argument by typing `C-u C-c C-p g`.

;; Refer to `README.md' at https://github.com/leafac/org-password-manager for
;; more details.

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
