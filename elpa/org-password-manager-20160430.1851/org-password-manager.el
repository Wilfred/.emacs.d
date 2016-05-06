;;; org-password-manager.el --- Minimal password manager for Emacs Org Mode.

;; Copyright (C) 2015 - Leandro Facchinetti <me@leafac.com>

;; Author: Leandro Facchinetti <me@leafac.com>
;; Version: 0.0.1
;; Package-Version: 20160430.1851
;; Keywords: password
;; URL: https://git.leafac.com/leafac/org-password-manager
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

;;              ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                          `ORG-PASSWORD-MANAGER'
;;               Minimal password manager for Emacs Org Mode.

;;                           Leandro Facchinetti
;;              ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


;; Table of Contents
;; ─────────────────

;; 1 Philosophy
;; 2 Features
;; 3 Installation
;; 4 Usage
;; .. 4.1 Store passwords in Org Mode files
;; .. 4.2 Get username
;; .. 4.3 Get password
;; .. 4.4 Generate password
;; 5 Configuration
;; 6 Comparison to similar tools

;; Table of Contents
;; ─────────────────

;; 1 Philosophy
;; 2 Features
;; 3 Installation
;; 4 Usage
;; .. 4.1 Store passwords in Org Mode files
;; .. 4.2 Get username
;; .. 4.3 Get password
;; .. 4.4 Generate password
;; 5 Configuration
;; 6 Comparison to similar tools

;; 1 Philosophy
;; ════════════

;;   Simple: To learn and use.

;;   Concise: Don’t recreate features that already exist elsewhere
;;            (e.g. secure password generation).

;;   Secure: Don’t do fancy security measures, they should be handled by
;;           specialized tools (e.g. [GnuPG]).

;;   Flexible: Restrict the users the least possible.


;;   [GnuPG] https://gnupg.org/


;; 2 Features
;; ══════════

;;   1. Use [Org Mode] as password manager.

;;   2. Retrieve passwords in a practical and secure manner.

;;   3. Generate secure passwords.

;;   4. No configuration required.


;;   [Org Mode] http://orgmode.org/


;; 3 Installation
;; ══════════════

;;   Available in [MELPA]. Install with `M-x package-install'.

;;   Password generation depends on [`pwgen']. If you want to use this
;;   feature, install it.

;;   If you want to use the default keybindings described below on the
;;   [Usage] section, add the following line to your Emacs configuration:

;;   ┌────
;;   │ (add-hook 'org-mode-hook 'org-password-manager-key-bindings)
;;   └────

;;   If you want some other keybindings, refer to the body of the function
;;   `org-password-manager-key-bindings' for an example on how to do it.

;;   If you want [`ido'] completion, enable the `org-completion-use-ido'
;;   variable by adding the following line to your Emacs configuration:

;;   ┌────
;;   │ (setq org-completion-use-ido t)
;;   └────


;;   [MELPA] http://melpa.org/#/org-password-manager

;;   [`pwgen'] http://pwgen.sourceforge.net/

;;   [Usage] See section 4

;;   [`ido'] https://www.gnu.org/software/emacs/manual/ido.html


;; 4 Usage
;; ═══════




;; 4.1 Store passwords in Org Mode files
;; ─────────────────────────────────────

;;   Follow the example:

;;   ┌────
;;   │ * [[http://example.com][My favorite website]]
;;   │   :PROPERTIES:
;;   │   :USERNAME: leandro
;;   │   :PASSWORD: chunky-tempeh
;;   │   :END:
;;   |
;;   │ * SSH key
;;   │   :PROPERTIES:
;;   │   :PASSWORD: tofu
;;   │   :END:
;;   └────


;; 4.2 Get username
;; ────────────────

;;   Type `C-c C-p u' (`org-password-manager-get-username') and search for
;;   the title of the entry containing the `USERNAME' property (e.g. “My
;;   favorite website”). The username is copied to the clipboard.

;;   If the point is at an entry that contains the `USERNAME' property, it
;;   is copied without querying for the heading. If you still want to be
;;   queried (because you want the username for a different entry) use the
;;   `C-u' argument typing `C-u C-c C-p u'.


;; 4.3 Get password
;; ────────────────

;;   Type `C-c C-p p' (`org-password-manager-get-password') and search for
;;   the title of the entry containing the `PASSWORD' property (e.g. “My
;;   favorite website”). The password is copied to the clipboard. It tries
;;   to increase the security by skipping the kill ring and copying the
;;   password directly to the system’s clipboard and by erasing it after 30
;;   seconds. This period is customizable, refer to the [Configuration]
;;   section for more.

;;   If the point is at an entry that contains the `PASSWORD' property, it
;;   is copied without querying for the heading. If you still want to be
;;   queried (because you want the password for a different entry) use the
;;   `C-u' argument typing `C-u C-c C-p u'.


;;   [Configuration] See section 5


;; 4.4 Generate password
;; ─────────────────────

;;   Type `C-c C-p g' (`org-password-manager-generate-password') and the
;;   generated password is inserted under the point on the buffer. It is
;;   also copied to your clipboard. It tries to increase the security by
;;   skipping the kill ring and copying the password directly to the
;;   system’s clipboard and by erasing it after 30 seconds. This period is
;;   customizable, refer to the [Configuration] section for more.

;;   If you want to customize the `pwgen' command before running it
;;   (e.g. you want a shorter password), use the `C-u' argument by typing
;;   `C-u C-c C-p g'.


;;   [Configuration] See section 5


;; 5 Configuration
;; ═══════════════

;;   Refer to `M-x customize-group org-password-manager'.


;; 6 Comparison to similar tools
;; ═════════════════════════════

;;   This work was first inspired by [Emacs] and [Org mode], obviously.

;;   But I also want to cite two other projects that are similar in spirit
;;   to `org-password-manager'. They aim to accomplish the same
;;   goal—i.e. using [Emacs] [Org mode] as a password manager. Though they
;;   differ on design from each other and from
;;   `org-password-manager'. Thus, the effort to create
;;   `org-password-manager' is still justified.

;;   Those related projects are both called `org-passwords'. One is by
;;   [Jorge Alfaro-Murillo] and the other by [Andrea Crotti].

;;   [Jorge Alfaro-Murillo's `org-passwords'] has lots of features, way
;;   more than `org-password-manager' plans to have. For example, it
;;   implements its own password generator, requires configuration for
;;   pointing to a password file that should only contain passwords and
;;   opens that file in read-only mode with a timeout. It is so complete
;;   that it is in the official distribution of [Org mode] under
;;   [org-contrib].

;;   `org-password-manager', on the other hand, uses [pwgen] to generate
;;   passwords, handles passwords stored on the middle of any [Org mode]
;;   file with other contents and doesn’t open those files in any special
;;   way.

;;   [Andrea Crotti's `org-passwords'] is more minimal than
;;   `org-password-manager' aims to be. It only retrieves passwords for the
;;   entry under the point, generates passwords by calling [pwgen] and has
;;   almost no documentation, requiring the user to read the source.

;;   I appreciate the mentioned works and thank its authors.


;;   [Emacs] https://www.gnu.org/software/emacs/

;;   [Org mode] http://orgmode.org/

;;   [Jorge Alfaro-Murillo]
;;   https://bitbucket.org/alfaromurillo/org-passwords.el

;;   [Andrea Crotti] https://github.com/AndreaCrotti/org-passwords/

;;   [Jorge Alfaro-Murillo's `org-passwords']
;;   https://bitbucket.org/alfaromurillo/org-passwords.el

;;   [org-contrib]
;;   http://orgmode.org/cgit.cgi/org-mode.git/tree/contrib/lisp/org-passwords.el

;;   [pwgen] http://pwgen.sourceforge.net/

;;   [Andrea Crotti's `org-passwords']
;;   https://github.com/AndreaCrotti/org-passwords/

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
