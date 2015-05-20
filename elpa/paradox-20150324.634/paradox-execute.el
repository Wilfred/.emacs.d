;;; paradox-execute.el --- executing package transactions -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Prefix: paradox
;; Separator: -

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;


;;; Code:
(require 'dash)
(require 'package)
(require 'paradox-core)
(require 'paradox-github)

(defgroup paradox-execute nil
  "Paradox Packages Menu configurations."
  :prefix "paradox-"
  :package-version '(paradox . "2.0")
  :group 'paradox)


;;; Customization Variables
(defcustom paradox-execute-asynchronously 'ask
  "Whether the install/delete/upgrade should be asynchronous.
Possible values are:
  t, which means always;
  nil, which means never;
  ask, which means ask each time."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "Ask each time" ask))
  :package-version '(paradox . "2.0")
  :group 'paradox-execute)

(defcustom paradox-async-display-buffer-function #'display-buffer
  "Function used to display the *Paradox Report* buffer after an asynchronous upgrade.
Set this to nil to avoid displaying the buffer. Or set this to a
function like `display-buffer' or `pop-to-buffer'.

This is only used if `paradox-menu-execute' was given a non-nil
NOQUERY argument. Otherwise, only a message is displayed."
  :type '(choice (const :tag "Don't display the buffer" nil)
                 function)
  :package-version '(paradox . "2.0")
  :group 'paradox-execute)


;;; Execution Hook
(defvar paradox-after-execute-functions nil
  "List of functions run after performing package transactions.
These are run after a set of installation, deletion, or upgrades
has been performed. Each function in this hook must take a single
argument. An associative list of the form

    ((SYMBOL . DATA) (SYMBOL . DATA) ...)

This list contains the following entries, describing what
occurred during the execution:

  SYMBOL      DATA
  `installed' List of installed packages.
  `deleted'   List of deleted packages.
  `activated' List of activated packages.
  `error'     List of errors.
  `async'     Non-nil if transaction was performed asynchronously.
  `noquery'   The NOQUERY argument given to `paradox-menu-execute'.")
(put 'risky-local-variable-p 'paradox-after-execute-functions t)
(mapc (lambda (x) (add-hook 'paradox-after-execute-functions x t))
      '(paradox--activate-if-asynchronous
        paradox--refresh-package-buffer
        paradox--report-buffer-print
        paradox--report-buffer-display-if-noquery
        paradox--report-message
        ))

(declare-function paradox--generate-menu "paradox-menu")
(defun paradox--refresh-package-buffer (_)
  "Refresh the *Packages* buffer, if it exists."
  (let ((buf (get-buffer "*Packages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if (and (stringp paradox--current-filter)
                 (string-match "Upgrade" paradox--current-filter))
            ;; If this was an Upgrades buffer, go back to full list.
            (package-show-package-list nil nil)
          ;; Otherwise, just refresh whatever is displayed.
          (paradox-menu--refresh nil nil)
          (tabulated-list-print 'remember))))))

(defun paradox--activate-if-asynchronous (alist)
  "Activate packages after an asynchronous operation."
  (let-alist alist
    (when .async
      (mapc #'package-activate-1 .activated))))

(defun paradox--print-package-list (list)
  "Print LIST at point."
  (let* ((width (apply #'max
                  (mapcar (lambda (x) (string-width (symbol-name (package-desc-name x))))
                          list)))
         (tabulated-list-format
          `[("Package" ,(1+ width) nil)
            ("Version" 0 nil)])
         (tabulated-list-padding 2))
    (mapc
     (lambda (p) (tabulated-list-print-entry
             p
             `[,(symbol-name (package-desc-name p))
               ,(mapconcat #'number-to-string (package-desc-version p) ".")]))
     list)))

(defun paradox--report-buffer-print (alist)
  "Print a transaction report in *Package Report* buffer.
Possibly display the buffer or message the user depending on the
situation."
  (let-alist alist
    (let ((buf (get-buffer-create "*Paradox Report*"))
          (inhibit-read-only t))
      (with-current-buffer buf
        (goto-char (point-max))
        ;; TODO: Write our own mode for this.
        (special-mode)
        (insert "\n\n")
        (save-excursion
          (insert (format-time-string "Package transaction finished. %c\n"))
          (when .error
            (insert "Errors:\n  ")
            (dolist (it .error)
              (princ it (current-buffer))
              (insert "\n"))
            (insert "\n\n"))
          (when .installed
            (insert  "Installed:\n")
            (paradox--print-package-list .installed)
            (insert "\n"))
          (when .deleted
            (insert  "Deleted:\n")
            (paradox--print-package-list .deleted)
            (insert "\n")))))))

(defun paradox--report-buffer-display-if-noquery (alist)
  "Display report buffer if `paradox-execute' was called with a NOQUERY prefix.
ALIST describes the transaction.
`paradox-async-display-buffer-function' is used if transaction
was asynchronous. Otherwise, `pop-to-buffer' is used."
  (let-alist alist
    ;; The user has never seen the packages in this transaction. So
    ;; we display them in a buffer.
    (when (or .noquery .error)
      (let ((buf (get-buffer "*Paradox Report*")))
        (when (buffer-live-p buf)
          (cond
           ;; If we're async, the user might be doing something else, so
           ;; we don't steal focus.
           ((and .async paradox-async-display-buffer-function)
            (funcall paradox-async-display-buffer-function buf))
           ;; If we're not async, just go ahead and pop.
           ((or (not .async)
                ;; If there's an error, display the buffer even if
                ;; `paradox-async-display-buffer-function' is nil.
                .error)
            (pop-to-buffer buf))))))))

(defun paradox--report-message (alist)
  "Message the user about the executed transaction.
ALIST describes the transaction."
  (let-alist alist
    (message "%s%s"
      (paradox--format-message nil .installed .deleted)
      (if (memq 'paradox--report-buffer-print paradox-after-execute-functions)
          " See the buffer *Paradox Report* for more details." ""))
    (when .errors
      (message "Errors encountered during the operation: %S\n%s"
        .errors
        (if (memq 'paradox--report-buffer-print paradox-after-execute-functions)
            " See the buffer *Paradox Report* for more details." "")))))


;;; Execution
(defun paradox-menu-execute (&optional noquery)
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed.

Afterwards, if `paradox-automatically-star' is t, automatically
star new packages, and unstar removed packages. Upgraded packages
aren't changed.

Synchronicity of the actions depends on
`paradox-execute-asynchronously'. Optional argument NOQUERY
non-nil means do not ask the user to confirm. If asynchronous,
never ask anyway."
  (interactive "P")
  (unless (derived-mode-p 'paradox-menu-mode)
    (error "The current buffer is not in Paradox Menu mode"))
  (when (and (stringp paradox-github-token)
             (eq paradox-automatically-star 'unconfigured))
    (customize-save-variable
     'paradox-automatically-star
     (y-or-n-p "When you install new packages would you like them to be automatically starred?\n(They will be unstarred when you delete them) ")))
  (paradox--menu-execute-1 noquery))

(defmacro paradox--perform-package-transaction (install delete)
  "Install all packages from INSTALL and delete those from DELETE.
Return an alist with properties listing installed,
deleted, and activated packages, and errors."
  `(let (activated installed deleted errored)
     (advice-add #'package-activate-1 :after
                 (lambda (pkg &rest _)
                   (ignore-errors (add-to-list 'activated pkg 'append)))
                 '((name . paradox--track-activated)))
     (dolist (pkg ,install)
       (condition-case err
           (progn
             ;; 2nd arg introduced in 25.
             (if (version<= "25" emacs-version)
                 (package-install pkg (and (not (package-installed-p pkg))
                                           (package-installed-p
                                            (package-desc-name pkg))))
               (package-install pkg))
             (push pkg installed))
         (error (push err errored))))
     (dolist (pkg ,delete)
       (condition-case err
           (progn (package-delete pkg) (push pkg deleted))
         (error (push err errored))))
     (advice-remove #'package-activate-1 'paradox--track-activated)
     (list (cons 'installed (nreverse installed))
           (cons 'deleted (nreverse deleted))
           ;; Because we used 'append, this is in the right order.
           (cons 'activated activated)
           (cons 'error (nreverse errored)))))

(defvar paradox--current-filter)
(defvar paradox--spinner-stop nil
  "Holds the function that stops the spinner.")

(defun paradox--menu-execute-1 (&optional noquery)
  (let ((before-alist (paradox--repo-alist))
        install-list delete-list)
    (save-excursion
      (goto-char (point-min))
      (let ((p (point))
            (inhibit-read-only t))
        (while (not (eobp))
          (let ((c (char-after)))
            (if (eq c ?\s)
                (forward-line 1)
              (push (tabulated-list-get-id)
                    (cl-case c
                      (?D delete-list)
                      (?I install-list)))
              (delete-region p (point))
              (forward-line 1)
              (setq p (point)))))
        (when (or delete-list install-list)
          (delete-region p (point))
          (ignore-errors
            (set-window-start (selected-window) (point-min))))))
    (if (not (or delete-list install-list))
        (message "No operations specified.")
      ;; Confirm with the user.
      (when (or noquery
                (y-or-n-p (paradox--format-message 'question install-list delete-list)))
        ;; Background or foreground?
        (if (not (cl-case paradox-execute-asynchronously
                   ((nil) nil)
                   ((ask)
                    (if noquery nil
                      (y-or-n-p "Execute in the background? (see `paradox-execute-asynchronously')")))
                   (t t)))
            ;; Synchronous execution
            (progn
              (let ((alist (paradox--perform-package-transaction install-list delete-list)))
                (run-hook-with-args 'paradox-after-execute-functions
                                    `((noquery . ,noquery) (async . nil) ,@alist)))
              (when (and (stringp paradox-github-token) paradox-automatically-star)
                (paradox--post-execute-star-unstar before-alist (paradox--repo-alist))))
          ;; Start spinning
          (setq paradox--spinner-stop (spinner-start 'horizontal-moving))
          ;; Async execution
          (unless (require 'async nil t)
            (error "For asynchronous execution please install the `async' package"))
          ;; We have to do this with eval, because `async-start' is a
          ;; macro and it might not have been defined at compile-time.
          (eval
           `(async-start
             (lambda ()
               (require 'package)
               ,(async-inject-variables "\\`package-")
               (let ((alist ,(macroexpand
                              `(paradox--perform-package-transaction ',install-list ',delete-list))))
                 (list package-alist
                       (when (boundp 'package-selected-packages)
                         package-selected-packages)
                       package-archive-contents
                       ;; This is the alist that will be passed to the hook.
                       (cons '(noquery . ,noquery) (cons '(async . t) alist)))))
             (lambda (x)
               (setq package-alist (pop x)
                     package-selected-packages (pop x)
                     package-archive-contents (pop x))
               (when (functionp paradox--spinner-stop)
                 (funcall paradox--spinner-stop)
                 (setq paradox--spinner-stop nil))
               (run-hook-with-args 'paradox-after-execute-functions (pop x))
               (paradox--post-execute-star-unstar ',before-alist (paradox--repo-alist))))))))))


;;; Aux functions
(defun paradox--repo-alist ()
  "List of known repos."
  (cl-remove-duplicates
   (remove nil
           (--map (cdr-safe (assoc (car it) paradox--package-repo-list))
                  package-alist))))

(defun paradox--format-message (question-p install-list delete-list)
  "Format a message regarding a transaction.
If QUESTION-P is non-nil, format a question suitable for
`y-or-n-p', otherwise format a report in the past sense.
INSTALL-LIST and DELETE-LIST are a list of packages about to be
installed and deleted, respectively."
  (concat
   (when install-list
     (let ((len (length install-list)))
       (format "Install%s %d package%s"
         (if question-p "" "ed")
         len
         (if (> len 1) "s" ""))))
   (when (and install-list (not delete-list))
     (if question-p "? " "."))
   (when (and install-list delete-list)
     ", and ")
   (when delete-list
     (let ((len (length delete-list)))
       (format "Delete%s %d package%s%s"
         (if question-p "" "d")
         len
         (if (> len 1) "s" "")
         (if question-p "? " "."))))))

(defun paradox--post-execute-star-unstar (before after)
  "Star repos in AFTER absent from BEFORE, unstar vice-versa."
  (mapc #'paradox--star-repo
    (-difference (-difference after before) paradox--user-starred-list))
  (mapc #'paradox--unstar-repo
    (-intersection (-difference before after) paradox--user-starred-list)))

(provide 'paradox-execute)
;;; paradox-execute.el ends here.
