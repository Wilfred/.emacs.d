;;; paradox-github.el --- interacting with the Github API -*- lexical-binding:t -*-

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
(require 'package)
(require 'paradox-core)

(defgroup paradox-github nil
  "Paradox Github configurations."
  :prefix "paradox-"
  :package-version '(paradox . "2.0")
  :group 'paradox)

(defvar paradox--user-starred-list nil)


;;; Github token
(defcustom paradox-github-token nil
  "Access token to use for github actions.
Currently, that means (un)starring repos.

To generate an access token:
  1. Visit the page https://github.com/settings/tokens/new and
     login to github (if asked).
  2. Give the token any name you want (Paradox, for instance).
  3. The only permission we need is \"public_repo\", so unmark
     all others.
  4. Click on \"Generate Token\", copy the generated token, and
     save it to this variable by writing
         (setq paradox-github-token TOKEN)
     somewhere in your configuration and evaluating it (or just
     restart emacs).

This is similar to how erc or jabber handle authentication in
emacs, but the following disclaimer always worth reminding.

DISCLAIMER
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC. This
token grants (very) limited access to your account.
END DISCLAIMER

Paradox will ask you whether you want github integration the
first time you start it. If you answer \"no\", it will remember
your choice via `customize-save-variable'. You can do this
manually by setting this variable to t. Setting it to nil means
it hasn't been configured yet."
  :type '(choice (string :tag "Token")
                 (const :tag "Disable" t)
                 (const :tag "Ask me next time" nil))
  :group 'paradox-github
  :package-version '(paradox . "0.2"))

(defcustom paradox-automatically-star 'unconfigured
  "When you install new packages, should they be automatically starred?
This variable has no effect if `paradox-github-token' isn't set
to a string.

Paradox is capable of automatically starring packages when you
install them, and unstarring when you delete them. This only
applies to actual installation/deletion, i.e. Paradox doesn't
auto (un)star packages that were simply upgraded.

If this variable is nil, this behaviour is disabled. \\<paradox-menu-mode-map>

On the Package Menu, you can always manually star packages with \\[paradox-menu-mark-star-unstar]."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Ask later" unconfigured))
  :group 'paradox-github
  :package-version '(paradox . "0.2"))

(defmacro paradox--enforce-github-token (&rest forms)
  "If a token is defined, perform FORMS, otherwise ignore forms ask for it be defined."
  `(if (stringp paradox-github-token)
       (progn ,@forms)
     (setq paradox-github-token nil)
     (paradox--check-github-token)))

(defun paradox--check-github-token ()
  "Check that the user has either set or refused the github token.
If neither has happened, ask the user now whether he'd like to
configure or refuse the token."
  (if (stringp paradox-github-token) t
    (if paradox-github-token
        t
      (if (not (y-or-n-p "Would you like to set up GitHub integration?
This will allow you to star/unstar packages from the Package Menu. "))
          (customize-save-variable 'paradox-github-token t)
        (describe-variable 'paradox-github-token)
        (when (get-buffer "*Help*")
          (switch-to-buffer "*Help*")
          (delete-other-windows))
        (if (y-or-n-p "Follow the instructions on the `paradox-github-token' variable.
May I take you to the token generation page? ")
            (browse-url "https://github.com/settings/tokens/new"))
        (message "Once you're finished, simply call `paradox-list-packages' again.")
        nil))))


;;; Starring
(defun paradox-star-all-installed-packages ()
  "Star all of your currently installed packages.
No questions asked."
  (interactive)
  (paradox--enforce-github-token
   (mapc (lambda (x) (paradox--star-package-safe (car-safe x))) package-alist)))

(defun paradox--star-package-safe (pkg &optional delete query)
  "Star PKG without throwing errors, unless DELETE is non-nil, then unstar.
If QUERY is non-nil, ask the user first."
  (let ((repo (cdr-safe (assoc pkg paradox--package-repo-list))))
    (when (and repo (not (assoc repo paradox--user-starred-list)))
      (paradox--star-repo repo delete query))))

(defun paradox--star-repo (repo &optional delete query)
  "Star REPO, unless DELETE is non-nil, then unstar.
If QUERY is non-nil, ask the user first.

Throws error if repo is malformed."
  (when (or (not query)
            (y-or-n-p (format "Really %sstar %s? "
                        (if delete "un" "") repo)))
    (paradox--github-action-star repo delete)
    (message "%starred %s." (if delete "Uns" "S") repo)
    (if delete
        (setq paradox--user-starred-list
              (remove repo paradox--user-starred-list))
      (add-to-list 'paradox--user-starred-list repo))))

(defun paradox--unstar-repo (repo &optional delete query)
  "Unstar REPO.
Calls (paradox--star-repo REPO (not DELETE) QUERY)."
  (paradox--star-repo repo (not delete) query))

(defun paradox--refresh-user-starred-list ()
  "Fetch the user's list of starred repos."
  (setq paradox--user-starred-list
        (ignore-errors
          (paradox--github-action
           "user/starred?per_page=100" nil
           'paradox--full-name-reader))))

(defun paradox--github-action-star (repo &optional delete no-result)
  "Call `paradox--github-action' with \"user/starred/REPO\" as the action.
DELETE and NO-RESULT are passed on."
  (paradox--github-action (concat "user/starred/" repo)
                          (if (stringp delete) delete (if delete "DELETE" "PUT"))
                          (null no-result)))


;;; The Base (generic) function
(defun paradox--github-action (action &optional method reader max-pages)
  "Contact the github api performing ACTION with METHOD.
Default METHOD is \"GET\".

Action can be anything such as \"user/starred?per_page=100\". If
it's not a full url, it will be prepended with
\"https://api.github.com/\".

The api action might not work if `paradox-github-token' isn't
set. This function also handles the pagination used in github
results, results of each page are appended. Use MAX-PAGES to
limit the number of pages that are fetched.

Return value is always a list.
- If READER is nil, the result of the action is completely
  ignored (no pagination is performed on this case, making it
  much faster).
- Otherwise, READER is called as a function with point right
  after the headers and should always return a list. As a special
  exception, if READER is t, it is equivalent to a function that
  returns (t)."
  ;; Make sure the token's configured.
  (unless (string-match "\\`https?://" action)
    (setq action (concat "https://api.github.com/" action)))
  ;; Make the request
  (let (next)
    (append
     (with-temp-buffer
       (save-excursion
         (shell-command
          (if (stringp paradox-github-token)
              (format "curl -s -i -d \"\" -X %s -u %s:x-oauth-basic \"%s\" "
                (or method "GET") paradox-github-token action)

            (format "curl -s -i -d \"\" -X %s \"%s\" "
              (or method "GET") action)) t))
       (when reader
         (unless (search-forward " " nil t)
           (error "Invalid request:\n%s"
             (buffer-substring-no-properties (point-min) (point-max))))
         (cl-case (thing-at-point 'number)
           (204 '(t)) ;; OK, but no content.
           (404 nil) ;; Not found.
           (200 ;; Good.
            (when (search-forward-regexp
                   "^Link: .*<\\([^>]+\\)>; rel=\"next\"" nil t)
              (setq next (match-string-no-properties 1)))
            (search-forward-regexp "^?$")
            (skip-chars-forward "[:blank:]\n")
            (delete-region (point-min) (point))
            (unless (eobp) (if (eq reader t) t (funcall reader))))
           (t (error "Github returned: %s" (thing-at-point 'line))))))
     (unless (or (not next) (and max-pages (< max-pages 2)))
       (paradox--github-action
        next method reader
        (when max-pages (1- max-pages)))))))

(provide 'paradox-github)
;;; paradox-github.el ends here.
