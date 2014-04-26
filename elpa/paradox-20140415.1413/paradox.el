;;; paradox.el --- Display Package Ratings on the *Packages* buffer.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/paradox
;; Version: 0.9.4
;; Keywords: package packages mode-line
;; Package-Requires: ((emacs "24.1") (tabulated-list "1.0") (package "1.0") (dash "2.6.0") (cl-lib "1.0"))
;; Prefix: paradox 
;; Separator: -

;;; Commentary:
;; 
;; Project for modernizing Emacs' Package Menu. With package ratings,
;; usage statistics, customizability, and more.
;; 
;; Usage
;; ===
;; 
;; To install it, call M-x `package-install' RET paradox.
;; 
;; To use it, simply call M-x `paradox-list-packages' (instead of the
;; regular `list-packages').  
;; This will give you most features out of the box. If you want to be
;; able to star packages as well, just configure the
;; `paradox-github-token' variable then call `paradox-list-packages'
;; again.
;; 
;; If you'd like to stop using Paradox, you may call `paradox-disable'
;; and go back to using the regular `list-packages'.
;; 
;; ## Current Features ##
;; 
;; ### Package Ratings ###
;; 
;; The first feature you should know about is our integration with
;; **GitHub Stars**, which works as *rough package rating* system.  
;; That is, Paradox package menu will:
;; 
;; 1. Display the number of GitHub Stars each package has (assuming it's
;;    in a github repo, of course);
;; 2. Automatically star packages you install, and unstar packages you delete;
;; 3. Let you star and unstar packages by hitting the `s' key;
;; 4. Let you star all packages you have installed with M-x `paradox-star-all-installed-packages'.
;; 
;; Item **1.** will work out of the box, the other items obviously
;; require a github account (Paradox will help you generate a token the
;; first time you call `paradox-list-packages').
;;   
;; ### Several Improvements ###
;; 
;; Other features include many small improvements to the package menu
;; itself and also work out of the box.
;; 
;; * Shortcuts for package filtering:
;;     * <f r> filters by regexp (`occur');
;;     * <f u> display only packages with upgrades;
;;     * <f k> filters by keyword (emacs 24.4 only).
;; * `hl-line-mode' enabled by default.
;; * Display useful information on the mode-line and cleanup a bunch of
;;   useless stuff (customizable).
;; * Customization! Just call M-x `paradox-customize' to see what you can
;;   do.
;;     * Customize column widths.
;;     * Customize faces (`paradox-star-face',
;;       `paradox-status-face-alist' and `paradox-archive-face').
;;     * Customize local variables.

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

;;; Change Log:
;; 0.9.2 - 2014/04/15 - Fix advice being enabled automatically.
;; 0.9.2 - 2014/04/15 - Ask the user before automatically starring.
;; 0.9.1 - 2014/04/14 - paradox-filter-upgrades is informative when there are no upgrades.
;; 0.9   - 2014/04/14 - First full feature release.
;; 0.5   - 2014/04/14 - Star all installed packages.
;; 0.5   - 2014/04/13 - (Un)Star packages with the "s" key!.
;; 0.2   - 2014/04/13 - Control the face used for each status with paradox-status-face-alist.
;; 0.2   - 2014/04/13 - New archive face.
;; 0.2   - 2014/04/13 - Define filtering keys (fk, fu, fr).
;; 0.2   - 2014/04/11 - Hide buffer-name with paradox-display-buffer-name.
;; 0.2   - 2014/04/08 - Even better mode-line.
;; 0.2   - 2014/04/08 - Intelligent width for the "archive" column.
;; 0.2   - 2014/04/08 - Customizable widths.
;; 0.2   - 2014/04/08 - Prettier trunctation.
;; 0.1   - 2014/04/03 - Created File.
;;; Code:

(require 'package)
(require 'cl-lib)
(require 'dash)
(defconst paradox-version "0.9.4" "Version of the paradox.el package.")
(defun paradox-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and paradox versions."
  (interactive)
  (message "Your paradox-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           paradox-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/paradox/issues/new"))
(defun paradox-customize ()
  "Open the customization menu in the `paradox' group."
  (interactive)
  (customize-group 'paradox t))
(defgroup paradox nil
  "Customization group for paradox."
  :prefix "paradox-"
  :group 'emacs
  :package-version '(paradox . "0.1"))

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

DISCLAIMER:
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC. This
token grants (very) limited access to your account."
  :type 'string
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defcustom paradox-automatically-star 'unconfigured
  "When you install new packages, should they be automatically starred? 
NOTE: This variable has no effect if `paradox-github-token' isn't set.

Paradox is capable of automatically starring packages when you
install them, and unstarring when you delete them. This only
applies to actual installation/deletion, i.e. Paradox doesn't
auto (un)star packages that were simply upgraded.

If this variable is nil, this behaviour is disabled. \\<paradox-menu-mode-map>

On the Package Menu, you can always manually star packages with \\[paradox-menu-mark-star-unstar]."
  :type '(choice (const :tag "Yes." t)
                 (const :tag "No." nil)
                 (const :tag "Ask later." unconfigured))
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defface paradox-name-face
  '((t :inherit link))
  "Face used on the name column."
  :group 'paradox)
;; (defface paradox-version-face
;;   '((t :inherit default))
;;   "Face used on the version column."
;;   :group 'paradox)
(defface paradox-archive-face
  '((((background light)) :foreground "Grey60")
    (((background dark)) :foreground "Grey40"))
  "Face used on the archive column."
  :group 'paradox)
(defface paradox-star-face
  '((t :inherit font-lock-string-face))
  "Face used on the star column, for packages you haven't starred."
  :group 'paradox)
(defface paradox-starred-face
  '((t :weight bold :inherit paradox-star-face))
  "Face used on the star column, for packages you have starred."
  :group 'paradox)
;; (defface paradox-description-face
;;   '((t :inherit default))
;;   "Face used on the description column."
;;   :group 'paradox)

(defvar paradox--star-count nil)
(defvar paradox--package-repo-list nil)

(defvar paradox--star-count-url
  "https://raw.github.com/Bruce-Connor/paradox/data/data"
  "Address of the raw star-count file.")

(defvar paradox--package-count
  '(("total" . 0) ("built-in" . 0)
    ("obsolete" . 0)
    ("available" . 0) ("new" . 0)
    ("held" . 0) ("disabled" . 0)
    ("installed" . 0) ("unsigned" . 0)))

(defmacro paradox--cas (string)
  `(cdr (assoc-string ,string paradox--package-count)))

;;;###autoload
(defun paradox--refresh-star-count ()
  "Download the star-count file and populate the respective variable."
  (interactive)
  (with-current-buffer 
      (url-retrieve-synchronously paradox--star-count-url)
    (when (search-forward "\n\n") 
      (setq paradox--star-count (read (current-buffer)))
      (setq paradox--package-repo-list (read (current-buffer))))
    (kill-buffer))
  (when (stringp paradox-github-token)
    (paradox--refresh-user-starred-list)))

(defcustom paradox-hide-buffer-identification t
  "If non-nil, no buffer-name will be displayed in the packages buffer."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "0.5"))
(defvaralias 'paradox-hide-buffer-name 'paradox-hide-buffer-identification)

(defun paradox--build-buffer-id (st n)
  (list st (list :propertize (int-to-string n)
                 'face 'mode-line-buffer-id)))

;;;###autoload
(defun paradox-list-packages (no-fetch)
  "Improved version of `package-list-packages'.
Shows star count for packages, and extra information in the
mode-line."
  (interactive "P")
  (when (paradox--check-github-token)
    (paradox-enable)
    (unless no-fetch (paradox--refresh-star-count))
    (package-list-packages no-fetch)))

(defun paradox-enable ()
  "Enable paradox, overriding the default package-menu."
  (interactive)
  (ad-activate 'package-menu-execute)
  (if (version< emacs-version "24.3.50")
      (progn
        (require 'paradox-compat)
        (paradox--override-definition 'package-menu--print-info 'paradox--print-info-compat))
    (paradox--override-definition 'package-menu--print-info 'paradox--print-info))
  (paradox--override-definition 'package-menu--generate 'paradox--generate-menu)
  (paradox--override-definition 'truncate-string-to-width 'paradox--truncate-string-to-width)
  (paradox--override-definition 'package-menu-mode 'paradox-menu-mode))

(defvar paradox--backups nil)

(defun paradox-disable ()
  "Disable paradox, and go back to regular package-menu."
  (interactive)
  (ad-deactivate 'package-menu-execute)
  (dolist (it paradox--backups)
    (message "Restoring %s to %s" (car it) (eval (cdr it)))
    (fset (car it) (eval (cdr it))))
  (setq paradox--backups nil))

(defun paradox--override-definition (sym newdef)
  "Temporarily override SYM's function definition with NEWDEF.
The original definition is saved to paradox--SYM-backup."
  (let ((backup-name (intern (format "paradox--%s-backup" sym)))
        (def (symbol-function sym)))
    (unless (assoc sym paradox--backups)
      (message "Overriding %s with %s" sym newdef)
      (eval (list 'defvar backup-name nil))
      (add-to-list 'paradox--backups (cons sym backup-name))
      (set backup-name def)
      (fset sym newdef))))

;;; Right now this is trivial, but we leave it as function so it's easy to improve.
(defun paradox--active-p ()
  (null (null paradox--backups)))

(defun paradox--truncate-string-to-width (&rest args)
  "Like `truncate-string-to-width', except default ellipsis is \"…\" on package buffer."
  (when (and (eq major-mode 'paradox-menu-mode)
             (eq t (nth 4 args)))
    (setf (nth 4 args) (if (char-displayable-p ?…) "…" "$")))
  (apply paradox--truncate-string-to-width-backup args))

(defvar paradox--upgradeable-packages nil)
(defvar paradox--upgradeable-packages-number nil)
(defvar paradox--upgradeable-packages-any? nil)

(defun paradox-refresh-upgradeable-packages ()
  "Refresh the list of upgradeable packages."
  (interactive)
  (setq paradox--upgradeable-packages (package-menu--find-upgrades))
  (setq paradox--upgradeable-packages-number
        (length paradox--upgradeable-packages))
  (setq paradox--upgradeable-packages-any?
        (> paradox--upgradeable-packages-number 0)))

(defcustom paradox-status-face-alist
  '(("built-in"  . font-lock-builtin-face)
    ("available" . default)
    ("new"       . bold)
    ("held"      . font-lock-constant-face)
    ("disabled"  . font-lock-warning-face)
    ("installed" . font-lock-comment-face)
    ("unsigned"  . font-lock-warning-face))
  "List of (\"STATUS\" . FACE) cons cells.
When displaying the package menu, FACE will be used to paint the
Version, Status, and Description columns of each package whose
status is STATUS. "
  :type '(repeat (cons string face))
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defun paradox--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [STAR NAME VERSION STATUS DOC])."
  (let* ((pkg-desc (car pkg))
         (status  (cdr pkg))
         (face (or (cdr (assoc-string status paradox-status-face-alist))
                   'font-lock-warning-face))) ; obsolete.
    (paradox--incf status)
    (list pkg-desc
          `[,(list (symbol-name (package-desc-name pkg-desc))
                   'face 'paradox-name-face
                   'follow-link t
                   'package-desc pkg-desc
                   'action 'package-menu-describe-package)
            ,(propertize (package-version-join
                          (package-desc-version pkg-desc))
                         'font-lock-face face)
            ,(propertize status 'font-lock-face face)
            ,@(if (cdr package-archives)
                  (list (propertize (or (package-desc-archive pkg-desc) "")
                                    'font-lock-face 'paradox-archive-face)))
            ,(paradox--package-star-count (package-desc-name pkg-desc))
            ,(propertize (package-desc-summary pkg-desc)
                         'font-lock-face face)])))

(defun paradox--incf (status)
  (cl-incf (paradox--cas status))
  (unless (string= status "obsolete")
    (cl-incf (paradox--cas "total"))))

(defun paradox--improve-entry (entry)
  (setcdr entry (list 
                 (vconcat (list (paradox--entry-star-count entry))
                          (cadr entry)))))

(defun paradox--entry-star-count (entry)
  (paradox--package-star-count
   ;; The package symbol should be in the ID field, but that's not mandatory,
   (or (ignore-errors (elt (car entry) 1))
       ;; So we also try interning the package name.
       (intern (car (elt (cadr entry) 0))))))

(defvar paradox--user-starred-list nil)

(defun paradox--package-star-count (package)
  (let ((count (cdr (assoc package paradox--star-count)))
        (repo (cdr-safe (assoc package paradox--package-repo-list))))
    (propertize  
     (format "%s" (or count ""))
     'face
     (if (and repo (assoc-string repo paradox--user-starred-list))
         'paradox-starred-face
       'paradox-star-face))))

(defvar paradox--column-index-star nil)

(defun paradox--star-predicate (A B)
  (< (string-to-number (elt (cadr A) paradox--column-index-star))
     (string-to-number (elt (cadr B) paradox--column-index-star))))

(defvar paradox--current-filter nil)
(make-variable-buffer-local 'paradox--current-filter)

(defun paradox--generate-menu (remember-pos packages &optional keywords)
  "Populate the Package Menu, without hacking into the header-format.
If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (mapc (lambda (x) (setf (cdr x) 0)) paradox--package-count)
  (paradox-menu--refresh packages keywords)
  (setq paradox--current-filter
        (if keywords (mapconcat 'identity keywords ",")
          nil))
  (let ((idx (paradox--column-index "Package")))
    (setcar (aref tabulated-list-format idx)
            (if keywords
                (concat "Package[" paradox--current-filter "]")
              "Package")))  
  (if keywords
      (define-key package-menu-mode-map "q" 'package-show-package-list)
    (define-key package-menu-mode-map "q" 'quit-window))
  (tabulated-list-print remember-pos)
  (tabulated-list-init-header)
  (paradox--update-mode-line)
  (paradox-refresh-upgradeable-packages))

(if (version< emacs-version "24.3.50")
    (require 'paradox-compat)
  (defalias 'paradox-menu--refresh 'package-menu--refresh))

(defun paradox--column-index (regexp)
  (cl-position (format "\\`%s\\'" (regexp-quote regexp)) tabulated-list-format
            :test (lambda (x y) (string-match x (or (car-safe y) "")))))

(defvar paradox-menu-mode-map package-menu-mode-map)
(define-prefix-command 'paradox--filter-map)
(define-key paradox-menu-mode-map "f" 'paradox--filter-map)
(define-key paradox-menu-mode-map "s" 'paradox-menu-mark-star-unstar)

(define-key package-menu-mode-map "F" 'package-menu-filter)
(define-key paradox--filter-map "k" #'package-menu-filter)
(define-key paradox--filter-map "f" #'package-menu-filter)
(define-key paradox--filter-map "r" #'occur)
(define-key paradox--filter-map "o" #'occur)
(define-key paradox--filter-map "u" #'paradox-filter-upgrades)

(defun paradox-filter-upgrades ()
  "Show only upgradable packages."
  (interactive)
  (if (null paradox--upgradeable-packages)
      (message "No packages have upgrades.")
    (package-show-package-list
     (mapcar 'car paradox--upgradeable-packages))
    (if keywords
        (define-key package-menu-mode-map "q" 'package-show-package-list)
      (define-key package-menu-mode-map "q" 'quit-window))
    (paradox--add-filter "Upgrade")))

(defun paradox--add-filter (keyword)
  "Append KEYWORD to `paradox--current-filter', and rebind \"q\"."
  ;; (unless (= 0 (length paradox--current-filter))
  ;;   (setq paradox--current-filter 
  ;;         (concat paradox--current-filter ",")))
  (setq paradox--current-filter keyword)
  (define-key package-menu-mode-map "q" 'quit-window))

(defcustom paradox-column-width-package  18
  "Width of the \"Package\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-version  9
  "Width of the \"Version\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-status  10
  "Width of the \"Status\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-star 4
  "Width of the \"Star\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defvar paradox--column-name-star
  (if (char-displayable-p ?★) "★" "*"))

(define-derived-mode paradox-menu-mode tabulated-list-mode "Paradox Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<paradox-menu-mode-map>
\\{paradox-menu-mode-map}"
  (hl-line-mode 1)  
  (paradox--update-mode-line)
  (setq tabulated-list-format
        `[("Package" ,paradox-column-width-package package-menu--name-predicate)
          ("Version" ,paradox-column-width-version nil)
          ("Status" ,paradox-column-width-status package-menu--status-predicate)
          ,@(paradox--archive-format)
          (,paradox--column-name-star ,paradox-column-width-star paradox--star-predicate :right-align t)
          ("Description" 0 nil)])
  (setq paradox--column-index-star 
        (paradox--column-index paradox--column-name-star))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  ;; (add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
  (add-hook 'tabulated-list-revert-hook 'paradox-refresh-upgradeable-packages nil t)
  (add-hook 'tabulated-list-revert-hook 'paradox--refresh-star-count nil t)
  (add-hook 'tabulated-list-revert-hook 'paradox--update-mode-line nil t)
  (tabulated-list-init-header)
  ;; We need package-menu-mode to be our parent, otherwise some
  ;; commands throw errors. But we can't actually derive from it,
  ;; otherwise its initialization will screw up the header-format. So
  ;; we "patch" it like this.
  (put 'paradox-menu-mode 'derived-mode-parent 'package-menu-mode)
  (run-hooks 'package-menu-mode-hook))

(defun paradox--archive-format ()
  (when (and (cdr package-archives) 
             (null (version< emacs-version "24.3.50")))
    (list (list "Archive" 
                (apply 'max (mapcar 'length (mapcar 'car package-archives)))
                'package-menu--archive-predicate))))

(add-hook 'paradox-menu-mode-hook 'paradox-refresh-upgradeable-packages)

(defcustom paradox-local-variables
  '(mode-line-mule-info
    mode-line-client mode-line-modified
    mode-line-remote mode-line-position
    column-number-mode size-indication-mode
    (mode-line-front-space . " "))
  "Variables which will take special values on the Packages buffer.
This is a list, where each element is either SYMBOL or (SYMBOL . VALUE).

Each SYMBOL (if it is bound) will be locally set to VALUE (or
nil) on the Packages buffer."
  :type '(repeat (choice symbol (cons symbol sexp)))
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom paradox-display-buffer-name nil
  "If nil, *Packages* buffer name won't be displayed in the mode-line."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defun paradox--update-mode-line ()
  (mapc #'paradox--set-local-value paradox-local-variables)
  (setq mode-line-buffer-identification
        (list
         `(line-number-mode
           ("(" (:propertize "%4l" face mode-line-buffer-id) "/"
            ,(int-to-string (line-number-at-pos (point-max))) ")"))
         (list 'paradox-display-buffer-name
               (propertized-buffer-identification
                (format "%%%sb" (length (buffer-name)))))
         '(paradox--current-filter ("[" paradox--current-filter "]"))
         '(paradox--upgradeable-packages-any?
           (" " (:eval (paradox--build-buffer-id "Upgrade:" paradox--upgradeable-packages-number))))         
         '(package-menu--new-package-list
           (" " (:eval (paradox--build-buffer-id "New:" (paradox--cas "new")))))
         " " (paradox--build-buffer-id "Installed:" (+ (paradox--cas "installed") (paradox--cas "unsigned")))
         `(paradox--current-filter
           "" (" " ,(paradox--build-buffer-id "Total:" (length package-archive-contents)))))))

(defun paradox--set-local-value (x)
  (let ((sym (or (car-safe x) x)))
    (when (boundp sym)
      (set (make-local-variable sym) (cdr-safe x)))))

(defadvice package-menu-execute 
    (around paradox-around-package-menu-execute-advice ())
  "Star/Unstar packages which were installed/deleted during `package-menu-execute'."
  (when (and (stringp paradox-github-token)
             (eq paradox-automatically-star 'unconfigured))
    (customize-save-variable
     'paradox-automatically-star
     (y-or-n-p "When you install new packages would you like them to be automatically starred?\n(They will be unstarred when you delete them) ")))
  (if (and (stringp paradox-github-token) paradox-automatically-star)
      (let ((before (paradox--repo-alist)) after)
        ad-do-it
        (setq after (paradox--repo-alist))
        (mapc #'paradox--star-repo
              (-difference (-difference after before) paradox--user-starred-list))
        (mapc #'paradox--unstar-repo
              (-intersection (-difference before after) paradox--user-starred-list))
        (package-menu--generate t t))
    ad-do-it))

(defun paradox--repo-alist ()
  (cl-remove-duplicates
   (remove nil 
           (--map (cdr-safe (assoc (car it) paradox--package-repo-list)) 
                  package-alist))))


;;; Github api stuff
(defmacro paradox--enforce-github-token (&rest forms)
  "If a token is defined, perform FORMS, otherwise ignore forms ask for it be defined."
  `(if (stringp paradox-github-token)
       (progn ,@forms)
     (setq paradox-github-token nil)
     (paradox--check-github-token)))

(defun paradox-menu-mark-star-unstar (&optional n)
  "Mark a package for (un)starring and move to the next line."
  (interactive "p")
  (paradox--enforce-github-token
   (unless paradox--user-starred-list
     (paradox--refresh-user-starred-list))
   ;; Get package name
   (let ((pkg (intern (car (elt (tabulated-list-get-entry) 0))))
         will-delete repo)
     (unless pkg (error "Couldn't find package-name for this entry."))
     ;; get repo for this package
     (setq repo (cdr-safe (assoc pkg paradox--package-repo-list)))
     ;; (Un)Star repo
     (if (not repo)
         (message "This package is not a GitHub repo.")
       (setq will-delete (member repo paradox--user-starred-list))
       (paradox--star-repo repo will-delete)
       (cl-incf (cdr (assoc pkg paradox--star-count))
             (if will-delete -1 1))
       (tabulated-list-set-col paradox--column-name-star
                               (paradox--package-star-count pkg)))))
  (forward-line 1))

(defun paradox-star-all-installed-packages ()
  "Star all of your currently installed packages.
No questions asked."
  (interactive)
  (paradox--enforce-github-token
   (mapc (lambda (x) (paradox--star-package-safe (car-safe x))) package-alist)))

(defun paradox--star-package-safe (pkg &optional delete query)
  (let ((repo (cdr-safe (assoc pkg paradox--package-repo-list))))
    (when (and repo (not (assoc repo paradox--user-starred-list)))
      (paradox--star-repo repo delete query))))

(defun paradox--star-repo (repo &optional delete query)
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
  (paradox--star-repo repo (not delete) query))

(defun paradox--refresh-user-starred-list ()
  (setq paradox--user-starred-list
        (paradox--github-action
         "user/starred?per_page=100" nil
         'paradox--full-name-reader)))

(defun paradox--full-name-reader ()
  "Return all \"full_name\" properties in the buffer. Much faster than `json-read'."
  (let (out)
    (while (search-forward-regexp
            "^ *\"full_name\" *: *\"\\(.*\\)\", *$" nil t)
      (add-to-list 'out (match-string-no-properties 1)))
    (goto-char (point-max))
    out))

(defun paradox--github-action-star (repo &optional delete no-result)
  (paradox--github-action (concat "user/starred/" repo)
                          (if (stringp delete) delete (if delete "DELETE" "PUT"))
                          (null no-result)))

(defun paradox--github-action (action &optional method reader)
  "Contact the github api performing ACTION with METHOD.
Default METHOD is \"GET\".

Action can be anything such as \"user/starred?per_page=100\". If
it's not a full url, it will be prepended with
\"https://api.github.com/\".

This function does nothing if `paradox-github-token' isn't set.
This function also handles the pagination used in github results,
results of each page are appended.

Return value is always a list.
- If READER is nil, the result of the action is completely
  ignored (no pagination is performed on this case, making it
  much faster).
- Otherwise:
  - If the result was a 404, the function returns nil;
  - Otherwise, READER is called as a function with point right
    after the headers and should always return a list. As a
    special exception, if READER is t, it is equivalent to a
    function that returns (t)."
  ;; Make sure the token's configured.
  (unless (stringp paradox-github-token) (keyboard-quit))
  (unless (string-match "\\`https://" action)
    (setq action (concat "https://api.github.com/" action)))
  ;; Make the request
  (message "Contacting %s" action)
  (let (next)
    (append
     (with-temp-buffer
       (save-excursion
         (shell-command
          (format "curl -s -i -d \"\" -X %s -u %s:x-oauth-basic \"%s\" "
                  (or method "GET") paradox-github-token action) t))
       (when reader
         (unless (search-forward "\nStatus: " nil t)
           (message "%s" (buffer-string))
           (error ""))
         ;; 204 means OK, but no content.
         (if (looking-at "204") '(t)
           ;; 404 is not found.
           (if (looking-at "404") nil
             ;; Anything else gets interpreted.
             (when (search-forward-regexp "^Link: .*<\\([^>]+\\)>; rel=\"next\"" nil t)
               (setq next (match-string-no-properties 1)))
             (search-forward-regexp "^?$")
             (skip-chars-forward "[:blank:]\n")
             (unless (eobp) (if (eq reader t) t (funcall reader)))))))
     (when next (paradox--github-action next method reader)))))

(defun paradox--check-github-token ()
  (if (stringp paradox-github-token)
      t
    (if (or paradox-github-token
            (not (y-or-n-p "Would you like to set up GitHub integration?
This will allow you to star/unstar packages from the Package Menu. ")))
        (customize-save-variable 'paradox-github-token t)
      (describe-variable 'paradox-github-token)
      (when (get-buffer "*Help*")
        (switch-to-buffer "*Help*")
        (delete-other-windows))
      (if (y-or-n-p "Follow the instructions on the `paradox-github-token' variable.
May I take you to the token generation page? ")
          (browse-url "https://github.com/settings/tokens/new"))
      (message "Once you're finished, simply call `paradox-list-packages' again.")
      nil)))

(provide 'paradox)
;;; paradox.el ends here.
