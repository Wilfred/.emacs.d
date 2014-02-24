;;; -*- Mode: Emacs-Lisp; outline-regexp: ";;;+ [^\n]\\|(" -*-
;;;;;; redshank.el --- Common Lisp Editing Extensions

;; Copyright (C) 2006--2012  Michael Weber

;; Author: Michael Weber <michaelw@foldr.org>
;; Keywords: languages, lisp

;; Redshank, n.  A common Old World limicoline bird (Totanus
;;   calidris), having the legs and feet pale red. The spotted
;;   redshank (T. fuscus) is larger, and has orange-red legs.
;;   Called also redleg and _CLEE_.

;;;; Commentary
;;; Setup

;; Add this to your Emacs configuration:
;;
;;   (require 'redshank-loader
;;            "/path/redshank/redshank-loader")
;;            
;;   (eval-after-load "redshank-loader"
;;      `(redshank-setup '(lisp-mode-hook
;;                         slime-repl-mode-hook) t))
;;
;; Also, this mode can be enabled with M-x redshank-mode.
;;
;; For all features to work, the accompanying redshank.lisp needs to
;; be loaded along with SLIME.  This happens automatically through
;; slime-connected-hook.  If this is undesirable, set variable
;; `redshank-install-lisp-support' to nil before loading.
;;
;;
;; Customization of redshank can be accomplished with
;; M-x customize-group RET redshank RET, or with
;; `eval-after-load':
;;
;;   (eval-after-load "redshank"
;;     '(progn ...redefine keys, etc....))
;;
;; Some of the skeleton functions (like `redshank-in-package-skeleton' or
;; `redshank-mode-line-skeleton') are good candidates for autoinsert.
;; See `redshank-setup' (in file redshank-loader.el) for examples.
;;
;; This code was tested with Paredit 21, and should run at least in
;; GNU Emacs 22 and later.

;;; To Do

;; * Unit tests; no really, there are just too many ways to mess up
;;   code and comments.

;;; Known Issues

;; `redshank-align-slot-specs-in-form':
;; * Does not work if slot forms contain newlines
;; * Does not work well with #+ and #- reader conditionals
;; * Long slot options cause large columns (:documentation ...)

;;; Contact

;; Send questions, bug reports, comments and feature suggestions to
;; Michael Weber <michaelw+redshank@foldr.org>.  New versions can be
;; found at <http://www.foldr.org/~michaelw/lisp/redshank/>.

;;; Code:
(defconst redshank-version 1)

(eval-and-compile (require 'cl))
(require 'paredit)
(require 'skeleton)
(require 'easymenu)

;;;; Customizations
(defgroup redshank nil
  "Common Lisp Editing Extensions"
  :load 'redshank
  :group 'lisp)

(defface redshank-highlight-face
  '((t (:inherit highlight)))
    "Face used to highlight extracted binders."
  :group 'redshank)

(defcustom redshank-prefix-key "C-x C-r"
  "*Prefix key sequence for redshank-mode commands.
\\{redshank-mode-map}"
  :type  'string
  :group 'redshank)

(defcustom redshank-install-lisp-support t
  "*Install Lisp-side support for Redshank.
If enabled, load the REDSHANK package into a running Lisp when
connecting via SLIME.  If disabled, some of Redshank's functions
are not available."
  :type  'boolean
  :group 'redshank)

(defcustom redshank-reformat-form-containing-slots t
  "*Reformat DEFCLASS-like forms when modifying them with Redshank commands."
  :type  'boolean
  :group 'redshank)

(defcustom redshank-align-slot-forms-list
  '("defclass" "define-condition")
  "*Regular expression matching the beginning of forms which contain
slot definitions similar to DEFCLASS."
  :type '(repeat string)
  :group 'redshank)

(defcustom redshank-canonical-slot-name-function 'identity
  "*Function which, given a slot-name, returns a canonicalized
slot name.  Use it to enforce certain slot naming style."
  :type  '(radio
           (function-item redshank-canonical-slot-name/%)
           (function-item identity)
           (function :tag "Other"))
  :group 'redshank)

(defcustom redshank-accessor-name-function 'redshank-accessor-name/get
  "*Function which, given a slot-name, returns the accessor name."
  :type  '(radio
           (function-item redshank-accessor-name/get)
           (function-item redshank-accessor-name/of)
           (function-item redshank-accessor-name/ref)
           (function-item redshank-accessor-name/%)
           (function :tag "Other"))
  :group 'redshank)

(defcustom redshank-initarg-name-function 'redshank-initarg-name/keyword
  "*Function which, given a slot-name, returns a fitting initarg name."
  :type  '(radio
           (function-item redshank-initarg-name/keyword)
           (function-item redshank-initarg-name/symbol)
           (function :tag "Other"))
  :group 'redshank)

(defcustom redshank-canonical-package-designator-function
  'redshank-package-designator/uninterned-symbol
  "*Function which, given a package-name, returns a canonicalized
package designator."
  :type  '(radio
           (function-item redshank-package-designator/uninterned-symbol)
           (function-item redshank-package-designator/keyword)
           (function-item redshank-package-designator/symbol)
           (function-item redshank-package-designator/string)
           (function :tag "Other"))
  :group 'redshank)

(defcustom redshank-licence-names
  '("BSD-style" "GPL" "LGPL" "LLGPL" "MIT" "MIT-style")
  "List of (short) licence names."
  :type '(repeat string)
  :group 'redshank)

(defcustom redshank-asdf-component-mapping
  '(("\\.html\\'"             :html-file)
    ("\\.lisp\\'"             :file)
    ("\\.\\(?:lsp\\|cl\\)\\'" :file redshank-asdf-make-spec/file-type)
    ("\\.c\\'"                :c-source-file)
    ("\\.java\\'"             :java-source-file)
    ("."                      :static-file redshank-asdf-make-spec/filename))
  "Mapping of file names to ASDF components via regexp."
  ;; XXX :type ?
  :group 'redshank)

(defcustom redshank-asdf-exclusion-regexp
  "^\\([^[:alnum:]]\\|.*~\\|CVS$\\|semantic\\.cache$\\)"
  "Files and directories matching this regular expression will be
excluded in the template generated by `redshank-asdf-defsystem-skeleton'.

The default regexp should catch in particular temporary files
\(#edited, backup~), and version control directories \(CVS, .svn,
_darcs, .git)."
  :type 'string
  :group 'redshank)

(defvar redshank-form-generator-alist
  '((lisp-mode
     ("defclass"   . redshank-defclass-skeleton)
     ("defpackage" . redshank-defpackage-skeleton)
     ("in-package" . redshank-in-package-skeleton)
     ("defsystem"  . redshank-asdf-defsystem-skeleton)
     (t            . redshank-lisp-generate-form))
    (emacs-lisp-mode
     (t . redshank-elisp-generate-form)))
  "Alist of shape \((MODE . MODE-ALIST)...).  MODE-ALIST is an
alist of shape \((KEY . GENERATOR)...), where key is a either
a string, a function, or the symbol T, and GENERATOR a nullary
function.")

(eval-and-compile
  (defvar redshank-path
    (let ((path (or (locate-library "redshank") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the Redshank package.
This is used to load the supporting Common Lisp library.  The
default value is automatically computed from the location of the
Emacs Lisp package."))

;;;; Minor Mode Definition
(defconst redshank-menu
  (let ((CONNECTEDP '(redshank-connected-p))
        (SLIMEP '(featurep 'slime)))
    `("Redshank"
      [ "Condify"                   redshank-condify-form t ]
      [ "Extract to Defun"          redshank-extract-to-defun ,CONNECTEDP ]
      [ "Extract to Enclosing Let"  redshank-letify-form-up t ]
      [ "Enclose form with Lambda"  redshank-enclose-form-with-lambda ]
      [ "Rewrite Negated Predicate" redshank-rewrite-negated-predicate t ]
      [ "Splice Progn"              redshank-maybe-splice-progn t ]
      [ "Wrap into Eval-When"       redshank-eval-whenify-form t ]
      "--"
      [ "Align Slot Specs in Form"  redshank-align-slot-specs-in-form t ]
      [ "Align Forms as Columns"    redshank-align-forms-as-columns t ]
      "--"
      [ "Complete Form"             redshank-complete-form ,SLIMEP ]
      [ "Insert Form with Slots"    redshank-form-with-slots-skeleton t ]
      [ "Insert Defclass Form"      redshank-defclass-skeleton t ]
      [ "Insert Define-Condition Form"
                                    redshank-define-condition-skeleton t ]
      [ "Insert Slot Spec Form"     redshank-slot-spec-skeleton t ]
      [ "Insert Defsystem Form"     redshank-asdf-defsystem-skeleton t ]
      [ "Insert Defpackage Form"    redshank-defpackage-skeleton ,CONNECTEDP ]
      [ "Insert In-Package Form"    redshank-in-package-skeleton ,CONNECTEDP ]
      [ "Insert Mode Line"          redshank-mode-line-skeleton t ]))
  "Standard menu for the Redshank minor mode.")

(defconst redshank-keys
  '(("A" . redshank-align-forms-as-columns)
    ("a" . redshank-align-slot-specs-in-form)
    ("c" . redshank-condify-form)
    ("e" . redshank-eval-whenify-form)
    ("f" . redshank-complete-form)
    ("l" . redshank-letify-form-up)
    ("C-l" . redshank-enclose-form-with-lambda)
    ("n" . redshank-rewrite-negated-predicate)
    ("p" . redshank-maybe-splice-progn)
    ("x" . redshank-extract-to-defun)
    ("C" . redshank-form-with-slots-skeleton)
    ("P" . redshank-defpackage-skeleton)
    ("I" . redshank-in-package-skeleton)
    ("M" . redshank-mode-line-skeleton)
    ("S" . redshank-slot-spec-skeleton))
  "Standard key bindings for the Redshank minor mode.")

(defvar redshank-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (spec redshank-keys)
      (let* ((key-spec (concat redshank-prefix-key " " (car spec)))
             (key (read-kbd-macro key-spec)))
        (define-key map key (cdr spec))))
    (define-key map (kbd "M-<mouse-1>") 'redshank-ignore-event)
    (define-key map (kbd "M-<drag-mouse-1>") 'redshank-ignore-event)
    (define-key map (kbd "M-<down-mouse-1>") 'redshank-copy-thing-at-point)
    (define-key map (kbd "M-S-<mouse-1>") 'redshank-ignore-event)
    (define-key map (kbd "M-S-<drag-mouse-1>") 'redshank-ignore-event)
    (define-key map (kbd "M-S-<down-mouse-1>") 'redshank-generate-thing-at-point)
    (easy-menu-define menu-bar-redshank map "Redshank" redshank-menu)
    map)
  "Keymap for the Redshank minor mode.")

;;;###autoload
(define-minor-mode redshank-mode
  "Minor mode for editing and refactoring (Common) Lisp code.

\\{redshank-mode-map}"
  :lighter " Redshank"
  :keymap `(,(read-kbd-macro redshank-prefix-key) . redshank-mode-map)
  (when redshank-mode
    (easy-menu-add menu-bar-redshank redshank-mode-map)))

;;;###autoload
(defun turn-on-redshank-mode ()
  "Turn on Redshank mode.  Please see function `redshank-mode'.

This function is designed to be added to hooks, for example:
  \(add-hook 'lisp-mode-hook 'turn-on-redshank-mode)"
  (interactive)
  (redshank-mode +1))

;;;; Utility Functions
(defun redshank-connected-p ()
  "Checks whether Redshank is connected to an inferior Lisp via SLIME."
  (and (featurep 'slime)
       (slime-connected-p)
       (slime-eval `(cl:packagep (cl:find-package :redshank)))))

(defun redshank-accessor-name/% (slot-name)
  "Removes preceding percent signs (%) from slot names."
  (if (string-match "^%+\\(.*\\)$" slot-name)
      (match-string-no-properties 1 slot-name)
    slot-name))

(defun redshank-accessor-name/get (slot-name)
  "GET-SLOT style accessor names."
  (concat "get-" (redshank-accessor-name/% slot-name)))

(defun redshank-accessor-name/of (slot-name)
  "SLOT-OF style accessor names."
  (concat (redshank-accessor-name/% slot-name) "-of"))

(defun redshank-accessor-name/ref (slot-name)
  "SLOT-REF style accessor names."
  (concat (redshank-accessor-name/% slot-name) "-ref"))

(defun redshank-accessor-name (slot-name)
  (if (functionp redshank-accessor-name-function)
      (funcall redshank-accessor-name-function slot-name)
    (redshank-accessor-name/get slot-name)))

(defun redshank-canonical-slot-name/% (slot-name)
  "%SLOT style slots names."
  (if (string-match "^%" slot-name)
      slot-name
    (concat "%" slot-name)))

(defun redshank-canonical-slot-name (slot-name)
  "Returns canonicalized slot name.  You can use this hook to
ensure certain style in naming your slots, for instance
%SLOT."
  (if (functionp redshank-canonical-slot-name-function)
      (funcall redshank-canonical-slot-name-function slot-name)
    slot-name))

(defun redshank-initarg-name (slot-name)
  (if (functionp redshank-initarg-name-function)
      (funcall redshank-initarg-name-function slot-name)
    (redshank-initarg-name/keyword slot-name)))

(defun redshank-initarg-name/keyword (slot-name)
  (concat ":" (redshank-accessor-name/% slot-name)))

(defun redshank-initarg-name/symbol (slot-name)
  (concat "'" (redshank-accessor-name/% slot-name)))

;;;
(defun redshank--looking-at-or-inside (spec)
  (let ((form-regex (concat "(\\(" spec "\\)\\S_"))
        (here.point (point)))
    (unless (looking-at "(")
      (ignore-errors (backward-up-list)))
    (or (looking-at form-regex)
        (prog1 nil
          (goto-char here.point)))))

(defun redshank--align-slot-form-regexp ()
  (mapconcat 'identity redshank-align-slot-forms-list "\\|"))

(defun redshank-maybe-splice-progn ()
  "Splice PROGN form at point into its surrounding form.
Nothing is done if point is not preceding a PROGN form."
  (interactive "*")
  (paredit-point-at-sexp-start)
  (when (redshank--looking-at-or-inside "progn")
    (paredit-forward-kill-word)
    (delete-region (prog1 (point) (paredit-skip-whitespace t))
                   (point))
    (paredit-splice-sexp-killing-backward)
    (paredit-point-at-sexp-start)))

(defun redshank-point-at-enclosing-let-form ()
  "Move point to enclosing LET/LET* form if existing.
Point is not moved across other binding forms \(e.g., DEFUN,
LABELS or FLET.)"
  (interactive)
  (let ((here.point (point)))
    (or (ignore-errors
          (block nil
            (backward-up-list)
            (while (not (looking-at "(let\\*?\\S_"))
              (when (looking-at "(\\(def\\s_*\\|labels\\|flet\\)\\S_")
                (return nil))
              (backward-up-list))
            (point)))
        (prog1 nil
          (goto-char here.point)))))

(defun redshank--symbol-namep (symbol)
  (and (stringp symbol)
       (not (string= symbol ""))))

(defun redshank--trim-whitespace (string)
  (when (string-match "^\\s *\\(.*?\\)\\s *$" string)
    (match-string-no-properties 1 string)))

(defun redshank-canonical-package-name (package-name)
  (and package-name (not (string= "" package-name))
       ;; very naive
       (lexical-let ((package-name (redshank--trim-whitespace package-name)))
         (if (or (string-match "^#?:\\(.*\\)$" package-name)
                 (string-match "^\"\\(.*\\)\"$" package-name))
             (match-string-no-properties 1 package-name)
           package-name))))

(defun redshank-canonical-package-designator (package-name)
  (and package-name (not (string= "" package-name))
       (funcall redshank-canonical-package-designator-function
                (redshank-canonical-package-name package-name))))

(defun redshank-package-designator/uninterned-symbol (package-name)
  (concat "#:" (downcase package-name)))

(defun redshank-package-designator/keyword (package-name)
  (concat ":" (downcase package-name)))

(defun redshank-package-designator/symbol (package-name)
  (downcase package-name))

(defun redshank-package-designator/string (package-name)
  (prin1-to-string (upcase package-name)))


(defun redshank--end-of-sexp-column ()
  "Move point to end of current form, neglecting trailing whitespace."
  (forward-sexp)
  (while (forward-comment +1))
  (skip-chars-backward "[:space:]"))

(defun redshank--sexp-column-widths ()
  "Return list of column widths for s-expression at point."
  (down-list)
  (loop do (while (forward-comment 1))
        until (or (looking-at ")") (eobp))
        collect (- (- (point)
                      (progn
                        (redshank--end-of-sexp-column)
                        (point))))
        finally (up-list)))

(defun redshank--max* (&rest args)
  (reduce #'max args :key (lambda (arg) (or arg 0))))

(defun redshank-align-sexp-columns (column-widths)
  "Align expressions in S-expression at point.
COLUMN-WIDTHS is expected to be a list."
  (down-list)
  (loop initially (while (forward-comment +1))
        for width in column-widths
        until (looking-at ")")
        do (let ((beg (point)))
             (redshank--end-of-sexp-column)
             (let ((used (- (point) beg)))
               (just-one-space (if (looking-at "[[:space:]]*)") 0
                                 (1+ (- width used))))))
        finally (up-list)))

(defun redshank--slot-form-at-point-p ()
  (ignore-errors
    (save-excursion
      (backward-up-list +3)
      (redshank--looking-at-or-inside (redshank--align-slot-form-regexp)))))

(defun redshank--region-active-p ()
  "Returns true if `transient-mark-mode' is used and region is active."
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (boundp 'mark-active)
       mark-active))

(defun redshank-ignore-event (event)
  "Ignores a (mouse) event.
This is used to override mouse bindings in a minor mode keymap,
but does otherwise nothing."
  (interactive "e"))

(defmacro redshank--with-doublequotes (&rest body)
  `(progn
     (paredit-doublequote)
     (insert (or (progn ,@body) ""))
     (paredit-doublequote)
     nil))

;; lenient variant of `slime-read-package-name'
(defun redshank-read-package-name (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (redshank-canonical-package-name
     (completing-read prompt (when (and (featurep 'slime)
                                        (redshank-connected-p))
                               (slime-bogus-completion-alist
                                (slime-eval
                                 `(swank:list-all-package-names t))))
                      nil nil initial-value nil initial-value))))

(defun redshank-find-potential-buffer-package ()
  (redshank-canonical-package-name
   (or slime-buffer-package
       (and (fboundp 'slime-find-buffer-package)
            (slime-find-buffer-package))
       (let ((case-fold-search t)
             (regexp (concat "^(\\(cl:\\|common-lisp:\\)?defpackage\\>[ \t']*"
                             "\\([^()]+\\)")))
         (save-excursion
           (when (or (re-search-backward regexp nil t)
                     (re-search-forward  regexp nil t))
             (match-string-no-properties 2)))))))

(defun redshank--assoc-match (key alist)
  (loop for entry in alist do
        (cond ((stringp (car entry))
               (when (eq t (compare-strings (car entry) 0 nil
                                            key 0 nil
                                            case-fold-search))
                 (return entry)))
              ((functionp (car entry))
               (when (funcall (car entry) key)
                 (return entry)))
              ((eq t (car entry))
               (return entry)))))

;;; ASDF
(defun redshank-walk-filesystem (spec enter-fn leave-fn)
  (when (and (funcall enter-fn
                      (file-name-directory spec)
                      (file-name-nondirectory spec))
             (file-directory-p spec))
    (let ((contents (sort (directory-files spec nil nil nil)
                          #'string<)))
      (dolist (file contents)
        (unless (member file '("." ".."))
          (redshank-walk-filesystem (concat (file-name-as-directory spec) file)
                                    enter-fn leave-fn)))
      (when leave-fn
        (funcall leave-fn
                 (file-name-directory spec)
                 (file-name-nondirectory spec))))))

(defun redshank-asdf-make-spec/file-type (filename)
  (list (file-name-sans-extension filename)
        :type (file-name-extension filename)))

(defun redshank-asdf-make-spec/filename (filename)
  (list* filename (when (file-name-extension filename)
                    (list :pathname filename))))

(defun redshank-asdf-classify-component (directory filename)
  (dolist (mapping redshank-asdf-component-mapping)
    (destructuring-bind (regex tag &optional filename-fn) mapping
      (when (string-match regex (concat directory filename))
        (return `(,tag ,@(if filename-fn
                             (funcall filename-fn filename)
                           (list (file-name-sans-extension filename)))))))))

(defun redshank-asdf-insert-module-components (directory)
  "Inserts DIRECTORY as ASDF module into current buffer.
The outermost :module/:components is not provided."
  (interactive "*DDirectory: ")
  (paredit-open-parenthesis)
  (redshank-walk-filesystem
   (file-name-as-directory directory)
   (lambda (dir file)
     (cond ((member file '("." ""))
            ;; top-level directory should not get a :module header,
            ;; but should be searched
            t)
           ((string-match redshank-asdf-exclusion-regexp file)
            nil)
           ((file-regular-p (concat dir file))
            (let ((desc (redshank-asdf-classify-component dir file)))
              (when desc
                (prin1 desc (current-buffer))
                (newline-and-indent))
              nil))
           ((file-directory-p (concat dir file))
            (paredit-open-parenthesis)
            (insert ":module " file)
            (newline-and-indent)
            (insert ":serial t")
            (newline-and-indent)
            (insert ":components ")
            (newline-and-indent)
            (paredit-open-parenthesis)
            ;; do descend into directory
            t)))
   (lambda (dir file)
     (unless (or (member file '("." ""))
                 (string-match redshank-asdf-exclusion-regexp file))
       (paredit-close-parenthesis)
       (paredit-close-parenthesis)
       (newline-and-indent))))
  (paredit-close-parenthesis))

(defun redshank-asdf-read-system-name (prompt &optional initial-input default)
  "Read from the minibuffer and return the name of an ASDF system.
Does not perform input validation.

PROMPT can be any valid argument to `concat'.  It will be
displayed as the prompt when reading from the minibuffer.

Uses `slime-read-system-name' if it is available."
  (if (and (fboundp 'slime-read-system-name)
           (redshank-connected-p))
      (slime-read-system-name prompt initial-input)
    (read-string prompt initial-input nil default)))

;;; Highlighting
(defvar redshank-letify-overlay
  (let ((overlay (make-overlay 1 1)))
    (overlay-put overlay 'face 'redshank-highlight-face)
    overlay)
  "Overlay to highlight letified binders.")

(defun redshank-highlight-binder (beg end)
  (move-overlay redshank-letify-overlay beg end))

(defun redshank-unhighlight-binder ()
  (interactive)
  (delete-overlay redshank-letify-overlay))

;;; Hooking into SLIME
(defun redshank-on-connect ()
  "Activate Lisp-side support for Redshank."
  (slime-eval-async
   `(cl:progn
      (cl:pushnew (cl:pathname ,redshank-path) swank::*load-path*
                  :test 'cl:equal)
      (cl:ignore-errors (swank:swank-require :redshank)))))

(defun redshank-slime-install ()
  "Install Redshank hook for SLIME connections."
  (add-hook 'slime-connected-hook 'redshank-on-connect))

(defun redshank-slime-uninstall ()
  "Uninstall Redshank hook from SLIME."
  (remove-hook 'slime-connected-hook 'redshank-on-connect))

;;;; Form Frobbing
(defun redshank-letify-form (var)
  "Extract the form at point into a new LET binding.
The binding variable's name is requested in the mini-buffer."
  (interactive "*sVariable name: ")
  (when (redshank--symbol-namep var)
    (paredit-point-at-sexp-start)
    (paredit-wrap-sexp +1)              ; wrap with (LET ...)
    (insert "let ")
    (paredit-wrap-sexp +1)              ; wrap binders
    (let ((binder.start (point)))
      (paredit-wrap-sexp +1)
      (insert var " ")
      (up-list)
      (redshank-highlight-binder binder.start (point)))
    (up-list)                           ; point at LET body
    (paredit-newline)
    (save-excursion                     ; insert variable name
      (insert var))))

(defun redshank-letify-form-up (var &optional arg)
  "Extract the form at point into a (possibly enclosing) LET binding.
The binding variable's name is requested in the mini-buffer.
With prefix argument, or if no suitable binding can be found,
`redshank-letify-form' is executed instead."
  (interactive "*sVariable name: \nP")
  (let ((let.start (save-excursion
                     (redshank-point-at-enclosing-let-form))))
    (cond ((and (redshank--symbol-namep var)
                (not arg)
                let.start)
           (paredit-point-at-sexp-start)
           (let* ((form.start (prog1 (point) (forward-sexp)))
                  (form (delete-and-extract-region form.start (point))))
             (save-excursion
               (insert var)
               (goto-char let.start)
               (down-list)              ; move point from |(let ...
               (forward-sexp +2)        ; to behind last binder form
               (backward-down-list)
               (paredit-newline)        ; insert new binder
               (let ((binder.start (point)))
                 (insert "(" var " " form ")")
                 (redshank-highlight-binder binder.start (point)))
               (backward-sexp)          ; ... and reindent it
               (indent-sexp))))
          (t (redshank-letify-form var)))))

(defun redshank-extract-to-defun (start end name &optional package)
  "Extracts region from START to END as new defun NAME.
The marked region is replaced with a call, the actual function
definition is placed on the kill ring.

A best effort is made to determine free variables in the marked
region and make them parameters of the extracted function.  This
involves macro-expanding code, and as such might have side effects."
  (interactive "*r\nsName for extracted function: ")
  (let* ((form-string (buffer-substring-no-properties start end))
         (free-vars (slime-eval `(redshank:free-vars-for-emacs
                                  ,(concat "(locally " form-string ")")
                                  ,(or package (slime-pretty-package-name
                                                (slime-current-package))))
                                package)))
    (flet ((princ-to-string (o)
             (with-output-to-string
               (princ (if (null o) "()" o)))))
      (with-temp-buffer
        (lisp-mode)                     ; for proper indentation
        (insert "(defun " name " " (princ-to-string free-vars) "\n")
        (insert form-string ")\n")
        (goto-char (point-min))
        (indent-sexp)
        (paredit-hack-kill-region (point-min) (point-max))
        (message (substitute-command-keys
                  "Extracted function `%s' now on kill ring; \\[yank] to insert at point.") ;
                 name))
      (delete-region start end)
      (princ (list* name free-vars) (current-buffer)))))

(defun redshank-enclose-form-with-lambda (arglist)
  "Enclose form with lambda expression with parameter VAR.
With prefix argument ARG, enclose ARG upward forms.

Example:
  \(foo x (bar y| z) qux)

\\[redshank-enclose-form-with-lambda] RET RET yields:

  \(foo x (lambda (y) (bar y| z)) qux)"
  (interactive
   (let ((arglist (thing-at-point 'symbol)))
     (when (and (stringp arglist)
                (string-match "[(]" arglist))
       (setq arglist ""))
     (list (read-string "Lambda arglist: " arglist))))
  (save-excursion
    (call-interactively 'backward-up-list)
    (paredit-wrap-sexp +1)
    (insert "lambda (" arglist ")")
    (if (> (- (line-end-position) (line-beginning-position))
           (current-fill-column))
        (newline)
      (insert " "))
    (backward-up-list)
    (indent-sexp)))

(defun redshank-condify-form ()
  "Transform a Common Lisp IF form into an equivalent COND form."
  (interactive "*")
  (flet ((redshank--frob-cond-branch ()
            (paredit-wrap-sexp +2)
            (forward-sexp)
            (redshank-maybe-splice-progn)))
    (save-excursion
      (unless (redshank--looking-at-or-inside "if")
        (error "Cowardly refusing to mutilate other forms than IF"))
      (paredit-forward-kill-word)
      (insert "cond")
      (just-one-space)
      (redshank--frob-cond-branch)
      (up-list)
      (paredit-newline)
      (save-excursion (insert "t "))
      (redshank--frob-cond-branch))))

(defun redshank-eval-whenify-form (&optional n)
  "Wraps top-level form at point with (EVAL-WHEN (...) ...).
With optional numeric argument, wrap N top-level forms."
  ;; A slightly modified version of `asf-eval-whenify' from
  ;; <http://boinkor.net/archives/2006/11/three_useful_emacs_hacks_for_l.html>
  (interactive "*p")
  (save-excursion
    (if (and (boundp 'slime-repl-input-start-mark)
             slime-repl-input-start-mark)
        (slime-repl-beginning-of-defun)
      (beginning-of-defun))
    (paredit-wrap-sexp n)
    (insert "eval-when (:compile-toplevel :load-toplevel :execute)\n")
    (backward-up-list)
    (indent-sexp)))

(defun redshank-rewrite-negated-predicate ()
  "Rewrite the negated predicate of a WHEN or UNLESS form at point."
  (interactive "*")
  (save-excursion
    (flet ((redshank--frob-form (new-head)
             (paredit-forward-kill-word)
             (insert new-head)
             (paredit-forward-kill-word)
             (paredit-splice-sexp-killing-backward)
             (just-one-space)))
      ;; Okay, I am cheating here...
      (cond ((redshank--looking-at-or-inside "when\\s-+(not")
             (redshank--frob-form "unless"))
            ((redshank--looking-at-or-inside "unless\\s-+(not")
             (redshank--frob-form "when"))
            (t
             (error "Cowardly refusing to mutilate unknown form"))))))

(defun redshank-align-forms-as-columns (beg end)
  "Align S-expressions in region as columns.
Example:
  \(define-symbol-macro MEM (mem-of *cpu*))
  \(define-symbol-macro IP (ip-of *cpu*))
  \(define-symbol-macro STACK (stack-of *cpu*))

is formatted as:

  \(define-symbol-macro MEM   (mem-of *cpu*))
  \(define-symbol-macro IP    (ip-of *cpu*))
  \(define-symbol-macro STACK (stack-of *cpu*))"
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (let* ((columns
            (loop do (while (forward-comment +1))
                  until (or (looking-at ")") (eobp))
                  collect (redshank--sexp-column-widths)))
           (max-column-widths
            (loop for cols = columns then (mapcar #'cdr cols)
                  while (some #'consp cols)
                  collect (apply #'redshank--max* (mapcar #'car cols)))))
      (goto-char beg)
      (loop do (while (forward-comment +1))
            until (or (looking-at ")") (eobp))
            do (redshank-align-sexp-columns max-column-widths)))))

(defun redshank-align-slot-specs-in-form ()
  "Align slots of a Common Lisp DEFCLASS-like form at point.
Example (| denotes cursor position):
|(defclass identifier ()
   ((name :reader get-name :initarg :name)
    (location :reader get-location :initarg :location)
    (scope :accessor get-scope :initarg :scope)
    (definition :accessor get-definition :initform nil))
   (:default-initargs :scope *current-scope*))

is formatted to:

|(defclass identifier ()
   ((name       :reader   get-name       :initarg  :name)
    (location   :reader   get-location   :initarg  :location)
    (scope      :accessor get-scope      :initarg  :scope)
    (definition :accessor get-definition :initform nil))
   (:default-initargs :scope *current-scope*))

This also works for DEFINE-CONDITION, etc.  See also:
`redshank-align-slot-form-list'"
  (interactive "*")
  (when (redshank--looking-at-or-inside (redshank--align-slot-form-regexp))
    (save-excursion
      (down-list)
      (forward-sexp +3)                 ; move to slots definitions
      (let ((slots.end (save-excursion (forward-sexp) (point))))
        (redshank-align-forms-as-columns (progn (down-list) (point))
                                         slots.end)))
    (indent-sexp)))

(defun redshank-complete-form ()
  "If a Common Lisp DEFCLASS-like slot form is at point, attempt to complete it.
The surrounding form is reformatted, if this is enabled by
`redshank-reformat-form-containing-slots'.

If point is not in a slot form, fall back to `slime-complete-form'.

\\<redshank-mode-map>\\[redshank-complete-form]

\(defclass foo ()
  \(...
   \(slot-n |)
   ...))
  ->
\(defclass foo ()
  \(...
   \(slot-n :accessor get-slot-n :initarg :slot-n)|
   ...))"
  (interactive "*")
  (if (not (redshank--slot-form-at-point-p))
      (call-interactively 'slime-complete-form)
    (backward-up-list)
    (down-list)
    (let ((slot-name (substring-no-properties (thing-at-point 'symbol))))
      (when slot-name
        (forward-sexp)
        (just-one-space)
        (let ((start (point)))
          (paredit-ignore-sexp-errors
            (while (not (eobp))
              (forward-sexp)))
          (delete-region start (point)))
        (insert ":accessor " (redshank-accessor-name slot-name)
                " :initarg " (redshank-initarg-name slot-name))
        (up-list)
        (when redshank-reformat-forms-containing-slots
          (save-excursion
            (backward-up-list +2) ; to beginning of defclass-like form
            (redshank-align-slot-specs-in-form)))))))

(defun redshank-copy-thing-at-point (event)
  "Insert at point the syntactical element clicked on with the mouse.
Clicking on an open parenthesis inserts the whole form,
clicking on a symbol, number, string, etc., inserts it,
clicking within a (line) comment, inserts the comment up to the
end of the line.

When `transient-mark-mode' is enabled, and a region is
active, it is deleted.

This should be bound to a mouse click event type."
  (interactive "*e")
  (let* ((echo-keystrokes 0)
	 (start-posn (event-start event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn)))
    (let ((contents
           (with-current-buffer (window-buffer start-window)
             (save-excursion
               (goto-char start-point)
               (cond ((paredit-in-comment-p)
                      (skip-syntax-backward "^<")
                      (skip-syntax-backward "<")
                      (let ((comment.start (point)))
                        (end-of-line)
                        (buffer-substring comment.start (point))))
                     ((and (not (paredit-in-string-p))
                           (looking-at ";"))
                      (let ((comment.start (point)))
                        (end-of-line)
                        (buffer-substring comment.start (point))))
                     (t (thing-at-point 'sexp)))))))
      (cond ((and (stringp contents)
                  (not (equal "" contents)))
             (when (redshank--region-active-p)
               (delete-region (region-beginning) (region-end)))
             (unless (or (bolp)
                         (and (minibufferp)
                              (= (point) (minibuffer-prompt-end)))
                         (save-excursion
                           (backward-char)
                           (looking-at "\\s-\\|\\s\(")))
               (insert " "))
             (let ((contents.start (point)))
               (insert contents)
               (unless (or (eolp)
                           (and (minibufferp)
                                (= (point) (minibuffer-prompt-end)))
                           (looking-at "\\s-\\|\\s\)"))
                 (insert " "))
               (save-excursion
                 (goto-char contents.start)
                 (indent-sexp))))
            (t
             (message "Don't know what to copy?"))))))

;;;
(defvar redshank-thing-at-point)

(defun redshank-elisp-generate-form (&optional name)
  (interactive "*")
  (require 'eldoc)
  (let* ((sym (intern-soft (or name redshank-thing-at-point)))
         (args (eldoc-function-argstring sym)))
    (save-match-data
      (string-match "\\`[^ )]* ?" args)
      (setq args (substring args (match-end 0)))
      (insert (format "(%s " sym))
      (let ((point (point)))
        (insert args)
        (goto-char point)))))

(defun redshank-lisp-generate-form (&optional name)
  (interactive "*")
  (insert "(" (or name redshank-thing-at-point) " )")
  (backward-char +1)
  (when (fboundp 'slime-complete-form)
    (slime-complete-form)))

(defun redshank-generate-thing-at-point (event)
  "Generates a (mode-specific) form corresponding to the symbol at point.
The actual generator function is determined by
`redshank-form-generator-alist'.

Generators can access the actual value dispatched on via
REDSHANK-THING-AT-POINT."
  (interactive "*e")
  (let* ((echo-keystrokes 0)
	 (start-posn (event-start event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn))
         (redshank-thing-at-point
          (with-current-buffer (window-buffer start-window)
            (save-excursion
              (goto-char start-point)
              (thing-at-point 'symbol))))
         (mode-table (assq major-mode redshank-form-generator-alist))
         (generator (redshank--assoc-match redshank-thing-at-point
                                           (cdr mode-table))))
    (if generator
        (if (interactive-p)
            (call-interactively (cdr generator))
          (funcall (cdr generator)))
      (message "Don't know a generator for `%s'." redshank-thing-at-point))))

;;;; Skeletons
(define-skeleton redshank-mode-line-skeleton
  "Inserts mode line."
  nil
  (concat ";;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp;"
          (if buffer-file-coding-system
              (let ((coding (coding-system-get buffer-file-coding-system
                                               'mime-charset)))
                (if coding (concat " Coding:" (symbol-name coding))
                  ""))
            "") " -*-")
  & \n & \n
  _)

(define-skeleton redshank-in-package-skeleton
  "Inserts mode line and Common Lisp IN-PACKAGE form."
  (redshank-canonical-package-designator
   (redshank-read-package-name "Package: "
                               (redshank-find-potential-buffer-package)))
  '(if (bobp) (redshank-mode-line-skeleton))
  '(paredit-open-parenthesis)
  "in-package " str
  '(paredit-close-parenthesis) \n
  \n _)

(define-skeleton redshank-defpackage-skeleton
  "Inserts a Common Lisp DEFPACKAGE skeleton."
  (redshank-canonical-package-designator
   (skeleton-read "Package: " (or (ignore-errors
                                    (file-name-sans-extension
                                     (file-name-nondirectory
                                      (buffer-file-name))))
                                  "TEMP")))
  '(paredit-open-parenthesis) "defpackage " str
  \n '(paredit-open-parenthesis)
     ":nicknames" ((redshank-canonical-package-designator
                    (skeleton-read "Nickname: ")) " " str)
   & '(paredit-close-parenthesis) & \n
   | '(progn
        (backward-up-list)
        (kill-sexp))
  '(paredit-open-parenthesis)
  ":use " (redshank-canonical-package-designator "cl")
          ((redshank-canonical-package-designator
            (redshank-read-package-name "USEd package: ")) " " str)
  '(paredit-close-parenthesis)
  '(paredit-close-parenthesis) \n
  \n _)

(define-skeleton redshank-asdf-defsystem-skeleton
  "Inserts an ASDF:DEFSYSTEM skeleton."
  (skeleton-read "System: " (or (ignore-errors
                                  (file-name-sans-extension
                                   (file-name-nondirectory
                                    (buffer-file-name))))
                                "TEMP"))
  '(when (member major-mode '(fundamental-mode text-mode))
     (asdf-mode))
  '(paredit-open-parenthesis) "asdf:defsystem " str
  \n ":version \"0\""
  \n ":description " (redshank--with-doublequotes
                      (skeleton-read "Short description: "))
  \n ":maintainer \"" user-full-name " <" user-mail-address ">\""
  \n ":author \"" user-full-name " <" user-mail-address ">\""
  \n ":licence "
    (redshank--with-doublequotes
      (let ((completion-ignore-case t))
        (completing-read (concat "Licence (default: "
                                 (first redshank-licence-names) "): ")
                         redshank-licence-names
                         nil nil nil nil (first redshank-licence-names))))
  \n ":depends-on"
     '(paredit-open-parenthesis)
     ((redshank-asdf-read-system-name "Depends on: ") str " ") & -1
     '(paredit-close-parenthesis)
  \n ":serial t"
  \n ";; components likely need manual reordering"
  \n ":components " (condition-case nil
                        (redshank-asdf-insert-module-components
                         (read-directory-name "Directory: "))
                      ((quit) "()"))
  \n ";; :long-description \"\""
  \n '(paredit-close-parenthesis)
  \n _)

(define-skeleton redshank-form-with-slots-skeleton
  "Inserts a Common Lisp form skeleton containing slot specs."
  (completing-read "Type: " redshank-align-slot-forms-list nil nil
                   (first redshank-align-slot-forms-list))
  '(paredit-open-parenthesis) str " "
  '(setq v1 (skeleton-read "Name: ")) v1 " "
  '(paredit-open-parenthesis)
  ((skeleton-read "Superclass: ") str " ") & -1
  '(paredit-close-parenthesis)
  \n '(indent-according-to-mode)
  '(paredit-open-parenthesis)
  ((skeleton-read "Slot: ")
   '(indent-according-to-mode)
   '(paredit-open-parenthesis)
   str
   ;; Ugly, but skeleton-read _must_ have the first str literal
   '(backward-delete-char (length str))
   (redshank-canonical-slot-name str)
   " :accessor " (redshank-accessor-name str)
   " :initarg " (redshank-initarg-name str)
   '(paredit-close-parenthesis) \n) & '(join-line)
  '(paredit-close-parenthesis)
  ;; \n "(:default-initargs " - ")" ;; add to your liking...
  '(paredit-close-parenthesis) "\n" \n
  _)

(defun redshank-defclass-skeleton ()  
  "Inserts a Common Lisp DEFCLASS skeleton."
  (interactive "*")
  (redshank-form-with-slots-skeleton "defclass"))

(defun redshank-define-condition-skeleton ()  
  "Inserts a Common Lisp DEFINE-CONDITION skeleton."
  (interactive "*")
  (redshank-form-with-slots-skeleton "define-condition"))

(define-skeleton redshank-slot-spec-skeleton
  "Inserts a Common Lisp DEFCLASS slot skeleton."
  "Slot: "
  ((skeleton-read "Slot: ")
   '(indent-according-to-mode)
   '(paredit-open-parenthesis)
   str
   ;; Ugly, but skeleton-read _must_ have the first str literal
   '(backward-delete-char (length str))
   (redshank-canonical-slot-name str)
   " :accessor " (redshank-accessor-name str)
   " :initarg " (redshank-initarg-name str)
   '(paredit-close-parenthesis) \n) & '(join-line)
   _)

(defadvice redshank-form-with-slots-skeleton
  (after redshank-format-form-with-slots activate)
  "Align DEFCLASS-like slots."
  (when redshank-reformat-form-containing-slots
    (save-excursion
      (backward-sexp)
      (redshank-align-slot-specs-in-form))))

(defadvice redshank-slot-spec-skeleton
  (after redshank-reformat-defclass activate)
  "Align DEFCLASS slots."
  (when redshank-reformat-form-containing-slots
    (save-excursion
      (backward-up-list +2)
      (redshank-align-slot-specs-in-form))))

;;;; ASDF mode
;;;###autoload
(define-derived-mode asdf-mode lisp-mode "ASDF"
  "Major mode for ASDF files.  This mode is derived from `lisp-mode'
and activates minor mode `redshank-mode' by default.

\\{asdf-mode-map}"
  (add-hook 'asdf-mode-hook 'turn-on-redshank-mode))

;;;###autoload
(defun turn-on-asdf-mode ()
  "Turn on ASDF mode.  Please see function `asdf-mode'.

This function is designed to be added to hooks, for example:
  \(add-hook 'lisp-mode-hook 'turn-on-asdf-mode)"
  (interactive)
  (asdf-mode))

;;;; Initialization
(eval-after-load "slime"
  '(progn
     (substitute-key-definition 'slime-complete-form 'redshank-complete-form
                                redshank-mode-map slime-mode-map)
     (when redshank-install-lisp-support
       (redshank-slime-install))))

(add-hook 'pre-command-hook 'redshank-unhighlight-binder)
(provide 'redshank)

;;;; Licence

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation;

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
