;;; org-macro.el --- Macro Replacement Code for Org Mode

;; Copyright (C) 2013-2015 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Macros are expanded with `org-macro-replace-all', which relies
;; internally on `org-macro-expand'.

;; Default templates for expansion are stored in the buffer-local
;; variable `org-macro-templates'.  This variable is updated by
;; `org-macro-initialize-templates', which recursively calls
;; `org-macro--collect-macros' in order to read setup files.

;; Argument in macros are separated with commas. Proper escaping rules
;; are implemented in `org-macro-escape-arguments' and arguments can
;; be extracted from a string with `org-macro-extract-arguments'.

;; Along with macros defined through #+MACRO: keyword, default
;; templates include the following hard-coded macros:
;; {{{time(format-string)}}}, {{{property(node-property)}}},
;; {{{input-file}}} and {{{modification-time(format-string)}}}.

;; Upon exporting, "ox.el" will also provide {{{author}}}, {{{date}}},
;; {{{email}}} and {{{title}}} macros.

;;; Code:
(require 'org-macs)
(require 'org-compat)

(declare-function org-element-at-point "org-element" ())
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-map "org-element"
		  (data types fun &optional info first-match no-recursion
			with-affiliated))
(declare-function org-element-parse-buffer "org-element"
		  (&optional granularity visible-only))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-file-contents "org" (file &optional noerror))
(declare-function org-mode "org" ())
(declare-function org-remove-double-quotes "org" (s))
(declare-function org-with-wide-buffer "org-macs" (&rest body))

;;; Variables

(defvar org-macro-templates nil
  "Alist containing all macro templates in current buffer.
Associations are in the shape of (NAME . TEMPLATE) where NAME
stands for macro's name and template for its replacement value,
both as strings.  This is an internal variable.  Do not set it
directly, use instead:

  #+MACRO: name template")
(make-variable-buffer-local 'org-macro-templates)


;;; Functions

(defun org-macro--collect-macros ()
  "Collect macro definitions in current buffer and setup files.
Return an alist containing all macro templates found."
  (let* (collect-macros			; For byte-compiler.
	 (collect-macros
	  (lambda (files templates)
	    ;; Return an alist of macro templates.  FILES is a list of
	    ;; setup files names read so far, used to avoid circular
	    ;; dependencies.  TEMPLATES is the alist collected so far.
	    (let ((case-fold-search t))
	      (org-with-wide-buffer
	       (goto-char (point-min))
	       (while (re-search-forward
		       "^[ \t]*#\\+\\(MACRO\\|SETUPFILE\\):" nil t)
		 (let ((element (org-element-at-point)))
		   (when (eq (org-element-type element) 'keyword)
		     (let ((val (org-element-property :value element)))
		       (if (equal (org-element-property :key element) "MACRO")
			   ;; Install macro in TEMPLATES.
			   (when (string-match
				  "^\\(.*?\\)\\(?:\\s-+\\(.*\\)\\)?\\s-*$" val)
			     (let* ((name (match-string 1 val))
				    (template (or (match-string 2 val) ""))
				    (old-cell (assoc name templates)))
			       (if old-cell (setcdr old-cell template)
				 (push (cons name template) templates))))
			 ;; Enter setup file.
			 (let ((file (expand-file-name
				      (org-remove-double-quotes val))))
			   (unless (member file files)
			     (with-temp-buffer
			       (setq default-directory
				     (file-name-directory file))
			       (org-mode)
			       (insert (org-file-contents file 'noerror))
			       (setq templates
				     (funcall collect-macros (cons file files)
					      templates)))))))))))
	      templates))))
    (funcall collect-macros nil nil)))

(defun org-macro-initialize-templates ()
  "Collect macro templates defined in current buffer.
Templates are stored in buffer-local variable
`org-macro-templates'.  In addition to buffer-defined macros, the
function installs the following ones: \"property\",
\"time\". and, if the buffer is associated to a file,
\"input-file\" and \"modification-time\"."
  (let* ((templates (org-macro--collect-macros))
	 (update-templates
	  (lambda (cell)
	    (let ((old-template (assoc (car cell) templates)))
	      (if old-template (setcdr old-template (cdr cell))
		(push cell templates))))))
    ;; Install hard-coded macros.
    (mapc update-templates
	  (list (cons "property"
		      "(eval (save-excursion
        (let ((l \"$2\"))
          (when (org-string-nw-p l)
            (condition-case _
                (let ((org-link-search-must-match-exact-headline t))
                  (org-link-search l nil t))
              (error
               (error \"Macro property failed: cannot find location %s\"
                      l)))))
        (org-entry-get nil \"$1\" 'selective)))")
		(cons "time" "(eval (format-time-string \"$1\"))")))
    (let ((visited-file (buffer-file-name (buffer-base-buffer))))
      (when (and visited-file (file-exists-p visited-file))
	(mapc update-templates
	      (list (cons "input-file" (file-name-nondirectory visited-file))
		    (cons "modification-time"
			  (format "(eval (format-time-string \"$1\" '%s))"
				  (prin1-to-string
				   (nth 5 (file-attributes visited-file)))))))))
    (setq org-macro-templates templates)))

(defun org-macro-expand (macro templates)
  "Return expanded MACRO, as a string.
MACRO is an object, obtained, for example, with
`org-element-context'.  TEMPLATES is an alist of templates used
for expansion.  See `org-macro-templates' for a buffer-local
default value.  Return nil if no template was found."
  (let ((template
	 ;; Macro names are case-insensitive.
	 (cdr (assoc-string (org-element-property :key macro) templates t))))
    (when template
      (let ((value (replace-regexp-in-string
                    "\\$[0-9]+"
                    (lambda (arg)
                      (or (nth (1- (string-to-number (substring arg 1)))
                               (org-element-property :args macro))
                          ;; No argument: remove place-holder.
                          ""))
                    template nil 'literal)))
        ;; VALUE starts with "(eval": it is a s-exp, `eval' it.
        (when (string-match "\\`(eval\\>" value)
          (setq value (eval (read value))))
        ;; Return string.
        (format "%s" (or value ""))))))

(defun org-macro-replace-all (templates &optional finalize keywords)
  "Replace all macros in current buffer by their expansion.

TEMPLATES is an alist of templates used for expansion.  See
`org-macro-templates' for a buffer-local default value.

If optional arg FINALIZE is non-nil, raise an error if a macro is
found in the buffer with no definition in TEMPLATES.

Optional argument KEYWORDS, when non-nil is a list of keywords,
as strings, where macro expansion is allowed."
  (save-excursion
    (goto-char (point-min))
    (let ((properties-regexp
	   (format "\\`EXPORT_%s\\+?\\'" (regexp-opt keywords)))
	  record)
      (while (re-search-forward "{{{[-A-Za-z0-9_]" nil t)
	(let* ((datum (save-match-data (org-element-context)))
	       (type (org-element-type datum))
	       (macro
		(cond
		 ((eq type 'macro) datum)
		 ;; In parsed keywords and associated node properties,
		 ;; force macro recognition.
		 ((or (and (eq type 'keyword)
			   (member (org-element-property :key datum) keywords))
		      (and (eq type 'node-property)
			   (org-string-match-p
			    properties-regexp
			    (org-element-property :key datum))))
		  (save-restriction
		    (narrow-to-region (match-beginning 0) (line-end-position))
		    (org-element-map (org-element-parse-buffer) 'macro
		      #'identity nil t))))))
	  (when macro
	    (let* ((value (org-macro-expand macro templates))
		   (begin (org-element-property :begin macro))
		   (signature (list begin
				    macro
				    (org-element-property :args macro))))
	      ;; Avoid circular dependencies by checking if the same
	      ;; macro with the same arguments is expanded at the same
	      ;; position twice.
	      (cond ((member signature record)
		     (error "Circular macro expansion: %s"
			    (org-element-property :key macro)))
		    (value
		     (push signature record)
		     (delete-region
		      begin
		      ;; Preserve white spaces after the macro.
		      (progn (goto-char (org-element-property :end macro))
			     (skip-chars-backward " \t")
			     (point)))
		     ;; Leave point before replacement in case of
		     ;; recursive expansions.
		     (save-excursion (insert value)))
		    (finalize
		     (error "Undefined Org macro: %s; aborting"
			    (org-element-property :key macro)))))))))))

(defun org-macro-escape-arguments (&rest args)
  "Build macro's arguments string from ARGS.
ARGS are strings.  Return value is a string with arguments
properly escaped and separated with commas.  This is the opposite
of `org-macro-extract-arguments'."
  (let ((s ""))
    (dolist (arg (reverse args) (substring s 1))
      (setq s
	    (concat
	     ","
	     (replace-regexp-in-string
	      "\\(\\\\*\\),"
	      (lambda (m)
		(concat (make-string (1+ (* 2 (length (match-string 1 m)))) ?\\)
			","))
	      ;; If a non-terminal argument ends on backslashes, make
	      ;; sure to also escape them as they will be followed by
	      ;; a comma.
	      (concat arg (and (not (equal s ""))
			       (string-match "\\\\+\\'" arg)
			       (match-string 0 arg)))
	      nil t)
	     s)))))

(defun org-macro-extract-arguments (s)
  "Extract macro arguments from string S.
S is a string containing comma separated values properly escaped.
Return a list of arguments, as strings.  This is the opposite of
`org-macro-escape-arguments'."
  ;; Do not use `org-split-string' since empty strings are
  ;; meaningful here.
  (split-string
   (replace-regexp-in-string
    "\\(\\\\*\\),"
    (lambda (str)
      (let ((len (length (match-string 1 str))))
	(concat (make-string (/ len 2) ?\\)
		(if (zerop (mod len 2)) "\000" ","))))
    s nil t)
   "\000"))


(provide 'org-macro)
;;; org-macro.el ends here
