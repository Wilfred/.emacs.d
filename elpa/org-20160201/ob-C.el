;;; ob-C.el --- org-babel functions for C and similar languages

;; Copyright (C) 2010-2016 Free Software Foundation, Inc.

;; Author: Eric Schulte
;;      Thierry Banel
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

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

;; Org-Babel support for evaluating C, C++, D code.
;;
;; very limited implementation:
;; - currently only support :results output
;; - not much in the way of error feedback

;;; Code:
(require 'ob)
(require 'cc-mode)

(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))
(declare-function org-remove-indentation "org" (code &optional n))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("C++" . "cpp"))
(add-to-list 'org-babel-tangle-lang-exts '("D" . "d"))

(defvar org-babel-default-header-args:C '())

(defcustom org-babel-C-compiler "gcc"
  "Command used to compile a C source code file into an executable.
May be either a command in the path, like gcc
or an absolute path name, like /usr/local/bin/gcc
parameter may be used, like gcc -v"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defcustom org-babel-C++-compiler "g++"
  "Command used to compile a C++ source code file into an executable.
May be either a command in the path, like g++
or an absolute path name, like /usr/local/bin/g++
parameter may be used, like g++ -v"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defcustom org-babel-D-compiler "rdmd"
  "Command used to compile and execute a D source code file.
May be either a command in the path, like rdmd
or an absolute path name, like /usr/local/bin/rdmd
parameter may be used, like rdmd --chatty"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defvar org-babel-c-variant nil
  "Internal variable used to hold which type of C (e.g. C or C++ or D)
is currently being evaluated.")

(defun org-babel-execute:cpp (body params)
  "Execute BODY according to PARAMS.
This function calls `org-babel-execute:C++'."
  (org-babel-execute:C++ body params))

(defun org-babel-expand-body:cpp (body params)
  "Expand a block of C++ code with org-babel according to it's
header arguments."
  (org-babel-expand-body:C++ body params))

(defun org-babel-execute:C++ (body params)
  "Execute a block of C++ code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((org-babel-c-variant 'cpp)) (org-babel-C-execute body params)))

(defun org-babel-expand-body:C++ (body params)
  "Expand a block of C++ code with org-babel according to it's
header arguments."
  (let ((org-babel-c-variant 'cpp)) (org-babel-C-expand-C++ body params)))

(defun org-babel-execute:D (body params)
  "Execute a block of D code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((org-babel-c-variant 'd)) (org-babel-C-execute body params)))

(defun org-babel-expand-body:D (body params)
  "Expand a block of D code with org-babel according to it's
header arguments."
  (let ((org-babel-c-variant 'd)) (org-babel-C-expand-D body params)))

(defun org-babel-execute:C (body params)
  "Execute a block of C code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((org-babel-c-variant 'c)) (org-babel-C-execute body params)))

(defun org-babel-expand-body:C (body params)
  "Expand a block of C code with org-babel according to it's
header arguments."
  (let ((org-babel-c-variant 'c)) (org-babel-C-expand-C body params)))

(defun org-babel-C-execute (body params)
  "This function should only be called by `org-babel-execute:C'
or `org-babel-execute:C++' or `org-babel-execute:D'."
  (let* ((tmp-src-file (org-babel-temp-file
			"C-src-"
			(case org-babel-c-variant
			 (c   ".c"  )
			 (cpp ".cpp")
			 (d   ".d"  ))))
	 (tmp-bin-file (org-babel-temp-file "C-bin-" org-babel-exeext)) ;; not used for D
	 (cmdline (cdr (assoc :cmdline params)))
	 (cmdline (if cmdline (concat " " cmdline) ""))
	 (flags (cdr (assoc :flags params)))
	 (flags (mapconcat 'identity
			   (if (listp flags) flags (list flags)) " "))
	 (full-body
	  (case org-babel-c-variant
	    (c   (org-babel-C-expand-C   body params))
	    (cpp (org-babel-C-expand-C++ body params))
	    (d   (org-babel-C-expand-D   body params)))))
    (with-temp-file tmp-src-file (insert full-body))
    (case org-babel-c-variant
      ((c cpp)
       (org-babel-eval
	(format "%s -o %s %s %s"
		(case org-babel-c-variant
		 (c   org-babel-C-compiler)
		 (cpp org-babel-C++-compiler))
		(org-babel-process-file-name tmp-bin-file)
		flags
		(org-babel-process-file-name tmp-src-file)) ""))
      (d nil)) ;; no separate compilation for D
    (let ((results
	   (org-babel-eval
	    (case org-babel-c-variant
	      ((c cpp)
	       (concat tmp-bin-file cmdline))
	      (d
	       (format "%s %s %s %s"
		       org-babel-D-compiler
		       flags
		       (org-babel-process-file-name tmp-src-file)
		       cmdline)))
	    "")))
      (when results
	(setq results (org-babel-trim (org-remove-indentation results)))
	(org-babel-reassemble-table
	 (org-babel-result-cond (cdr (assoc :result-params params))
	   (org-babel-read results t)
	   (let ((tmp-file (org-babel-temp-file "c-")))
	     (with-temp-file tmp-file (insert results))
	     (org-babel-import-elisp-from-file tmp-file)))
	 (org-babel-pick-name
	  (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
	 (org-babel-pick-name
	  (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))
      )))

(defun org-babel-C-expand-C++ (body params)
  "Expand a block of C or C++ code with org-babel according to
it's header arguments."
  (org-babel-C-expand-C body params))

(defun org-babel-C-expand-C (body params)
  "Expand a block of C or C++ code with org-babel according to
it's header arguments."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
	(colnames (cdar (org-babel-get-header params :colname-names)))
	(main-p (not (string= (cdr (assoc :main params)) "no")))
	(includes (org-babel-read
		   (or (cdr (assoc :includes params))
		       (org-entry-get nil "includes" t))
		   nil))
	(defines (org-babel-read
		  (or (cdr (assoc :defines params))
		      (org-entry-get nil "defines" t))
		  nil)))
    (when (stringp includes)
      (setq includes (split-string includes)))
    (when (stringp defines)
      (let ((y nil)
	    (result (list t)))
	(dolist (x (split-string defines))
	  (if (null y)
	      (setq y x)
	    (nconc result (list (concat y " " x)))
	    (setq y nil)))
	(setq defines (cdr result))))
    (mapconcat 'identity
	       (list
		;; includes
		(mapconcat
		 (lambda (inc) (format "#include %s" inc))
		 includes "\n")
		;; defines
		(mapconcat
		 (lambda (inc) (format "#define %s" inc))
		 (if (listp defines) defines (list defines)) "\n")
		;; variables
		(mapconcat 'org-babel-C-var-to-C vars "\n")
		;; table sizes
		(mapconcat 'org-babel-C-table-sizes-to-C vars "\n")
		;; tables headers utility
		(when colnames
		  (org-babel-C-utility-header-to-C))
		;; tables headers
		(mapconcat 'org-babel-C-header-to-C colnames "\n")
		;; body
		(if main-p
		    (org-babel-C-ensure-main-wrap body)
		  body) "\n") "\n")))

(defun org-babel-C-expand-D (body params)
  "Expand a block of D code with org-babel according to
it's header arguments."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
	(colnames (cdar (org-babel-get-header params :colname-names)))
	(main-p (not (string= (cdr (assoc :main params)) "no")))
	(imports (or (cdr (assoc :imports params))
		     (org-babel-read (org-entry-get nil "imports" t)))))
    (when (stringp imports)
      (setq imports (split-string imports)))
    (setq imports (append imports '("std.stdio" "std.conv")))
    (mapconcat 'identity
	       (list
		"module mmm;"
		;; imports
		(mapconcat
		 (lambda (inc) (format "import %s;" inc))
		 imports "\n")
		;; variables
		(mapconcat 'org-babel-C-var-to-C vars "\n")
		;; table sizes
		(mapconcat 'org-babel-C-table-sizes-to-C vars "\n")
		;; tables headers utility
		(when colnames
		  (org-babel-C-utility-header-to-C))
		;; tables headers
		(mapconcat 'org-babel-C-header-to-C colnames "\n")
		;; body
		(if main-p
		    (org-babel-C-ensure-main-wrap body)
		  body) "\n") "\n")))

(defun org-babel-C-ensure-main-wrap (body)
  "Wrap BODY in a \"main\" function call if none exists."
  (if (string-match "^[ \t]*[intvod]+[ \t\n\r]*main[ \t]*(.*)" body)
      body
    (format "int main() {\n%s\nreturn 0;\n}\n" body)))

(defun org-babel-prep-session:C (session params)
  "This function does nothing as C is a compiled language with no
support for sessions"
  (error "C is a compiled languages -- no support for sessions"))

(defun org-babel-load-session:C (session body params)
  "This function does nothing as C is a compiled language with no
support for sessions"
  (error "C is a compiled languages -- no support for sessions"))

;; helper functions

(defun org-babel-C-format-val (type val)
  "Handle the FORMAT part of TYPE with the data from VAL."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
	(cons "" (format format-data val))
      (funcall format-data val))))

(defun org-babel-C-val-to-C-type (val)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL."
  (let* ((basetype (org-babel-C-val-to-base-type val))
	 (type
	  (case basetype
	    (integerp '("int" "%d"))
	    (floatp '("double" "%f"))
	    (stringp
	     (list
	      (if (equal org-babel-c-variant 'd) "string" "const char*")
	      "\"%s\""))
	    (t (error "unknown type %S" basetype)))))
    (cond
     ((integerp val) type) ;; an integer declared in the #+begin_src line
     ((floatp val) type) ;; a numeric declared in the #+begin_src line
     ((and (listp val) (listp (car val))) ;; a table
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "[%d][%d]" (length val) (length (car val)))
	   (concat
	    (if (equal org-babel-c-variant 'd) "[\n" "{\n")
	    (mapconcat
	     (lambda (v)
	       (concat
		(if (equal org-babel-c-variant 'd) " [" " {")
		(mapconcat (lambda (w) (format ,(cadr type) w)) v ",")
		(if (equal org-babel-c-variant 'd) "]" "}")))
	     val
	     ",\n")
	    (if (equal org-babel-c-variant 'd) "\n]" "\n}"))))))
     ((or (listp val) (vectorp val)) ;; a list declared in the #+begin_src line
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "[%d]" (length val))
	   (concat
	    (if (equal org-babel-c-variant 'd) "[" "{")
	    (mapconcat (lambda (v) (format ,(cadr type) v)) val ",")
	    (if (equal org-babel-c-variant 'd) "]" "}"))))))
     (t ;; treat unknown types as string
      type))))

(defun org-babel-C-val-to-base-type (val)
  "Determine the base type of VAL which may be
`integerp' if all base values are integers
`floatp' if all base values are either floating points or integers
`stringp' otherwise."
  (cond
   ((integerp val) 'integerp)
   ((floatp val) 'floatp)
   ((or (listp val) (vectorp val))
    (let ((type nil))
      (mapc (lambda (v)
	      (case (org-babel-C-val-to-base-type v)
		(stringp (setq type 'stringp))
		(floatp
		 (if (or (not type) (eq type 'integerp))
		     (setq type 'floatp)))
		(integerp
		 (unless type (setq type 'integerp)))))
	    val)
      type))
   (t 'stringp)))

(defun org-babel-C-var-to-C (pair)
  "Convert an elisp val into a string of C code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
	(val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
	(setq val (string-to-char val))))
    (let* ((type-data (org-babel-C-val-to-C-type val))
	   (type (car type-data))
	   (formated (org-babel-C-format-val type-data val))
	   (suffix (car formated))
	   (data (cdr formated)))
      (format "%s %s%s = %s;"
	      type
	      var
	      suffix
	      data))))

(defun org-babel-C-table-sizes-to-C (pair)
  "Create constants of table dimensions, if PAIR is a table."
  (when (listp (cdr pair))
    (cond
     ((listp (cadr pair)) ;; a table
      (concat
       (format "const int %s_rows = %d;" (car pair) (length (cdr pair)))
       "\n"
       (format "const int %s_cols = %d;" (car pair) (length (cadr pair)))))
     (t ;; a list declared in the #+begin_src line
      (format "const int %s_cols = %d;" (car pair) (length (cdr pair)))))))

(defun org-babel-C-utility-header-to-C ()
  "Generate a utility function to convert a column name
into a column number."
  (case org-babel-c-variant
    ((c cpp)
     "int get_column_num (int nbcols, const char** header, const char* column)
{
  int c;
  for (c=0; c<nbcols; c++)
    if (strcmp(header[c],column)==0)
      return c;
  return -1;
}
"
     )
    (d
     "int get_column_num (string[] header, string column)
{
  foreach (c, h; header)
    if (h==column)
      return to!int(c);
  return -1;
}
"
     )))

(defun org-babel-C-header-to-C (head)
  "Convert an elisp list of header table into a C or D vector
specifying a variable with the name of the table."
  (let ((table (car head))
        (headers (cdr head)))
    (concat
     (format
      (case org-babel-c-variant
	((c cpp) "const char* %s_header[%d] = {%s};")
	(d       "string %s_header[%d] = [%s];"))
      table
      (length headers)
      (mapconcat (lambda (h) (format "%S" h)) headers ","))
     "\n"
     (case org-babel-c-variant
       ((c cpp)
	(format
	 "const char* %s_h (int row, const char* col) { return %s[row][get_column_num(%d,%s_header,col)]; }"
	 table table (length headers) table))
       (d
	(format
	 "string %s_h (ulong row, string col) { return %s[row][get_column_num(%s_header,col)]; }"
	 table table table))))))

(provide 'ob-C)

;;; ob-C.el ends here
