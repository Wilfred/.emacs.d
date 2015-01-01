;; jabber-xml.el - XML functions

;; Copyright (C) 2003, 2004, 2007, 2008 - Magnus Henoch - mange@freemail.hu
;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'xml)
(require 'jabber-util)
(eval-when-compile
  (require 'cl))

(defun jabber-escape-xml (str)
  "escape strings for xml"
  (if (stringp str)
      (let ((newstr (concat str)))
	;; Form feeds might appear in code you copy, etc.  Nevertheless,
	;; it's invalid XML.
	(setq newstr (jabber-replace-in-string newstr "\f" "\n"))
	;; Other control characters are also illegal, except for
	;; tab, CR, and LF.
	(setq newstr (jabber-replace-in-string newstr "[\000-\010\013\014\016-\037]" " "))
	(setq newstr (jabber-replace-in-string newstr "&" "&amp;"))
	(setq newstr (jabber-replace-in-string newstr "<" "&lt;"))
	(setq newstr (jabber-replace-in-string newstr ">" "&gt;"))
	(setq newstr (jabber-replace-in-string newstr "'" "&apos;"))
	(setq newstr (jabber-replace-in-string newstr "\"" "&quot;"))
	newstr)
    str))

(defun jabber-unescape-xml (str)
  "unescape xml strings"
  ;; Eventually this can be done with `xml-substitute-special', but the
  ;; version in xml.el of GNU Emacs 21.3 is buggy.
  (if (stringp str)
      (let ((newstr str))
	(setq newstr (jabber-replace-in-string newstr "&quot;" "\""))
	(setq newstr (jabber-replace-in-string newstr "&apos;" "'"))
	(setq newstr (jabber-replace-in-string newstr "&gt;" ">"))
	(setq newstr (jabber-replace-in-string newstr "&lt;" "<"))
	(setq newstr (jabber-replace-in-string newstr "&amp;" "&"))
	newstr)
    str))

(defun jabber-sexp2xml (sexp)
  "converts an SEXP in the format (tagname ((attribute-name . attribute-value)...) children...) and converts it to well-formatted xml."
  (cond
   ((stringp sexp)
    (jabber-escape-xml sexp))
   ((listp (car sexp))
    (let ((xml ""))
      (dolist (tag sexp)
	(setq xml (concat xml (jabber-sexp2xml tag))))
      xml))
   ;; work around bug in old versions of xml.el, where ("") can appear
   ;; as children of a node
   ((and (consp sexp)
	 (stringp (car sexp))
	 (zerop (length (car sexp))))
    "")
   (t
    (let ((xml ""))
      (setq xml (concat "<" 
			(symbol-name (car sexp))))
      (dolist (attr (cadr sexp))
	(if (consp attr)
	    (setq xml (concat xml
			      (format " %s='%s'"
				      (symbol-name (car attr))
				      (jabber-escape-xml (cdr attr)))))))
      (if (cddr sexp)
	  (progn
	    (setq xml (concat xml ">"))
	    (dolist (child (cddr sexp))
	      (setq xml (concat xml
				(jabber-sexp2xml child))))
	    (setq xml (concat xml
			      "</"
			      (symbol-name (car sexp))
			      ">")))
	(setq xml (concat xml
			  "/>")))
      xml))))

(defun jabber-xml-skip-tag-forward (&optional dont-recurse-into-stream)
  "Skip to end of tag or matching closing tag if present.
Return t iff after a closing tag, otherwise throws an 'unfinished
tag with value nil.
If DONT-RECURSE-INTO-STREAM is true, stop after an opening
<stream:stream> tag.

The version of `sgml-skip-tag-forward' in Emacs 21 isn't good
enough for us."
  (skip-chars-forward "^<")
  (cond
   ((looking-at "<!\\[CDATA\\[")
    (if (search-forward "]]>" nil t)
	(goto-char (match-end 0))
      (throw 'unfinished nil)))
   ((looking-at "<\\([^ \t\n/>]+\\)\\([ \t\n]+[^=]+='[^']*'\\|[ \t\n]+[^=]+=\"[^\"]*\"\\)*")
    (let ((node-name (match-string 1)))
      (goto-char (match-end 0))
      (cond
       ((looking-at "/>")
	(goto-char (match-end 0))
	t)
       ((looking-at ">")
	(forward-char 1)
	(unless (and dont-recurse-into-stream (equal node-name "stream:stream"))
	  (loop 
	   do (skip-chars-forward "^<")
	   until (looking-at (regexp-quote (concat "</" node-name ">")))
	   do (jabber-xml-skip-tag-forward))
	  (goto-char (match-end 0)))
	t)
       (t
	(throw 'unfinished nil)))))
   (t
    (throw 'unfinished nil))))

(defsubst jabber-xml-node-name (node)
  "Return the tag associated with NODE.
The tag is a lower-case symbol."
  (if (listp node) (car node)))

(defsubst jabber-xml-node-attributes (node)
  "Return the list of attributes of NODE.
The list can be nil."
  (if (listp node) (nth 1 node)))

(defsubst jabber-xml-node-children (node)
  "Return the list of children of NODE.
This is a list of nodes, and it can be nil."
  (let ((children (cddr node)))
    ;; Work around a bug in early versions of xml.el
    (if (equal children '(("")))
	nil
      children)))

(defun jabber-xml-get-children (node child-name)
  "Return the children of NODE whose tag is CHILD-NAME.
CHILD-NAME should be a lower case symbol."
  (let ((match ()))
    (dolist (child (jabber-xml-node-children node))
      (if child
	  (if (equal (jabber-xml-node-name child) child-name)
	      (push child match))))
    (nreverse match)))

;; `xml-get-attribute' returns "" if the attribute is not found, which
;; is not very useful.  Therefore, we use `xml-get-attribute-or-nil'
;; if present, or emulate its behavior.
(eval-and-compile
  (if (fboundp 'xml-get-attribute-or-nil)
      (defsubst jabber-xml-get-attribute (node attribute)
	"Get from NODE the value of ATTRIBUTE.
Return nil if the attribute was not found."
	(when (consp node)
	  (xml-get-attribute-or-nil node attribute)))
    (defsubst jabber-xml-get-attribute (node attribute)
      "Get from NODE the value of ATTRIBUTE.
Return nil if the attribute was not found."
      (when (consp node)
	(let ((result (xml-get-attribute node attribute)))
	  (and (> (length result) 0) result))))))

(defsubst jabber-xml-get-xmlns (node)
  "Get \"xmlns\" attribute of NODE, or nil if not present."
  (jabber-xml-get-attribute node 'xmlns))

(defun jabber-xml-path (xml-data path)
  "Find sub-node of XML-DATA according to PATH.
PATH is a vaguely XPath-inspired list.  Each element can be:

a symbol     go to first child node with this node name
cons cell    car is string containing namespace URI,
             cdr is string containing node name.  Find
             first matching child node.
any string   character data of this node"
  (let ((node xml-data))
    (while (and path node)
      (let ((step (car path)))
	(cond
	 ((symbolp step)
	  (setq node (car (jabber-xml-get-children node step))))
	 ((consp step)
	  ;; This will be easier with namespace-aware use
	  ;; of xml.el.  It will also be more correct.
	  ;; Now, it only matches explicit namespace declarations.
	  (setq node
		(dolist (x (jabber-xml-get-children node (intern (cdr step))))
		  (when (string= (jabber-xml-get-attribute x 'xmlns)
				 (car step))
		    (return x)))))
	 ((stringp step)
	  (setq node (car (jabber-xml-node-children node)))
	  (unless (stringp node)
	    (setq node nil)))
	 (t
	  (error "Unknown path step: %s" step))))
      (setq path (cdr path)))
    node))

(defmacro jabber-xml-let-attributes (attributes xml-data &rest body)
  "Bind variables to the same-name attribute values in XML-DATA."
  `(let ,(mapcar #'(lambda (attr)
		     (list attr `(jabber-xml-get-attribute ,xml-data ',attr)))
		 attributes)
     ,@body))
(put 'jabber-xml-let-attributes 'lisp-indent-function 2)

(defun jabber-xml-resolve-namespace-prefixes (xml-data &optional default-ns prefixes)
  (let ((node-name (jabber-xml-node-name xml-data))
	(attrs (jabber-xml-node-attributes xml-data)))
    (setq prefixes (jabber-xml-merge-namespace-declarations attrs prefixes))

    ;; If there is an xmlns attribute, it is the new default
    ;; namespace.
    (let ((xmlns (jabber-xml-get-xmlns xml-data)))
      (when xmlns
	(setq default-ns xmlns)))
    ;; Now, if the node name has a prefix, replace it and add an
    ;; "xmlns" attribute.  Slightly ugly, but avoids the need to
    ;; change all the rest of jabber.el at once.
    (let ((node-name-string (symbol-name node-name)))
      (when (string-match "\\(.*\\):\\(.*\\)" node-name-string)
	(let* ((prefix (match-string 1 node-name-string))
	       (unprefixed (match-string 2 node-name-string))
	       (ns (assoc prefix prefixes)))
	  (if (null ns)
	      ;; This is not supposed to happen...
	      (message "jabber-xml-resolve-namespace-prefixes: Unknown prefix in %s" node-name-string)
	    (setf (car xml-data) (intern unprefixed))
	    (setf (cadr xml-data) (cons (cons 'xmlns (cdr ns)) (delq 'xmlns attrs)))))))
    ;; And iterate through all child elements.
    (mapc (lambda (x) 
	    (when (listp x)
	      (jabber-xml-resolve-namespace-prefixes x default-ns prefixes)))
	  (jabber-xml-node-children xml-data))
    xml-data))

(defun jabber-xml-merge-namespace-declarations (attrs prefixes)
  ;; First find any xmlns:foo attributes..
  (dolist (attr attrs)
    (let ((attr-name (symbol-name (car attr))))
      (when (string-match "xmlns:" attr-name)
	(let ((prefix (substring attr-name (match-end 0)))
	      (ns-uri (cdr attr)))
	  ;; A slightly complicated dance to never change the
	  ;; original value of prefixes (since the caller depends on
	  ;; it), but also to avoid excessive copying (which remove
	  ;; always does).  Might need to profile and tweak this for
	  ;; performance.
	  (setq prefixes
		(cons (cons prefix ns-uri)
			(if (assoc prefix prefixes)
			    (remove (assoc prefix prefixes) prefixes)
			  prefixes)))))))
  prefixes)  

(provide 'jabber-xml)

;;; arch-tag: ca206e65-7026-4ee8-9af2-ff6a9c5af98a
