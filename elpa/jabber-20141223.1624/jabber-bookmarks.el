;; jabber-bookmarks.el - bookmarks according to XEP-0048

;; Copyright (C) 2007, 2008 - Magnus Henoch - mange@freemail.hu

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

(require 'jabber-private)
(require 'jabber-widget)

(require 'cl)

(defvar jabber-bookmarks (make-hash-table :test 'equal)
  "Mapping from full JIDs to bookmarks.
Bookmarks are what has been retrieved from the server, as list of
XML elements.  This is nil if bookmarks have not been retrieved,
and t if no bookmarks where found.")

;;;###autoload
(defun jabber-get-conference-data (jc conference-jid cont &optional key)
  "Get bookmark data for CONFERENCE-JID.
KEY may be nil or one of :name, :autojoin, :nick and :password.
If KEY is nil, a plist containing the above keys is returned.
CONT is called when the result is available, with JC and the
result as arguments.  If CONT is nil, return the requested data
immediately, and return nil if it is not in the cache."
  (if (null cont)
      (let ((cache (jabber-get-bookmarks-from-cache jc)))
       (if (and cache (listp cache))
        (jabber-get-conference-data-internal
         cache conference-jid key)))
    (jabber-get-bookmarks 
     jc
     (lexical-let ((conference-jid conference-jid)
		   (key key)
		   (cont cont))
       (lambda (jc result)
	 (let ((entry (jabber-get-conference-data-internal result conference-jid key)))
	   (funcall cont jc entry)))))))

(defun jabber-get-conference-data-internal (result conference-jid key)
  (let ((entry (dolist (node result)
		(when (and (eq (jabber-xml-node-name node) 'conference)
			   (string= (jabber-xml-get-attribute node 'jid) conference-jid))
		  (return (jabber-parse-conference-bookmark node))))))
    (if key
	(plist-get entry key)
      entry)))

;;;###autoload
(defun jabber-parse-conference-bookmark (node)
  "Convert a <conference/> tag into a plist.
The plist may contain the keys :jid, :name, :autojoin,
:nick and :password."
  (when (eq (jabber-xml-node-name node) 'conference)
    (list :jid (jabber-xml-get-attribute node 'jid)
	  :name (jabber-xml-get-attribute node 'name)
	  :autojoin (member (jabber-xml-get-attribute node 'autojoin)
			    '("true" "1"))
	  :nick (car (jabber-xml-node-children
		      (car (jabber-xml-get-children node 'nick))))
	  :password (car (jabber-xml-node-children
			  (car (jabber-xml-get-children node 'password)))))))

;;;###autoload
(defun jabber-get-bookmarks (jc cont &optional refresh)
  "Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and the bookmark list.  CONT will be
called as the result of a filter function or a timer.
If REFRESH is non-nil, always fetch bookmarks."
  (let ((bookmarks (gethash (jabber-connection-bare-jid jc) jabber-bookmarks)))
    (if (and (not refresh) bookmarks)
	(run-with-timer 0 nil cont jc (when (listp bookmarks) bookmarks))
      (lexical-let* ((cont cont)
		     (callback (lambda (jc result) (jabber-get-bookmarks-1 jc result cont))))
	(jabber-private-get jc 'storage "storage:bookmarks"
			    callback callback)))))

(defun jabber-get-bookmarks-1 (jc result cont)
  (let ((my-jid (jabber-connection-bare-jid jc))
	(value 
	 (if (eq (jabber-xml-node-name result) 'storage)
	     (or (jabber-xml-node-children result) t)
	   t)))
    (puthash my-jid value jabber-bookmarks)
    (funcall cont jc (when (listp value) value))))

;;;###autoload
(defun jabber-get-bookmarks-from-cache (jc)
  "Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil."
  (gethash (jabber-connection-bare-jid jc) jabber-bookmarks))

(defun jabber-set-bookmarks (jc bookmarks &optional callback)
  "Set bookmarks to BOOKMARKS, which is a list of XML elements.
If CALLBACK is non-nil, call it with JC and t or nil as arguments
on success or failure, respectively."
  (unless callback
    (setq callback #'ignore))
  (jabber-private-set
   jc
   `(storage ((xmlns . "storage:bookmarks"))
	     ,@bookmarks)
   callback t
   callback nil))

;;;###autoload
(defun jabber-edit-bookmarks (jc)
  "Create a buffer for editing bookmarks interactively."
  (interactive (list (jabber-read-account)))
  (jabber-get-bookmarks jc 'jabber-edit-bookmarks-1 t))

(defun jabber-edit-bookmarks-1 (jc bookmarks)
  (setq bookmarks
	(mapcar
	 (lambda (e)
	   (case (jabber-xml-node-name e)
	     (url
	      (list 'url (or (jabber-xml-get-attribute e 'url) "")
		    (or (jabber-xml-get-attribute e 'name) "")))
	     (conference
	      (list 'conference
		    (or (jabber-xml-get-attribute e 'jid) "")
		    (or (jabber-xml-get-attribute e 'name) "")
		    (not (not (member (jabber-xml-get-attribute e 'autojoin)
				      '("true" "1"))))
		    (or (jabber-xml-path e '(nick "")) "")
		    (or (jabber-xml-path e '(password "")) "")))))
	 bookmarks))
  (setq bookmarks (delq nil bookmarks))
  (with-current-buffer (get-buffer-create "Edit bookmarks")
    (jabber-init-widget-buffer nil)
    (setq jabber-buffer-connection jc)
    
    (widget-insert (jabber-propertize (concat "Edit bookmarks for "
					      (jabber-connection-bare-jid jc))
				      'face 'jabber-title-large)
		   "\n\n")

    (when (or (bound-and-true-p jabber-muc-autojoin)
	      (bound-and-true-p jabber-muc-default-nicknames))
      (widget-insert "The variables `jabber-muc-autojoin' and/or `jabber-muc-default-nicknames'\n"
		     "contain values.  They are only available to jabber.el on this machine.\n"
		     "You may want to import them into your bookmarks, to make them available\n"
		     "to any client on any machine.\n")
      (widget-create 'push-button :notify 'jabber-bookmarks-import "Import values from variables")
      (widget-insert "\n\n"))

    (push (cons 'bookmarks
		(widget-create
		 '(repeat 
		   :tag "Bookmarks"
		   (choice
		    (list :tag "Conference"
			  (const :format "" conference)
			  (string :tag "JID") ;XXX: jid widget type?
			  (string :tag "Name")
			  (checkbox :tag "Autojoin" :format "%[%v%] Autojoin?\n")
			  (string :tag "Nick")	      ;or nil?
			  (string :tag "Password")    ;or nil?
			  )
		    (list :tag "URL"
			  (const :format "" url)
			  (string :tag "URL")
			  (string :tag "Name"))))
		 :value bookmarks))
	  jabber-widget-alist)

    (widget-insert "\n")
    (widget-create 'push-button :notify 'jabber-bookmarks-submit "Submit")

    (widget-setup)
    (widget-minor-mode 1)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))

(defun jabber-bookmarks-submit (&rest ignore)
  (let ((bookmarks (widget-value (cdr (assq 'bookmarks jabber-widget-alist)))))
    (setq bookmarks
	  (mapcar
	   (lambda (entry)
	     (case (car entry)
	       (url
		(destructuring-bind (symbol url name) entry
		  `(url ((url . ,url)
			 (name . ,name)))))
	       (conference
		(destructuring-bind (symbol jid name autojoin nick password)
		    entry
		  `(conference ((jid . ,jid)
				(name . ,name)
				(autojoin . ,(if autojoin
						 "1"
					       "0")))
			       ,@(unless (zerop (length nick))
				   `((nick () ,nick)))
			       ,@(unless (zerop (length password))
				   `((password () ,password))))))))
	   bookmarks))
    (remhash (jabber-connection-bare-jid jabber-buffer-connection) jabber-bookmarks)
    (jabber-private-set
     jabber-buffer-connection
     `(storage ((xmlns . "storage:bookmarks"))
	       ,@bookmarks)
     'jabber-report-success "Storing bookmarks"
     'jabber-report-success "Storing bookmarks")))

(defun jabber-bookmarks-import (&rest ignore)
  (let* ((value (widget-value (cdr (assq 'bookmarks jabber-widget-alist))))
	 (conferences (mapcar 
		       'cdr
		       (remove-if-not
			(lambda (entry)
			  (eq (car entry) 'conference))
			value))))
    (dolist (default-nickname jabber-muc-default-nicknames)
      (destructuring-bind (muc-jid . nick) default-nickname
	(let ((entry (assoc muc-jid conferences)))
	  (if entry
	      (setf (fourth entry) nick)
	    (setq entry (list muc-jid "" nil nick ""))
	    (push entry conferences)
	    (push (cons 'conference entry) value)))))
    (dolist (autojoin jabber-muc-autojoin)
      (let ((entry (assoc autojoin conferences)))
	(if entry
	    (setf (third entry) t)
	  (setq entry (list autojoin "" t "" ""))
	  (push (cons 'conference entry) value))))
    (widget-value-set (cdr (assq 'bookmarks jabber-widget-alist)) value)
    (widget-setup)))

(provide 'jabber-bookmarks)
;; arch-tag: a7d6f862-bac0-11db-831f-000a95c2fcd0
