;;; jabber-vcard.el --- vcards according to JEP-0054

;; Copyright (C) 2005, 2007  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; There are great variations in Jabber vcard implementations.  This
;; one adds some spice to the mix, while trying to follow the JEP
;; closely.

;; Fields not implemented: GEO, LOGO, AGENT, ORG, CATEGORIES, SOUND,
;; CLASS, KEY.

;; The internal data structure used for vCards is an alist.  All
;; keys are uppercase symbols.
;;
;; FN, NICKNAME, BDAY, JABBERID, MAILER, TZ, TITLE, ROLE, NOTE,
;; PRODID, REV, SORT-STRING, UID, URL, DESC:
;; Value is a string.
;;
;; N:
;;   Value is an alist, with keys FAMILY, GIVEN, MIDDLE, PREFIX and SUFFIX.
;;
;; ADR:
;;   Value is a list, each element representing a separate address.
;;   The car of each address is a list of types; possible values are
;;   HOME, WORK, POSTAL, PARCEL, DOM, INTL, PREF.
;;   The cdr of each address is an alist, with keys POBOX, EXTADD,
;;   STREET, LOCALITY, REGION, PCODE, CTRY, and values being strings.
;;
;; TEL:
;;   Value is a list, each element representing a separate phone number.
;;   The car of each number is a list of types; possible values are
;;   HOME, WORK, VOICE, FAX, PAGER, MSG, CELL, VIDEO, BBS, MODEM, ISDN,
;;   PCS, PREF
;;   The cdr is the phone number as a string.
;;
;; EMAIL:
;;   Value is a list, each element representing a separate e-mail address.
;;   The car of each address is a list of types; possible values are
;;   HOME, WORK, INTERNET, PREF, X400.  At least one of INTERNET and
;;   X400 is always present.
;;   The cdr is the address as a string.

;;; Code:

(require 'jabber-core)
(require 'jabber-widget)
(require 'jabber-iq)
(require 'jabber-avatar)

(defvar jabber-vcard-photo nil
  "The avatar structure for the photo in the vCard edit buffer.")
(make-variable-buffer-local 'jabber-vcard-photo)

(defun jabber-vcard-parse (vcard)
  "Parse the vCard XML structure given in VCARD.
The top node should be the `vCard' node."
  ;; Hm... stpeter has a <query/> as top node...
  ;;(unless (eq (jabber-xml-node-name vcard) 'vCard)
  ;;  (error "Invalid vCard"))
  (let (result)
    (dolist (verbatim-node '(FN NICKNAME BDAY JABBERID MAILER TZ
				 TITLE ROLE NOTE PRODID REV SORT-STRING
				 UID URL DESC))
      ;; There should only be one of each of these.  They are
      ;; used verbatim.
      (let ((node (car (jabber-xml-get-children vcard
						verbatim-node))))
	;; Some clients include the node, but without data
	(when (car (jabber-xml-node-children node))
	  (push (cons (jabber-xml-node-name node)
		      (car (jabber-xml-node-children node)))
		result))))

    ;; Name components
    (let ((node (car (jabber-xml-get-children vcard 'N))))
      ;; Subnodes are FAMILY, GIVEN, MIDDLE, PREFIX, SUFFIX
      (push (cons 'N
		  (let (name)
		    (dolist (subnode (jabber-xml-node-children node))
		      (when (and (memq (jabber-xml-node-name subnode)
				       '(FAMILY GIVEN MIDDLE PREFIX SUFFIX))
				 (not (zerop (length
					      (car (jabber-xml-node-children
						    subnode))))))
			(push (cons (jabber-xml-node-name subnode)
				    (car (jabber-xml-node-children
					  subnode)))
			      name)))
		    name))
	    result))

    ;; There can be several addresses
    (let (addresses)
      (dolist (adr (jabber-xml-get-children vcard 'ADR))
	;; Find address type(s)
	(let (types)
	  (dolist (possible-type '(HOME WORK POSTAL PARCEL DOM INTL PREF))
	    (when (jabber-xml-get-children adr possible-type)
	      (push possible-type types)))

	  (let (components)
	    (dolist (component (jabber-xml-node-children adr))
	      (when (and (memq (jabber-xml-node-name component)
			       '(POBOX EXTADD STREET LOCALITY REGION
				       PCODE CTRY))
			 (not (zerop (length
				      (car (jabber-xml-node-children
					    component))))))
		(push (cons (jabber-xml-node-name component)
			    (car (jabber-xml-node-children component)))
		      components)))

	    (push (cons types components) addresses))))

      (when addresses
	(push (cons 'ADR addresses) result)))

    ;; Likewise for phone numbers
    (let (phone-numbers)
      (dolist (tel (jabber-xml-get-children vcard 'TEL))
	;; Find phone type(s)
	(let ((number (car (jabber-xml-node-children
			    (car (jabber-xml-get-children tel 'NUMBER)))))
	      types)
	  ;; Some clients put no NUMBER node.  Avoid that.
	  (when number
	    (dolist (possible-type '(HOME WORK VOICE FAX PAGER MSG CELL
					  VIDEO BBS MODEM ISDN PCS PREF))
	      (when (jabber-xml-get-children tel possible-type)
		(push possible-type types)))
	    
	    (push (cons types number) phone-numbers))))
	
      (when phone-numbers
	(push (cons 'TEL phone-numbers) result)))

    ;; And for e-mail addresses
    (let (e-mails)
      (dolist (email (jabber-xml-get-children vcard 'EMAIL))
	(let ((userid (car (jabber-xml-node-children
			    (car (jabber-xml-get-children email 'USERID)))))
	      types)
	  ;; Some clients put no USERID node.  Avoid that.
	  (when userid
	    (dolist (possible-type '(HOME WORK INTERNET PREF X400))
	      (when (jabber-xml-get-children email possible-type)
		(push possible-type types)))
	    (unless (or (memq 'INTERNET types)
			(memq 'X400 types))
	      (push 'INTERNET types))
	    
    (push (cons types userid) e-mails))))

      (when e-mails
	(push (cons 'EMAIL e-mails) result)))

    ;; JEP-0153: vCard-based avatars
    (let ((photo-tag (car (jabber-xml-get-children vcard 'PHOTO))))
      (when photo-tag
	(let ((type (jabber-xml-path photo-tag '(TYPE "")))
	      (binval (jabber-xml-path photo-tag '(BINVAL ""))))
	  (when (and type binval)
	    (push (list 'PHOTO type binval) result)))))

    result))

(defun jabber-vcard-reassemble (parsed)
  "Create a vCard XML structure from PARSED."
  ;; Save photo in jabber-vcard-photo, to avoid excessive processing.
  (let ((photo (cdr (assq 'PHOTO parsed))))
    (cond
     ;; No photo
     ((null photo)
      (setq jabber-vcard-photo nil))
     ;; Existing photo
     ((listp photo)
      (setq jabber-vcard-photo 
	    (jabber-avatar-from-base64-string 
	     (nth 1 photo) (nth 0 photo))))
     ;; New photo from file
     (t
      (access-file photo "Avatar file not found")
      ;; Maximum allowed size is 8 kilobytes
      (when (> (nth 7 (file-attributes photo)) 8192)
	(error "Avatar bigger than 8 kilobytes"))
      (setq jabber-vcard-photo (jabber-avatar-from-file photo)))))

  `(vCard ((xmlns . "vcard-temp"))
	  ;; Put in simple fields
	  ,@(mapcar
	     (lambda (field)
	       (when (and (assq (car field) jabber-vcard-fields)
			  (not (zerop (length (cdr field)))))
		 (list (car field) nil (cdr field))))
	     parsed)
	  ;; Put in decomposited name
	  (N nil
	     ,@(mapcar
		(lambda (name-part)
		  (when (not (zerop (length (cdr name-part))))
		    (list (car name-part) nil (cdr name-part))))
		(cdr (assq 'N parsed))))
	  ;; Put in addresses
	  ,@(mapcar
	     (lambda (address)
	       (append '(ADR) '(())
		       (mapcar 'list (nth 0 address))
		       (mapcar (lambda (field)
				 (list (car field) nil (cdr field)))
			       (cdr address))))
	     (cdr (assq 'ADR parsed)))
	  ;; Put in phone numbers
	  ,@(mapcar
	     (lambda (phone)
	       (append '(TEL) '(())
		       (mapcar 'list (car phone))
		       (list (list 'NUMBER nil (cdr phone)))))
	     (cdr (assq 'TEL parsed)))
	  ;; Put in e-mail addresses
	  ,@(mapcar
	     (lambda (email)
	       (append '(EMAIL) '(())
		       (mapcar 'list (car email))
		       (list (list 'USERID nil (cdr email)))))
	     (cdr (assq 'EMAIL parsed)))
	  ;; Put in photo
	  ,@(when jabber-vcard-photo
	      `((PHOTO ()
		       (TYPE () ,(avatar-mime-type jabber-vcard-photo))
		       (BINVAL () ,(avatar-base64-data jabber-vcard-photo)))))))
		     
(add-to-list 'jabber-jid-info-menu
	     (cons "Request vcard" 'jabber-vcard-get))

(defun jabber-vcard-get (jc jid)
  "Request vcard from JID."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Request vcard from: " nil nil nil 'bare-or-muc)))
  (jabber-send-iq jc jid
		  "get"
		  '(vCard ((xmlns . "vcard-temp")))
		  #'jabber-process-data #'jabber-vcard-display
		  #'jabber-process-data "Vcard request failed"))

(defun jabber-vcard-edit (jc)
  "Edit your own vcard."
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc nil
		  "get"
		  '(vCard ((xmlns . "vcard-temp")))
		  #'jabber-vcard-do-edit nil
		  #'jabber-report-success "Vcard request failed"))

(defconst jabber-vcard-fields '((FN . "Full name")
				(NICKNAME . "Nickname")
				(BDAY . "Birthday")
				(URL . "URL")
				(JABBERID . "JID")
				(MAILER . "User agent")
				(TZ . "Time zone")
				(TITLE . "Title")
				(ROLE . "Role")
				(REV . "Last changed")
				(DESC . "Description")
				(NOTE . "Note")))

(defconst jabber-vcard-name-fields '((PREFIX . "Prefix")
				     (GIVEN . "Given name")
				     (MIDDLE . "Middle name")
				     (FAMILY . "Family name")
				     (SUFFIX . "Suffix")))

(defconst jabber-vcard-phone-types '((HOME . "Home")
				     (WORK . "Work")
				     (VOICE . "Voice")
				     (FAX . "Fax")
				     (PAGER . "Pager")
				     (MSG . "Message")
				     (CELL . "Cell phone")
				     (VIDEO . "Video")
				     (BBS . "BBS")
				     (MODEM . "Modem")
				     (ISDN . "ISDN")
				     (PCS . "PCS")))

(defconst jabber-vcard-email-types '((HOME . "Home")
				     (WORK . "Work")
				     (INTERNET . "Internet")
				     (X400 . "X400")
				     (PREF . "Preferred")))

(defconst jabber-vcard-address-types '((HOME . "Home")
				       (WORK . "Work")
				       (POSTAL . "Postal")
				       (PARCEL . "Parcel")
				       (DOM . "Domestic")
				       (INTL . "International")
				       (PREF . "Preferred")))

(defconst jabber-vcard-address-fields '((POBOX . "Post box")
					(EXTADD . "Ext. address")
					(STREET . "Street")
					(LOCALITY . "Locality")
					(REGION . "Region")
					(PCODE . "Post code")
					(CTRY . "Country")))

(defun jabber-vcard-display (jc xml-data)
  "Display received vcard."
  (let ((parsed (jabber-vcard-parse (jabber-iq-query xml-data))))
    (dolist (simple-field jabber-vcard-fields)
      (let ((field (assq (car simple-field) parsed)))
	(when field
	  (insert (cdr simple-field))
	  (indent-to 20)
	  (insert (cdr field) "\n"))))

    (let ((names (cdr (assq 'N parsed))))
      (when names
	(insert "\n")
	(dolist (name-field jabber-vcard-name-fields)
	  (let ((field (assq (car name-field) names)))
	    (when field
	      (insert (cdr name-field))
	      (indent-to 20)
	      (insert (cdr field) "\n"))))))

    (let ((email-addresses (cdr (assq 'EMAIL parsed))))
      (when email-addresses
	(insert "\n")
	(insert (jabber-propertize "E-mail addresses:\n"
				   'face 'jabber-title-medium))
	(dolist (email email-addresses)
	  (insert (mapconcat (lambda (type) 
			       (cdr (assq type jabber-vcard-email-types)))
			     (car email)
			     " "))
	  (insert ": " (cdr email) "\n"))))
	  
    (let ((phone-numbers (cdr (assq 'TEL parsed))))
      (when phone-numbers
	(insert "\n")
	(insert (jabber-propertize "Phone numbers:\n"
				   'face 'jabber-title-medium))
	(dolist (number phone-numbers)
	  (insert (mapconcat (lambda (type) 
			       (cdr (assq type jabber-vcard-phone-types)))
			     (car number)
			     " "))
	  (insert ": " (cdr number) "\n"))))

    (let ((addresses (cdr (assq 'ADR parsed))))
      (when addresses
	(insert "\n")
	(insert (jabber-propertize "Addresses:\n"
				   'face 'jabber-title-medium))
	(dolist (address addresses)
	  (insert (jabber-propertize
		   (mapconcat (lambda (type) 
				(cdr (assq type jabber-vcard-address-types)))
			      (car address)
			      " ")
		   'face 'jabber-title-small))
	  (insert "\n")
	  (dolist (address-field jabber-vcard-address-fields)
	    (let ((field (assq (car address-field) address)))
	      (when field
		(insert (cdr address-field))
		(indent-to 20)
		(insert (cdr field) "\n")))))))

    ;; JEP-0153: vCard-based avatars
    (let ((photo-type (nth 1 (assq 'PHOTO parsed)))
	  (photo-binval (nth 2 (assq 'PHOTO parsed))))
      (when (and photo-type photo-binval)
	(condition-case nil
	    ;; ignore the type, let create-image figure it out.
	    (let ((image (create-image (base64-decode-string photo-binval) nil t)))
	      (insert-image image "[Photo]")
	      (insert "\n"))
	  (error (insert "Couldn't display photo\n")))))))

(defun jabber-vcard-do-edit (jc xml-data closure-data)
  (let ((parsed (jabber-vcard-parse (jabber-iq-query xml-data)))
	start-position)
    (with-current-buffer (get-buffer-create "Edit vcard")
      (jabber-init-widget-buffer nil)

      (setq jabber-buffer-connection jc)

      (setq start-position (point))

      (dolist (simple-field jabber-vcard-fields)
	(widget-insert (cdr simple-field))
	(indent-to 15)
	(let ((default-value (cdr (assq (car simple-field) parsed))))
	  (push (cons (car simple-field)
		      (widget-create 'editable-field (or default-value "")))
		jabber-widget-alist)))
      
      (widget-insert "\n")
      (push (cons 'N
		  (widget-create 
		   '(set :tag "Decomposited name"
			 (cons :tag "Prefix" :format "%t: %v" (const :format "" PREFIX) (string :format "%v"))
			 (cons :tag "Given name" :format "%t: %v" (const :format "" GIVEN) (string :format "%v"))
			 (cons :tag "Middle name" :format "%t: %v" (const :format "" MIDDLE) (string :format "%v"))
			 (cons :tag "Family name" :format "%t: %v" (const :format "" FAMILY) (string :format "%v"))
			 (cons :tag "Suffix" :format "%t: %v" (const :format "" SUFFIX) (string :format "%v")))
		   :value (cdr (assq 'N parsed))))
	    jabber-widget-alist)

      (widget-insert "\n")
      (push (cons 'ADR
		  (widget-create
		   '(repeat :tag "Postal addresses"
			    (cons
			     :tag "Address"
			     (set :tag "Type"
				  (const :tag "Home" HOME)
				  (const :tag "Work" WORK)
				  (const :tag "Postal" POSTAL)
				  (const :tag "Parcel" PARCEL)
				  (const :tag "Domestic" DOM)
				  (const :tag "International" INTL)
				  (const :tag "Preferred" PREF))
			     (set
			      :tag "Address"
			      (cons :tag "Post box" :format "%t: %v"
				    (const :format "" POBOX) (string :format "%v"))
			      (cons :tag "Ext. address" :format "%t: %v"
				    (const :format "" EXTADD) (string :format "%v"))
			      (cons :tag "Street" :format "%t: %v"
				    (const :format "" STREET) (string :format "%v"))
			      (cons :tag "Locality" :format "%t: %v"
				    (const :format "" LOCALITY) (string :format "%v"))
			      (cons :tag "Region" :format "%t: %v"
				    (const :format "" REGION) (string :format "%v"))
			      (cons :tag "Post code" :format "%t: %v"
				    (const :format "" PCODE) (string :format "%v"))
			      (cons :tag "Country" :format "%t: %v"
				    (const :format "" CTRY) (string :format "%v")))))
		   :value (cdr (assq 'ADR parsed))))
	    jabber-widget-alist)

      (widget-insert "\n")
      (push (cons 'TEL
		  (widget-create
		   '(repeat :tag "Phone numbers"
			    (cons :tag "Number"
				  (set :tag "Type"
				       (const :tag "Home" HOME)
				       (const :tag "Work" WORK)
				       (const :tag "Voice" VOICE)
				       (const :tag "Fax" FAX)
				       (const :tag "Pager" PAGER)
				       (const :tag "Message" MSG)
				       (const :tag "Cell phone" CELL)
				       (const :tag "Video" VIDEO)
				       (const :tag "BBS" BBS)
				       (const :tag "Modem" MODEM)
				       (const :tag "ISDN" ISDN)
				       (const :tag "PCS" PCS))
				  (string :tag "Number")))
		   :value (cdr (assq 'TEL parsed))))
	    jabber-widget-alist)

      (widget-insert "\n")
      (push (cons 'EMAIL
		  (widget-create
		   '(repeat :tag "E-mail addresses"
			    (cons :tag "Address"
				  (set :tag "Type"
				       (const :tag "Home" HOME)
				       (const :tag "Work" WORK)
				       (const :tag "Internet" INTERNET)
				       (const :tag "X400" X400)
				       (const :tag "Preferred" PREF))
				  (string :tag "Address")))
		   :value (cdr (assq 'EMAIL parsed))))
	    jabber-widget-alist)

      (widget-insert "\n")
      (widget-insert "Photo/avatar:\n")
      (let* ((photo (assq 'PHOTO parsed))
	     (avatar (when photo
		       (jabber-avatar-from-base64-string (nth 2 photo)
							 (nth 1 photo)))))
	(push (cons 
	       'PHOTO
	       (widget-create
		`(radio-button-choice (const :tag "None" nil)
				      ,@(when photo
					  (list 
					   `(const :tag 
						   ,(concat
						     "Existing: "
						     (jabber-propertize " "
									'display (jabber-avatar-image avatar)))
						   ,(cdr photo))))
				      (file :must-match t :tag "From file"))
		:value (cdr photo)))
	      jabber-widget-alist))

      (widget-insert "\n")
      (widget-create 'push-button :notify #'jabber-vcard-submit "Submit")

      (widget-setup)
      (widget-minor-mode 1)
      (switch-to-buffer (current-buffer))
      (goto-char start-position))))

(defun jabber-vcard-submit (&rest ignore)
  (let ((to-publish (jabber-vcard-reassemble
		     (mapcar (lambda (entry)
			       (cons (car entry) (widget-value (cdr entry))))
			     jabber-widget-alist))))
    (jabber-send-iq jabber-buffer-connection nil
		    "set"
		    to-publish
		    #'jabber-report-success "Changing vCard"
		    #'jabber-report-success "Changing vCard")
    (when (bound-and-true-p jabber-vcard-avatars-publish)
      (jabber-vcard-avatars-update-current
       jabber-buffer-connection
       (and jabber-vcard-photo (avatar-sha1-sum jabber-vcard-photo))))))

(provide 'jabber-vcard)
;; arch-tag: 65B95E9C-63BD-11D9-94A9-000A95C2FCD0
