;;; jabber-vcard-avatars.el --- Avatars by JEP-0153

;; Copyright (C) 2006, 2007, 2008  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'jabber-avatar)

(defcustom jabber-vcard-avatars-retrieve (and (fboundp 'display-images-p)
					      (display-images-p))
  "Automatically download vCard avatars?"
  :group 'jabber-avatar
  :type 'boolean)

(defcustom jabber-vcard-avatars-publish t
  "Publish your vCard photo as avatar?"
  :group 'jabber-avatar
  :type 'boolean)

(defvar jabber-vcard-avatars-current-hash
  (make-hash-table :test 'equal)
  "For each connection, SHA1 hash of current avatar.
Keys are full JIDs.")

(add-to-list 'jabber-presence-chain 'jabber-vcard-avatars-presence)
(defun jabber-vcard-avatars-presence (jc xml-data)
  "Look for vCard avatar mark in <presence/> stanza."
  ;; Only look at ordinary presence
  (when (and jabber-vcard-avatars-retrieve
	     (null (jabber-xml-get-attribute xml-data 'type)))
    (let* ((from (jabber-jid-user (jabber-xml-get-attribute xml-data 'from)))
	   (photo (jabber-xml-path xml-data '(("vcard-temp:x:update" . "x") photo)))
	   (sha1-hash (car (jabber-xml-node-children photo))))
      (cond
       ((null sha1-hash)
	;; User has removed avatar
	(jabber-avatar-set from nil))
       ((string= sha1-hash (get (jabber-jid-symbol from) 'avatar-hash))
	;; Same avatar as before; do nothing
	)
       ((jabber-avatar-find-cached sha1-hash)
	;; Avatar is cached
	(jabber-avatar-set from sha1-hash))
       (t
	;; Avatar is not cached; retrieve it
	(jabber-vcard-avatars-fetch jc from sha1-hash))))))

(defun jabber-vcard-avatars-fetch (jc who sha1-hash)
  "Fetch WHO's vCard, and extract avatar."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Fetch whose vCard avatar: ")
		     nil))
  (jabber-send-iq jc who "get" '(vCard ((xmlns . "vcard-temp")))
		  #'jabber-vcard-avatars-vcard (cons who sha1-hash)
		  #'ignore nil))

(defun jabber-vcard-avatars-vcard (jc iq closure)
  "Get the photo from the vCard, and set the avatar."
  (let ((from (car closure))
	(sha1-hash (cdr closure))
	(photo (assq 'PHOTO (jabber-vcard-parse (jabber-iq-query iq)))))
    (if photo
	(let ((avatar (jabber-avatar-from-base64-string (nth 2 photo)
							(nth 1 photo))))
	  (unless (or (null sha1-hash)
		      (string= sha1-hash (avatar-sha1-sum avatar)))
	    (when jabber-avatar-verbose
	      (message "%s's avatar should have SHA1 sum %s, but has %s"
		       (jabber-jid-displayname from)
		       sha1-hash
		       (avatar-sha1-sum avatar))))
	  (jabber-avatar-cache avatar)
	  (jabber-avatar-set from avatar))
      (jabber-avatar-set from nil))))

(defun jabber-vcard-avatars-find-current (jc)
  "Request our own vCard, to find hash of avatar."
  (when jabber-vcard-avatars-publish
    (jabber-send-iq jc nil "get" '(vCard ((xmlns . "vcard-temp")))
		    #'jabber-vcard-avatars-find-current-1 t
		    #'jabber-vcard-avatars-find-current-1 nil)))

(defun jabber-vcard-avatars-find-current-1 (jc xml-data success)
  (jabber-vcard-avatars-update-current
   jc
   (and success
	(let ((photo (assq 'PHOTO (jabber-vcard-parse (jabber-iq-query xml-data)))))
	  (when photo
	    (let ((avatar (jabber-avatar-from-base64-string (nth 2 photo)
							    (nth 1 photo))))
	      (avatar-sha1-sum avatar)))))))

(defun jabber-vcard-avatars-update-current (jc new-hash)
  (let ((old-hash (gethash
		   (jabber-connection-bare-jid jc)
		   jabber-vcard-avatars-current-hash)))
    (when (not (string= old-hash new-hash))
      (puthash (jabber-connection-bare-jid jc)
	       new-hash jabber-vcard-avatars-current-hash)
      (jabber-send-current-presence jc))))

(add-to-list 'jabber-presence-element-functions 'jabber-vcard-avatars-presence-element)
(defun jabber-vcard-avatars-presence-element (jc)
  (when jabber-vcard-avatars-publish
    (let ((hash (gethash
		 (jabber-connection-bare-jid jc)
		 jabber-vcard-avatars-current-hash)))
      (list
       `(x ((xmlns . "vcard-temp:x:update"))
	   ;; if "not yet ready to advertise image", don't.
	   ;; that is, we haven't yet checked what avatar we have.
	   ,(when hash
	      `(photo () ,hash)))))))
	     
(provide 'jabber-vcard-avatars)
;; arch-tag: 3e50d460-8eae-11da-826c-000a95c2fcd0
