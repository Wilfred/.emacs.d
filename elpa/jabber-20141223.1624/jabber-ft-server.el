;; jabber-ft-server.el - handle incoming file transfers, by JEP-0096

;; Copyright (C) 2003, 2004, 2007 - Magnus Henoch - mange@freemail.hu
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

(require 'jabber-si-server)
(require 'jabber-util)

(defvar jabber-ft-sessions nil
  "Alist, where keys are (sid jid), and values are buffers of the files.")

(defvar jabber-ft-size nil
  "Size of the file that is being downloaded")

(defvar jabber-ft-md5-hash nil
  "MD5 hash of the file that is being downloaded")

(jabber-disco-advertise-feature "http://jabber.org/protocol/si/profile/file-transfer")

(add-to-list 'jabber-si-profiles
	     (list "http://jabber.org/protocol/si/profile/file-transfer"
		   'jabber-ft-accept
		   'jabber-ft-server-connected))

(defun jabber-ft-accept (jc xml-data)
  "Receive IQ stanza containing file transfer request, ask user"
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
	 (si-id (jabber-xml-get-attribute query 'id))
	 ;; TODO: check namespace
	 (file (car (jabber-xml-get-children query 'file)))
	 (name (jabber-xml-get-attribute file 'name))
	 (size (jabber-xml-get-attribute file 'size))
	 (date (jabber-xml-get-attribute file 'date))
	 (md5-hash (jabber-xml-get-attribute file 'hash))
	 (desc (car (jabber-xml-node-children
		     (car (jabber-xml-get-children file 'desc)))))
	 (range (car (jabber-xml-get-children file 'range))))
    (unless (and name size)
      ;; both name and size must be present
      (jabber-signal-error "modify" 'bad-request))

    (let ((question (format
		     "%s is sending you the file %s (%s bytes).%s  Accept? "
		     (jabber-jid-displayname from)
		     name
		     size
		     (if (not (zerop (length desc)))
			 (concat "  Description: '" desc "'")
		       ""))))
      (unless (yes-or-no-p question)
	(jabber-signal-error "cancel" 'forbidden)))

    ;; default is to save with given name, in current directory.
    ;; maybe that's bad; maybe should be customizable.
    (let* ((file-name (read-file-name "Download to: " nil nil nil name))
	   (buffer (create-file-buffer file-name)))
      (message "Starting download of %s..." (file-name-nondirectory file-name))
      (with-current-buffer buffer
	(kill-all-local-variables)
	(setq buffer-file-coding-system 'binary)
	;; For Emacs, switch buffer to unibyte _before_ anything goes into it,
	;; otherwise binary files are corrupted.  For XEmacs, it isn't needed,
	;; and it also doesn't have set-buffer-multibyte.
	(if (fboundp 'set-buffer-multibyte)
	    (set-buffer-multibyte nil))
	(set-visited-file-name file-name t)
	(set (make-local-variable 'jabber-ft-size) 
	     (string-to-number size))
	(set (make-local-variable 'jabber-ft-md5-hash)
	     md5-hash))
      (add-to-list 'jabber-ft-sessions
		   (cons (list si-id from) buffer)))
      
    ;; to support range, return something sensible here
    nil))

(defun jabber-ft-server-connected (jc jid sid send-data-function)
  ;; We don't really care about the send-data-function.  But if it's
  ;; a string, it means that we have no connection.
  (if (stringp send-data-function)
      (message "File receiving failed: %s" send-data-function)
    ;; On success, we just return our data receiving function.
    'jabber-ft-data))

(defun jabber-ft-data (jc jid sid data)
  "Receive chunk of transferred file."
  (let ((buffer (cdr (assoc (list sid jid) jabber-ft-sessions))))
    (with-current-buffer buffer
      ;; If data is nil, there is no more data.
      ;; But maybe the remote entity doesn't close the stream -
      ;; then we have to keep track of file size to know when to stop.
      ;; Return value is whether to keep connection open.
      (when data
	(insert data))
      (if (and data (< (buffer-size) jabber-ft-size))
	  t
	(basic-save-buffer)
	(if (and jabber-ft-md5-hash
		 (let ((file-hash (jabber-ft-get-md5 buffer-file-name)))
		   (and file-hash
			(not (string= file-hash jabber-ft-md5-hash)))))
	    ;; hash mismatch!
	    (progn
	      (message "%s downloaded - CHECKSUM MISMATCH!"
		       (file-name-nondirectory buffer-file-name))
	      (sleep-for 5))
	  ;; all is fine
	  (message "%s downloaded" (file-name-nondirectory buffer-file-name)))
	(kill-buffer buffer)
	nil))))

(provide 'jabber-ft-server)

;;; arch-tag: 334adcff-6210-496e-8382-8f49ae0248a1
