;; jabber-presence.el - roster and presence bookkeeping

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

(require 'jabber-core)
(require 'jabber-iq)
(require 'jabber-alert)
(require 'jabber-util)
(require 'jabber-menu)
(require 'jabber-muc)

(defvar jabber-presence-element-functions nil
  "List of functions returning extra elements for <presence/> stanzas.
Each function takes one argument, the connection, and returns a
possibly empty list of extra child element of the <presence/>
stanza.")

(defvar jabber-presence-history ()
  "Keeps track of previously used presence status types")

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "jabber:iq:roster" (function (lambda (jc x) (jabber-process-roster jc x nil)))))
(defun jabber-process-roster (jc xml-data closure-data)
  "process an incoming roster infoquery result
CLOSURE-DATA should be 'initial if initial roster push, nil otherwise."
  (let ((roster (plist-get (fsm-get-state-data jc) :roster))
	(from (jabber-xml-get-attribute xml-data 'from))
	(type (jabber-xml-get-attribute xml-data 'type))
	(id (jabber-xml-get-attribute xml-data 'id))
	(username (plist-get (fsm-get-state-data jc) :username))
	(server (plist-get (fsm-get-state-data jc) :server))
	(resource (plist-get (fsm-get-state-data jc) :resource))
	new-items changed-items deleted-items)
    ;; Perform sanity check on "from" attribute: it should be either absent
    ;; match our own JID, or match the server's JID (the latter is what
    ;; Facebook does).
    (if (not (or (null from)
		 (string= from server)
		 (string= from (concat username "@" server))
		 (string= from (concat username "@" server "/" resource))))
	(message "Roster push with invalid \"from\": \"%s\" (expected \"%s\", \"%s@%s\" or \"%s@%s/%s\")"
		 from
		 server username server username server resource)

      (dolist (item (jabber-xml-get-children (car (jabber-xml-get-children xml-data 'query)) 'item))
	(let (roster-item
	      (jid (jabber-jid-symbol (jabber-xml-get-attribute item 'jid))))

	  ;; If subscripton="remove", contact is to be removed from roster
	  (if (string= (jabber-xml-get-attribute item 'subscription) "remove")
	      (progn
		(if (jabber-jid-rostername jid)
		    (message "%s (%s) removed from roster" (jabber-jid-rostername jid) jid)
		  (message "%s removed from roster" jid))
		(push jid deleted-items))

	    ;; Find contact if already in roster
	    (setq roster-item (car (memq jid roster)))

	    (if roster-item
		(push roster-item changed-items)
	      ;; If not found, create a new roster item.
	      (unless (eq closure-data 'initial)
		(if (jabber-xml-get-attribute item 'name)
		    (message "%s (%s) added to roster" (jabber-xml-get-attribute item 'name) jid)
		  (message "%s added to roster" jid)))
	      (setq roster-item jid)
	      (push roster-item new-items))

	    ;; If this is an initial push, we want to forget
	    ;; everything we knew about this contact before - e.g. if
	    ;; the contact was online when we disconnected and offline
	    ;; when we reconnect, we don't want to see stale presence
	    ;; information.  This assumes that no contacts are shared
	    ;; between accounts.
	    (when (eq closure-data 'initial)
	      (setplist roster-item nil))

	    ;; Now, get all data associated with the contact.
	    (put roster-item 'name (jabber-xml-get-attribute item 'name))
	    (put roster-item 'subscription (jabber-xml-get-attribute item 'subscription))
	    (put roster-item 'ask (jabber-xml-get-attribute item 'ask))

	    ;; Since roster items can't be changed incrementally, we
	    ;; save the original XML to be able to modify it, instead of
	    ;; having to reproduce it.  This is for forwards
	    ;; compatibility.
	    (put roster-item 'xml item)

	    (put roster-item 'groups
		 (mapcar (lambda (foo) (nth 2 foo))
			 (jabber-xml-get-children item 'group)))))))
    ;; This is the function that does the actual updating and
    ;; redrawing of the roster.
    (jabber-roster-update jc new-items changed-items deleted-items)

    (if (and id (string= type "set"))
	(jabber-send-iq jc nil "result" nil
			nil nil nil nil id)))

  ;; After initial roster push, run jabber-post-connect-hooks.  We do
  ;; it here and not before since we want to have the entire roster
  ;; before we receive any presence stanzas.
  (when (eq closure-data 'initial)
    (run-hook-with-args 'jabber-post-connect-hooks jc)))

(defun jabber-initial-roster-failure (jc xml-data _closure-data)
  ;; If the initial roster request fails, let's report it, but run
  ;; jabber-post-connect-hooks anyway.  According to the spec, there
  ;; is nothing exceptional about the server not returning a roster.
  (jabber-report-success jc xml-data "Initial roster retrieval")
  (run-hook-with-args 'jabber-post-connect-hooks jc))

(add-to-list 'jabber-presence-chain 'jabber-process-presence)
(defun jabber-process-presence (jc xml-data)
  "process incoming presence tags"
  ;; XXX: use JC argument
  (let ((roster (plist-get (fsm-get-state-data jc) :roster))
	(from (jabber-xml-get-attribute xml-data 'from))
	(to (jabber-xml-get-attribute xml-data 'to))
	(type (jabber-xml-get-attribute xml-data 'type))
	(presence-show (car (jabber-xml-node-children
			     (car (jabber-xml-get-children xml-data 'show)))))
	(presence-status (car (jabber-xml-node-children
			       (car (jabber-xml-get-children xml-data 'status)))))
	(error (car (jabber-xml-get-children xml-data 'error)))
	(priority (string-to-number (or (car (jabber-xml-node-children (car (jabber-xml-get-children xml-data 'priority))))
					"0"))))
    (cond
     ((string= type "subscribe")
      (run-with-idle-timer 0.01 nil #'jabber-process-subscription-request jc from presence-status))

     ((jabber-muc-presence-p xml-data)
      (jabber-muc-process-presence jc xml-data))

     (t
      ;; XXX: Think about what to do about out-of-roster presences.
      (let ((buddy (jabber-jid-symbol from)))
	(if (memq buddy roster)
	    (let* ((oldstatus (get buddy 'show))
		   (resource (or (jabber-jid-resource from) ""))
		   (resource-plist (cdr (assoc resource
					       (get buddy 'resources))))
		   newstatus)
	      (cond
	       ((and (string= resource "") (member type '("unavailable" "error")))
		;; 'unavailable' or 'error' from bare JID means that all resources
		;; are offline.
		(setq resource-plist nil)
		(setq newstatus (if (string= type "error") "error" nil))
		(let ((new-message (if error
				       (jabber-parse-error error)
				     presence-status)))
		  ;; erase any previous information
		  (put buddy 'resources nil)
		  (put buddy 'connected nil)
		  (put buddy 'show newstatus)
		  (put buddy 'status new-message)))

	       ((string= type "unavailable")
		(setq resource-plist
		      (plist-put resource-plist 'connected nil))
		(setq resource-plist
		      (plist-put resource-plist 'show nil))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 presence-status)))

	       ((string= type "error")
		(setq newstatus "error")
		(setq resource-plist
		      (plist-put resource-plist 'connected nil))
		(setq resource-plist
		      (plist-put resource-plist 'show "error"))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 (if error
				     (jabber-parse-error error)
				   presence-status))))
	       ((or
		 (string= type "unsubscribe")
		 (string= type "subscribed")
		 (string= type "unsubscribed"))
		;; Do nothing, except letting the user know.  The Jabber protocol
		;; places all this complexity on the server.
		(setq newstatus type))
	       (t
		(setq resource-plist
		      (plist-put resource-plist 'connected t))
		(setq resource-plist
		      (plist-put resource-plist 'show (or presence-show "")))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 presence-status))
		(setq resource-plist
		      (plist-put resource-plist 'priority priority))
		(setq newstatus (or presence-show ""))))

	      (when resource-plist
		;; this is for `assoc-set!' in guile
		(if (assoc resource (get buddy 'resources))
		    (setcdr (assoc resource (get buddy 'resources)) resource-plist)
		  (put buddy 'resources (cons (cons resource resource-plist) (get buddy 'resources))))
		(jabber-prioritize-resources buddy))

	      (fsm-send jc (cons :roster-update buddy))

	      (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
		(run-hook-with-args hook
				    buddy
				    oldstatus
				    newstatus
				    (plist-get resource-plist 'status)
				    (funcall jabber-alert-presence-message-function
					     buddy
					     oldstatus
					     newstatus
					     (plist-get resource-plist 'status)))))))))))

(defun jabber-process-subscription-request (jc from presence-status)
  "process an incoming subscription request"
  (with-current-buffer (jabber-chat-create-buffer jc from)
    (ewoc-enter-last jabber-chat-ewoc (list :subscription-request presence-status :time (current-time)))

    (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
      (run-hook-with-args hook (jabber-jid-symbol from) nil "subscribe" presence-status (funcall jabber-alert-presence-message-function (jabber-jid-symbol from) nil "subscribe" presence-status)))))

(defun jabber-subscription-accept-mutual (&rest ignored)
  (message "Subscription accepted; reciprocal subscription request sent")
  (jabber-subscription-reply "subscribed" "subscribe"))

(defun jabber-subscription-accept-one-way (&rest ignored)
  (message "Subscription accepted")
  (jabber-subscription-reply "subscribed"))

(defun jabber-subscription-decline (&rest ignored)
  (message "Subscription declined")
  (jabber-subscription-reply "unsubscribed"))

(defun jabber-subscription-reply (&rest types)
  (let ((to (jabber-jid-user jabber-chatting-with)))
    (dolist (type types)
      (jabber-send-sexp jabber-buffer-connection `(presence ((to . ,to) (type . ,type)))))))

(defun jabber-prioritize-resources (buddy)
  "Set connected, show and status properties for BUDDY from highest-priority resource."
  (let ((resource-alist (get buddy 'resources))
	(highest-priority nil))
    ;; Reset to nil at first, for cases (a) resource-alist is nil
    ;; and (b) all resources are disconnected.
    (put buddy 'connected nil)
    (put buddy 'show nil)
    (put buddy 'status nil)
    (mapc #'(lambda (resource)
	      (let* ((resource-plist (cdr resource))
		     (priority (plist-get resource-plist 'priority)))
		(if (plist-get resource-plist 'connected)
		    (when (or (null highest-priority)
			      (and priority
				   (> priority highest-priority)))
		      ;; if no priority specified, interpret as zero
		      (setq highest-priority (or priority 0))
		      (put buddy 'connected (plist-get resource-plist 'connected))
		      (put buddy 'show (plist-get resource-plist 'show))
		      (put buddy 'status (plist-get resource-plist 'status))
		      (put buddy 'resource (car resource)))

		  ;; if we have not found a connected resource yet, but this
		  ;; disconnected resource has a status message, display it.
		  (when (not (get buddy 'connected))
		    (if (plist-get resource-plist 'status)
			(put buddy 'status (plist-get resource-plist 'status)))
		    (if (plist-get resource-plist 'show)
			(put buddy 'show (plist-get resource-plist 'show)))))))
	  resource-alist)))

(defun jabber-count-connected-resources (buddy)
  "Return the number of connected resources for BUDDY."
  (let ((resource-alist (get buddy 'resources))
	(count 0))
    (dolist (resource resource-alist)
      (if (plist-get (cdr resource) 'connected)
	  (setq count (1+ count))))
    count))

;;;###autoload
(defun jabber-send-presence (show status priority)
  "Set presence for all accounts."
  (interactive
   (list
    (completing-read "show: " '("" "away" "xa" "dnd" "chat")
		     nil t nil 'jabber-presence-history)
    (jabber-read-with-input-method "status message: " *jabber-current-status*
				   '*jabber-status-history*)
    (read-string "priority: " (int-to-string (if *jabber-current-priority*
						 *jabber-current-priority*
					       jabber-default-priority)))))

  (setq *jabber-current-show* show *jabber-current-status* status)
  (setq *jabber-current-priority*
	(if (numberp priority) priority (string-to-number priority)))

  (let (subelements-map)
    ;; For each connection, we use a different set of subelements.  We
    ;; cache them, to only generate them once.

    ;; Ordinary presence, with no specified recipient
    (dolist (jc jabber-connections)
      (let ((subelements (jabber-presence-children jc)))
        (push (cons jc subelements) subelements-map)
	(jabber-send-sexp-if-connected jc `(presence () ,@subelements))))

    ;; Then send presence to groupchats
    (dolist (gc *jabber-active-groupchats*)
      (let* ((buffer (get-buffer (jabber-muc-get-buffer (car gc))))
	     (jc (when buffer
		   (buffer-local-value 'jabber-buffer-connection buffer)))
	     (subelements (cdr (assq jc subelements-map))))
	(when jc
	  (jabber-send-sexp-if-connected
	   jc `(presence ((to . ,(concat (car gc) "/" (cdr gc))))
			 ,@subelements))))))

  (jabber-display-roster))

(defun jabber-presence-children (jc)
  "Return the children for a <presence/> stanza."
  `(,(when (> (length *jabber-current-status*) 0)
       `(status () ,*jabber-current-status*))
    ,(when (> (length *jabber-current-show*) 0)
	 `(show () ,*jabber-current-show*))
    ,(when *jabber-current-priority*
       `(priority () ,(number-to-string *jabber-current-priority*)))
    ,@(apply 'append (mapcar (lambda (f)
			       (funcall f jc))
			     jabber-presence-element-functions))))

(defun jabber-send-directed-presence (jc jid type)
  "Send a directed presence stanza to JID.
TYPE is one of:
\"online\", \"away\", \"xa\", \"dnd\", \"chatty\":
  Appear as present with the given status.
\"unavailable\":
  Appear as offline.
\"probe\":
  Ask the contact's server for updated presence.
\"subscribe\":
  Ask for subscription to contact's presence.
  (see also `jabber-send-subscription-request')
\"unsubscribe\":
  Cancel your subscription to contact's presence.
\"subscribed\":
  Accept contact's request for presence subscription.
  (this is usually done within a chat buffer)
\"unsubscribed\":
  Cancel contact's subscription to your presence."
  (interactive
   (list (jabber-read-account)
	 (jabber-read-jid-completing "Send directed presence to: ")
	 (completing-read "Type (default is online): "
			  '(("online")
			    ("away")
			    ("xa")
			    ("dnd")
			    ("chatty")
			    ("probe")
			    ("unavailable")
			    ("subscribe")
			    ("unsubscribe")
			    ("subscribed")
			    ("unsubscribed"))
			  nil t nil 'jabber-presence-history "online")))
  (cond
   ((member type '("probe" "unavailable"
		   "subscribe" "unsubscribe"
		   "subscribed" "unsubscribed"))
    (jabber-send-sexp jc `(presence ((to . ,jid)
				     (type . ,type)))))

   (t
    (let ((*jabber-current-show*
	   (if (string= type "online")
	       ""
	     type))
	  (*jabber-current-status* nil))
      (jabber-send-sexp jc `(presence ((to . ,jid))
				      ,@(jabber-presence-children jc)))))))

(defun jabber-send-away-presence (&optional status)
  "Set status to away.
With prefix argument, ask for status message."
  (interactive
   (list
    (when current-prefix-arg
      (jabber-read-with-input-method
       "status message: " *jabber-current-status* '*jabber-status-history*))))
  (jabber-send-presence "away" (if status status *jabber-current-status*)
			*jabber-current-priority*))

;; XXX code duplication!
(defun jabber-send-xa-presence (&optional status)
  "Send extended away presence.
With prefix argument, ask for status message."
  (interactive
   (list
    (when current-prefix-arg
      (jabber-read-with-input-method
       "status message: " *jabber-current-status* '*jabber-status-history*))))
  (jabber-send-presence "xa" (if status status *jabber-current-status*)
			*jabber-current-priority*))

;;;###autoload
(defun jabber-send-default-presence (&optional ignore)
  "Send default presence.
Default presence is specified by `jabber-default-show',
`jabber-default-status', and `jabber-default-priority'."
  (interactive)
  (jabber-send-presence
   jabber-default-show jabber-default-status jabber-default-priority))

(defun jabber-send-current-presence (&optional ignore)
  "(Re-)send current presence.
That is, if presence has already been sent, use current settings,
otherwise send defaults (see `jabber-send-default-presence')."
  (interactive)
  (if *jabber-current-show*
      (jabber-send-presence *jabber-current-show* *jabber-current-status*
			    *jabber-current-priority*)
    (jabber-send-default-presence)))

(add-to-list 'jabber-jid-roster-menu (cons "Send subscription request"
					   'jabber-send-subscription-request))
(defun jabber-send-subscription-request (jc to &optional request)
  "send a subscription request to jid, showing him your request
text, if specified"
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "request: ")))
  (jabber-send-sexp jc
		    `(presence
		      ((to . ,to)
		       (type . "subscribe"))
		      ,@(when (and request (> (length request) 0))
			  (list `(status () ,request))))))

(defvar jabber-roster-group-history nil
  "History of entered roster groups")

(add-to-list 'jabber-jid-roster-menu
	     (cons "Add/modify roster entry" 'jabber-roster-change))
(defun jabber-roster-change (jc jid name groups)
  "Add or change a roster item."
  (interactive (let* ((jid (jabber-jid-symbol
			    (jabber-read-jid-completing "Add/change JID: ")))
		      (account (jabber-read-account))
		      (name (get jid 'name))
		      (groups (get jid 'groups))
		      (all-groups
		       (apply #'append
			      (mapcar
			       (lambda (j) (get j 'groups))
			       (plist-get (fsm-get-state-data account) :roster)))))
		 (when (string< emacs-version "22")
		   ;; Older emacsen want the completion table to be an alist...
		   (setq all-groups (mapcar #'list all-groups)))
		 (list account
		       jid (jabber-read-with-input-method (format "Name: (default `%s') " name) nil nil name)
		       (delete ""
			       (completing-read-multiple
				(format
				 "Groups, comma-separated: (default %s) "
				 (if groups
				     (mapconcat #'identity groups ",")
				   "none"))
				all-groups
				nil nil nil
				'jabber-roster-group-history
				(mapconcat #'identity groups ",")
				t)))))
  ;; If new fields are added to the roster XML structure in a future standard,
  ;; they will be clobbered by this function.
  ;; XXX: specify account
  (jabber-send-iq jc nil "set"
		  (list 'query (list (cons 'xmlns "jabber:iq:roster"))
				(append
				 (list 'item (append
				     (list (cons 'jid (symbol-name jid)))
				     (if (and name (> (length name) 0))
					 (list (cons 'name name)))))
				 (mapcar #'(lambda (x) `(group () ,x))
				      groups)))
		  #'jabber-report-success "Roster item change"
		  #'jabber-report-success "Roster item change"))

(add-to-list 'jabber-jid-roster-menu
	     (cons "Delete roster entry" 'jabber-roster-delete))
(defun jabber-roster-delete (jc jid)
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Delete from roster: ")))
  (jabber-send-iq jc nil "set"
		  `(query ((xmlns . "jabber:iq:roster"))
			  (item ((jid . ,jid)
				 (subscription . "remove"))))
		  #'jabber-report-success "Roster item removal"
		  #'jabber-report-success "Roster item removal"))

(defun jabber-roster-delete-jid-at-point ()
  "Delete JID at point from roster.
Signal an error if there is no JID at point."
  (interactive)
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid))
	(account (get-text-property (point) 'jabber-account)))
    (if (and jid-at-point account
	     (or jabber-silent-mode (yes-or-no-p (format "Really delete %s from roster? " jid-at-point))))
	(jabber-roster-delete account jid-at-point)
      (error "No contact at point"))))

(defun jabber-roster-delete-group-from-jids (jc jids group)
  "Delete group `group' from all JIDs"
  (interactive)
  (dolist (jid jids)
    (jabber-roster-change
     jc jid (get jid 'name)
     (remove-if-not (lambda (g) (not (string= g group)))
		    (get jid 'groups)))))

(defun jabber-roster-edit-group-from-jids (jc jids group)
  "Edit group `group' from all JIDs"
  (interactive)
  (let ((new-group
	 (jabber-read-with-input-method
	  (format "New group: (default `%s') " group) nil nil group)))
    (dolist (jid jids)
      (jabber-roster-change
       jc jid (get jid 'name)
       (remove-duplicates
	(mapcar
	 (lambda (g) (if (string= g group)
			 new-group
		       g))
	 (get jid 'groups))
	:test 'string=)))))


(provide 'jabber-presence)

;;; arch-tag: b8616d4c-dde8-423e-86c7-da7b4928afc3
