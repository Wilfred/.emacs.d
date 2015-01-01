;; jabber-muc.el - advanced MUC functions

;; Copyright (C) 2010 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2003, 2004, 2007, 2008, 2009, 2010 - Magnus Henoch - mange@freemail.hu
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

(require 'jabber-chat)
(require 'jabber-widget)
(require 'jabber-disco)
(require 'jabber-muc-nick-coloring)

;; we need jabber-bookmarks for jabber-muc-autojoin (via
;; jabber-get-bookmarks and jabber-parse-conference-bookmark):
(require 'jabber-bookmarks)

(require 'cl)

;;;###autoload
(defvar *jabber-active-groupchats* nil
  "alist of groupchats and nicknames
Keys are strings, the bare JID of the room.
Values are strings.")

(defvar jabber-pending-groupchats (make-hash-table)
  "Hash table of groupchats and nicknames.
Keys are JID symbols; values are strings.
This table records the last nickname used to join the particular
chat room.  Items are thus never removed.")

(defvar jabber-muc-participants nil
  "alist of groupchats and participants
Keys are strings, the bare JID of the room.
Values are lists of nickname strings.")

(defvar jabber-group nil
  "the groupchat you are participating in")

(defvar jabber-muc-topic ""
  "The topic of the current MUC room.")

(defvar jabber-role-history ()
  "Keeps track of previously used roles")

(defvar jabber-affiliation-history ()
  "Keeps track of previously used affiliations")

(defvar jabber-muc-nickname-history ()
  "Keeps track of previously referred-to nicknames")

(defcustom jabber-muc-default-nicknames nil
  "Default nickname for specific MUC rooms."
  :group 'jabber-chat
  :type '(repeat
	  (cons :format "%v"
		(string :tag "JID of room")
		(string :tag "Nickname"))))

(defcustom jabber-muc-autojoin nil
  "List of MUC rooms to automatically join on connection.
This list is saved in your Emacs customizations.  You can also store
such a list on the Jabber server, where it is available to every
client; see `jabber-edit-bookmarks'."
  :group 'jabber-chat
  :type '(repeat (string :tag "JID of room")))

(defcustom jabber-muc-disable-disco-check nil
  "If non-nil, disable checking disco#info of rooms before joining them.
Disco information can tell whether the room exists and whether it is
password protected, but some servers do not support it.  If you want
to join chat rooms on such servers, set this variable to t."
  :group 'jabber-chat
  :type 'boolean)

(defcustom jabber-groupchat-buffer-format "*-jabber-groupchat-%n-*"
  "The format specification for the name of groupchat buffers.

These fields are available (all are about the group you are chatting
in):

%n   Roster name of group, or JID if no nickname set
%b   Name of group from bookmarks or roster name or JID if none set
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-groupchat-prompt-format "[%t] %n> "
  "The format specification for lines in groupchat.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
%n, %u, %r
     Nickname in groupchat
%j   Full JID (room@server/nick)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-muc-header-line-format
  '(" " (:eval (jabber-jid-displayname jabber-group))
    "\t" jabber-muc-topic)
  "The specification for the header line of MUC buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)

(defcustom jabber-muc-private-buffer-format "*-jabber-muc-priv-%g-%n-*"
  "The format specification for the buffer name for private MUC messages.

These fields are available:

%g   Roster name of group, or JID if no nickname set
%n   Nickname of the group member you're chatting with"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-muc-private-foreign-prompt-format "[%t] %g/%n> "
  "The format specification for lines others type in a private MUC buffer.

These fields are available:

%t  Time, formatted according to `jabber-chat-time-format'
%n  Nickname in room
%g  Short room name (either roster name or username part of JID)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-muc-print-names-format "	%n	%a	%j\n"
  "The format specification for MUC list lines.

Fields available:

%n  Nickname in room
%a  Affiliation status
%j  Full JID (room@server/nick)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-muc-private-header-line-format
  '(" " (:eval (jabber-jid-resource jabber-chatting-with))
    " in " (:eval (jabber-jid-displayname (jabber-jid-user jabber-chatting-with)))
    "\t" jabber-events-message
    "\t" jabber-chatstates-message)
  "The specification for the header line of private MUC chat buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)

;;;###autoload
(defvar jabber-muc-printers '()
  "List of functions that may be able to print part of a MUC message.
This gets prepended to `jabber-chat-printers', which see.")

;;;###autoload
(defun jabber-muc-get-buffer (group)
  "Return the chat buffer for chatroom GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-groupchat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname group))
                (cons ?b (jabber-jid-bookmarkname group))
		(cons ?j (jabber-jid-user group)))))

(defun jabber-muc-create-buffer (jc group)
  "Prepare a buffer for chatroom GROUP.
This function is idempotent."
  (with-current-buffer (get-buffer-create (jabber-muc-get-buffer group))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode jc #'jabber-chat-pp))
    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)

    (set (make-local-variable 'jabber-group) group)
    (make-local-variable 'jabber-muc-topic)
    (setq jabber-send-function 'jabber-muc-send)
    (setq header-line-format jabber-muc-header-line-format)
    (current-buffer)))

;;;###autoload
(defun jabber-muc-private-get-buffer (group nickname)
  "Return the chat buffer for private chat with NICKNAME in GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-muc-private-buffer-format
	       (list
		(cons ?g (jabber-jid-displayname group))
		(cons ?n nickname))))

(defun jabber-muc-private-create-buffer (jc group nickname)
  "Prepare a buffer for chatting with NICKNAME in GROUP.
This function is idempotent."
  (with-current-buffer (get-buffer-create (jabber-muc-private-get-buffer group nickname))
    (unless (eq major-mode 'jabber-chat-mode)
      (jabber-chat-mode jc #'jabber-chat-pp))

    (set (make-local-variable 'jabber-chatting-with) (concat group "/" nickname))
    (setq jabber-send-function 'jabber-chat-send)
    (setq header-line-format jabber-muc-private-header-line-format)

    (current-buffer)))

(defun jabber-muc-send (jc body)
  "Send BODY to MUC room in current buffer."
  ;; There is no need to display the sent message in the buffer, as
  ;; we will get it back from the MUC server.
  (jabber-send-sexp jc
		    `(message
		      ((to . ,jabber-group)
		       (type . "groupchat"))
		      (body () ,body))))

(defun jabber-muc-add-groupchat (group nickname)
  "Remember participating in GROUP under NICKNAME."
  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    (if whichgroup
	(setcdr whichgroup nickname)
      (add-to-list '*jabber-active-groupchats* (cons group nickname)))))

(defun jabber-muc-remove-groupchat (group)
  "Remove GROUP from internal bookkeeping."
  (let ((whichgroup (assoc group *jabber-active-groupchats*))
	(whichparticipants (assoc group jabber-muc-participants)))
    (setq *jabber-active-groupchats* 
	  (delq whichgroup *jabber-active-groupchats*))
    (setq jabber-muc-participants
	  (delq whichparticipants jabber-muc-participants))))

(defun jabber-muc-connection-closed (bare-jid)
  "Remove MUC data for BARE-JID.
Forget all information about rooms that had been entered with
this JID.  Suitable to call when the connection is closed."
  (dolist (room-entry jabber-muc-participants)
    (let* ((room (car room-entry))
	   (buffer (get-buffer (jabber-muc-get-buffer room))))
      (when (bufferp buffer)
	(with-current-buffer buffer
	  (when (string= bare-jid
			 (jabber-connection-bare-jid jabber-buffer-connection))
	    (setq *jabber-active-groupchats*
		  (delete* room *jabber-active-groupchats*
			   :key #'car :test #'string=))
	    (setq jabber-muc-participants
		  (delq room-entry jabber-muc-participants))))))))

(defun jabber-muc-participant-plist (group nickname)
  "Return plist associated with NICKNAME in GROUP.
Return nil if nothing known about that combination."
  (let ((whichparticipants (assoc group jabber-muc-participants)))
    (when whichparticipants
      (cdr (assoc nickname whichparticipants)))))

(defun jabber-muc-modify-participant (group nickname new-plist)
  "Assign properties in NEW-PLIST to NICKNAME in GROUP."
  (let ((participants (assoc group jabber-muc-participants)))
    ;; either we have a list of participants already...
    (if participants
	(let ((participant (assoc nickname participants)))
	  ;; and maybe this participant is already in the list
	  (if participant
	      ;; if so, just update role, affiliation, etc.
	      (setf (cdr participant) new-plist)
	    (push (cons nickname new-plist) (cdr participants))))
      ;; or we don't
      (push (cons group (list (cons nickname new-plist))) jabber-muc-participants))))

(defun jabber-muc-report-delta (nickname old-plist new-plist reason actor)
  "Compare OLD-PLIST and NEW-PLIST, and return a string explaining the change.
Return nil if nothing noteworthy has happened.
NICKNAME is the user experiencing the change.  REASON and ACTOR, if non-nil,
are the corresponding presence fields.

This function is only concerned with presence stanzas resulting
in the user entering/staying in the room."
  ;; The keys in the plist are affiliation, role and jid.
  (when (plist-get new-plist 'jid)
    ;; nickname is only used for displaying, so we can modify it if we
    ;; want to.
    (setq nickname (concat nickname " <" 
			   (jabber-jid-user (plist-get new-plist 'jid))
			   ">")))
  (cond
   ((null old-plist)
    ;; User enters the room
    (concat nickname " enters the room ("
	    (plist-get new-plist 'role)
	    (unless (string= (plist-get new-plist 'affiliation) "none")
	      (concat ", " (plist-get new-plist 'affiliation)))
	    ")"))

   ;; If affiliation changes, the role change is usually the logical
   ;; one, so don't report it separately.
   ((not (string= (plist-get old-plist 'affiliation)
		  (plist-get new-plist 'affiliation)))
    (let ((actor-reason (concat (when actor
				  (concat " by " actor))
				(when reason
				  (concat ": " reason))))
	  (from (plist-get old-plist 'affiliation))
	  (to (plist-get new-plist 'affiliation)))
      ;; There are many ways to express these transitions in English.
      ;; This one favors eloquence over regularity and consistency.
      (cond
       ;; Higher affiliation
       ((or (and (member from '("outcast" "none" "member"))
		 (member to '("admin" "owner")))
	    (and (string= from "admin") (string= to "owner")))
	(concat nickname " has been promoted to " to actor-reason))
       ;; Lower affiliation
       ((or (and (member from '("owner" "admin"))
		 (string= to "member"))
	    (and (string= from "owner") (string= to "admin")))
	(concat nickname " has been demoted to " to actor-reason))
       ;; Become member
       ((string= to "member")
	(concat nickname " has been granted membership" actor-reason))
       ;; Lose membership
       ((string= to "none")
	(concat nickname " has been deprived of membership" actor-reason)))))

   ;; Role changes
   ((not (string= (plist-get old-plist 'role)
		  (plist-get new-plist 'role)))
    (let ((actor-reason (concat (when actor
				  (concat " by " actor))
				(when reason
				  (concat ": " reason))))
	  (from (plist-get old-plist 'role))
	  (to (plist-get new-plist 'role)))
      ;; Possible roles are "none" (not in room, hence not of interest
      ;; in this function), "visitor" (no voice), "participant" (has
      ;; voice), and "moderator".
      (cond
       ((string= to "moderator")
	(concat nickname " has been granted moderator privileges" actor-reason))
       ((and (string= from "moderator")
	     (string= to "participant"))
	(concat nickname " had moderator privileges revoked" actor-reason))
       ((string= to "participant")
	(concat nickname " has been granted voice" actor-reason))
       ((string= to "visitor")
	(concat nickname " has been denied voice" actor-reason)))))))

(defun jabber-muc-remove-participant (group nickname)
  "Forget everything about NICKNAME in GROUP."
  (let ((participants (assoc group jabber-muc-participants)))
    (when participants
      (let ((participant (assoc nickname (cdr participants))))
	(setf (cdr participants) (delq participant (cdr participants)))))))

(defmacro jabber-muc-argument-list (&optional args)
  "Prepend connection and group name to ARGS.
If the current buffer is not an MUC buffer, signal an error.
This macro is meant for use as an argument to `interactive'."
  `(if (null jabber-group)
       (error "Not in MUC buffer")
     (nconc (list jabber-buffer-connection jabber-group) ,args)))

(defun jabber-muc-read-completing (prompt &optional allow-not-joined)
  "Read the name of a joined chatroom, or use chatroom of current buffer, if any.
If ALLOW-NOT-JOINED is provided and true, permit choosing any
JID; only provide completion as a guide."
  (or jabber-group
      (jabber-read-jid-completing prompt
				  (if (null *jabber-active-groupchats*)
				      (error "You haven't joined any group")
				    (mapcar (lambda (x) (jabber-jid-symbol (car x)))
					    *jabber-active-groupchats*))
				  (not allow-not-joined)
				  jabber-group)))

(defun jabber-muc-read-nickname (group prompt)
  "Read the nickname of a participant in GROUP."
  (let ((nicknames (cdr (assoc group jabber-muc-participants))))
    (unless nicknames
      (error "Unknown group: %s" group))
    (completing-read prompt nicknames nil t nil 'jabber-muc-nickname-history)))

(add-to-list 'jabber-jid-muc-menu
             (cons "Request vcard" 'jabber-muc-vcard-get))

;;;###autoload
(defun jabber-muc-vcard-get (jc group nickname)
  "Request vcard from chat with NICKNAME in GROUP."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
    (let ((muc-name (format "%s/%s" group nickname)))
	(jabber-vcard-get jc muc-name)))

(defun jabber-muc-instant-config (jc group)
  "Accept default configuration for GROUP.
This can be used for a newly created room, as an alternative to
filling out the configuration form with `jabber-muc-get-config'.
Both of these methods unlock the room, so that other users can
enter it."
  (interactive (jabber-muc-argument-list))
  (jabber-send-iq jc group
		  "set"
		  '(query ((xmlns . "http://jabber.org/protocol/muc#owner"))
			  (x ((xmlns . "jabber:x:data") (type . "submit"))))
		  #'jabber-report-success "MUC instant configuration"
		  #'jabber-report-success "MUC instant configuration"))

(add-to-list 'jabber-jid-muc-menu
   (cons "Configure groupchat" 'jabber-muc-get-config))

(defun jabber-muc-get-config (jc group)
  "Ask for MUC configuration form"
  (interactive (jabber-muc-argument-list))
  (jabber-send-iq jc group
		  "get"
		  '(query ((xmlns . "http://jabber.org/protocol/muc#owner")))
		  #'jabber-process-data #'jabber-muc-render-config
		  #'jabber-process-data "MUC configuration request failed"))

(defalias 'jabber-groupchat-get-config 'jabber-muc-get-config
  "Deprecated. See `jabber-muc-get-config' instead.")

(defun jabber-muc-render-config (jc xml-data)
  "Render MUC configuration form"

  (let ((query (jabber-iq-query xml-data))
	xdata)
    (dolist (x (jabber-xml-get-children query 'x))
      (if (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	  (setq xdata x)))
    (if (not xdata)
	(insert "No configuration possible.\n")
      
    (jabber-init-widget-buffer (jabber-xml-get-attribute xml-data 'from))
    (setq jabber-buffer-connection jc)

    (jabber-render-xdata-form xdata)

    (widget-create 'push-button :notify #'jabber-muc-submit-config "Submit")
    (widget-insert "\t")
    (widget-create 'push-button :notify #'jabber-muc-cancel-config "Cancel")
    (widget-insert "\n")

    (widget-setup)
    (widget-minor-mode 1))))

(defalias 'jabber-groupchat-render-config 'jabber-muc-render-config
  "Deprecated. See `jabber-muc-render-config' instead.")

(defun jabber-muc-submit-config (&rest ignore)
  "Submit MUC configuration form."

  (jabber-send-iq jabber-buffer-connection jabber-submit-to
		  "set"
		  `(query ((xmlns . "http://jabber.org/protocol/muc#owner"))
			  ,(jabber-parse-xdata-form))
		  #'jabber-report-success "MUC configuration"
		  #'jabber-report-success "MUC configuration"))

(defalias 'jabber-groupchat-submit-config 'jabber-muc-submit-config
  "Deprecated. See `jabber-muc-submit-config' instead.")

(defun jabber-muc-cancel-config (&rest ignore)
  "Cancel MUC configuration form."

  (jabber-send-iq jabber-buffer-connection jabber-submit-to
		  "set"
		  '(query ((xmlns . "http://jabber.org/protocol/muc#owner"))
			  (x ((xmlns . "jabber:x:data") (type . "cancel"))))
		  nil nil nil nil))

(defalias 'jabber-groupchat-cancel-config 'jabber-muc-cancel-config
  "Deprecated. See `jabber-muc-cancel-config' instead.")

(add-to-list 'jabber-jid-muc-menu
	     (cons "Join groupchat" 'jabber-muc-join))

(defun jabber-muc-join (jc group nickname &optional popup)
  "join a groupchat, or change nick.
In interactive calls, or if POPUP is true, switch to the
groupchat buffer."
  (interactive 
   (let ((account (jabber-read-account))
	 (group (jabber-read-jid-completing "group: ")))
     (list account group (jabber-muc-read-my-nickname account group) t)))

  ;; If the user is already in the room, we don't need as many checks.
  (if (or (assoc group *jabber-active-groupchats*)
	  ;; Or if the users asked us not to check disco info.
	  jabber-muc-disable-disco-check)
      (jabber-muc-join-3 jc group nickname nil popup)
    ;; Else, send a disco request to find out what we are connecting
    ;; to.
    (jabber-disco-get-info jc group nil #'jabber-muc-join-2
			   (list group nickname popup))))

(defalias 'jabber-groupchat-join 'jabber-muc-join
  "Deprecated. Use `jabber-muc-join' instead.")

(defun jabber-muc-join-2 (jc closure result)
  (destructuring-bind (group nickname popup) closure
    (let* ( ;; Either success...
	  (identities (car result))
	  (features (cadr result))
	  ;; ...or error
	  (condition (when (eq identities 'error) (jabber-error-condition result))))
      (cond
       ;; Maybe the room doesn't exist yet.
       ((eq condition 'item-not-found)
	(unless (or jabber-silent-mode
                    (y-or-n-p (format "%s doesn't exist.  Create it? "
                                      (jabber-jid-displayname group))))
	  (error "Non-existent groupchat")))

       ;; Maybe the room doesn't support disco.
       ((eq condition 'feature-not-implemented)
	t				;whatever... we will ignore it later
	)
       ;; Maybe another error occurred. Report it to user
       (condition
	(message "Couldn't query groupchat: %s" (jabber-parse-error result)))

       ;; Bad stanza? Without NS, for example
       ((and (eq identities 'error) (not condition))
        (message "Bad error stanza received")))

      ;; Continue only if it is really chat room.  If there was an
      ;; error, give the chat room the benefit of the doubt.  (Needed
      ;; for ejabberd's mod_irc, for example)
      (when (or condition
                (find "conference" (if (sequencep identities) identities nil)
                      :key (lambda (i) (aref i 1))
                      :test #'string=))
        (let ((password
	     ;; Is the room password-protected?
	     (when (member "muc_passwordprotected" features)
	       (or
		(jabber-get-conference-data jc group nil :password)
		(read-passwd (format "Password for %s: " (jabber-jid-displayname group)))))))

	(jabber-muc-join-3 jc group nickname password popup))))))

(defalias 'jabber-groupchat-join-2 'jabber-muc-join-2
  "Deprecated. See `jabber-muc-join-2' instead.")

(defun jabber-muc-join-3 (jc group nickname password popup)

  ;; Remember that this is a groupchat _before_ sending the stanza.
  ;; The response might come quicker than you think.

  (puthash (jabber-jid-symbol group) nickname jabber-pending-groupchats)
  
  (jabber-send-sexp jc
		    `(presence ((to . ,(format "%s/%s" group nickname)))
			       (x ((xmlns . "http://jabber.org/protocol/muc"))
				  ,@(when password
				      `((password () ,password))))
			       ,@(jabber-presence-children jc)))

  ;; There, stanza sent.  Now we just wait for the MUC service to
  ;; mirror the stanza.  This is handled in
  ;; `jabber-muc-process-presence', where a buffer will be created for
  ;; the room.

  ;; But if the user interactively asked to join, he/she probably
  ;; wants the buffer to pop up right now.
  (when popup
    (let ((buffer (jabber-muc-create-buffer jc group)))
      (switch-to-buffer buffer))))

(defalias 'jabber-groupchat-join-3 'jabber-muc-join-3
  "Deprecated. See `jabber-muc-join-3' instead.")

(defun jabber-muc-read-my-nickname (jc group &optional default)
  "Read nickname for joining GROUP. If DEFAULT is non-nil, return default nick without prompting."
  (let ((default-nickname (or
			   (jabber-get-conference-data jc group nil :nick)
			   (cdr (assoc group jabber-muc-default-nicknames))
			   (plist-get (fsm-get-state-data jc) :username))))
    (if default
        default-nickname
        (jabber-read-with-input-method (format "Nickname: (default %s) "
					   default-nickname) 
				   nil nil default-nickname))))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Change nickname" 'jabber-muc-nick))

(defalias 'jabber-muc-nick 'jabber-muc-join)

(add-to-list 'jabber-jid-muc-menu
	     (cons "Leave groupchat" 'jabber-muc-leave))

(defun jabber-muc-leave (jc group)
  "leave a groupchat"
  (interactive (jabber-muc-argument-list))
  (let ((whichgroup (assoc group *jabber-active-groupchats*)))
    ;; send unavailable presence to our own nick in room
    (jabber-send-sexp jc
		      `(presence ((to . ,(format "%s/%s" group (cdr whichgroup)))
				  (type . "unavailable"))))))

(defalias 'jabber-groupchat-leave 'jabber-muc-leave
  "Deprecated. Use `jabber-muc-leave' instead.")

(add-to-list 'jabber-jid-muc-menu
	     (cons "List participants" 'jabber-muc-names))

(defun jabber-muc-names ()
  "Print names, affiliations, and roles of participants in current buffer."
  (interactive)
  (ewoc-enter-last jabber-chat-ewoc (list :notice
					  (jabber-muc-print-names
					   (cdr (assoc jabber-group jabber-muc-participants)))
					  :time (current-time))))

(defun jabber-muc-format-names (participant)
  "Format one participant name"
  (format-spec jabber-muc-print-names-format
               (list
                (cons ?n (car participant))
                (cons ?a (plist-get (cdr participant) 'affiliation))
                (cons ?j (or (plist-get (cdr participant) 'jid) "")))))

(defun jabber-muc-print-names (participants)
  "Format and return data in PARTICIPANTS."
  (let ((mlist) (plist) (vlist) (nlist))
    (mapcar (lambda (x)
              (let ((role (plist-get (cdr x) 'role)))
                (cond ((string= role "moderator")
                       (add-to-list 'mlist x))
                      ((string= role "participant")
                       (add-to-list 'plist x))
                      ((string= role "visitor")
                       (add-to-list 'vlist x))
                      ((string= role "none")
                       (add-to-list 'nlist x)))))
            participants)
    (concat
     (apply 'concat "\nModerators:\n" (mapcar 'jabber-muc-format-names mlist))
     (apply 'concat "\nParticipants:\n" (mapcar 'jabber-muc-format-names plist))
     (apply 'concat "\nVisitors:\n" (mapcar 'jabber-muc-format-names vlist))
     (apply 'concat "\nNones:\n" (mapcar 'jabber-muc-format-names nlist)))
    ))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Set topic" 'jabber-muc-set-topic))

(defun jabber-muc-set-topic (jc group topic)
  "Set topic of GROUP to TOPIC."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-read-with-input-method "New topic: " jabber-muc-topic))))
  (jabber-send-message jc group topic nil "groupchat"))

(defun jabber-muc-snarf-topic (xml-data)
  "Record subject (topic) of the given <message/>, if any."
  (let ((new-topic (jabber-xml-path xml-data '(subject ""))))
    (when new-topic
      (setq jabber-muc-topic new-topic))))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Set role (kick, voice, op)" 'jabber-muc-set-role))

(defun jabber-muc-set-role (jc group nickname role reason)
  "Set role of NICKNAME in GROUP to ROLE, specifying REASON."
  (interactive
   (jabber-muc-argument-list
    (let ((nickname (jabber-muc-read-nickname jabber-group "Nickname: ")))
      (list nickname
	    (completing-read "New role: " '(("none") ("visitor") ("participant") ("moderator")) nil t nil 'jabber-role-history)
	    (read-string "Reason: ")))))
  (unless (or (zerop (length nickname)) (zerop (length role)))
    (jabber-send-iq jc group "set"
		    `(query ((xmlns . "http://jabber.org/protocol/muc#admin"))
			    (item ((nick . ,nickname)
				   (role . ,role))
				  ,(unless (zerop (length reason))
				     `(reason () ,reason))))
		    'jabber-report-success "Role change"
		    'jabber-report-success "Role change")))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Set affiliation (ban, member, admin)" 'jabber-muc-set-affiliation))

(defun jabber-muc-set-affiliation (jc group nickname-or-jid nickname-p affiliation reason)
  "Set affiliation of NICKNAME-OR-JID in GROUP to AFFILIATION.
If NICKNAME-P is non-nil, NICKNAME-OR-JID is a nickname in the
group, else it is a JID."
  (interactive
   (jabber-muc-argument-list
    (let ((nickname-p (y-or-n-p "Specify user by room nickname? ")))
      (list
       (if nickname-p
	   (jabber-muc-read-nickname jabber-group "Nickname: ")
	 (jabber-read-jid-completing "User: "))
       nickname-p
       (completing-read "New affiliation: "
			'(("none") ("outcast") ("member") ("admin") ("owner")) nil t nil 'jabber-affiliation-history)
       (read-string "Reason: ")))))
  (let ((jid
	 (if nickname-p
	     (let ((participants (cdr (assoc group jabber-muc-participants))))
	       (unless participants
		 (error "Couldn't find group %s" group))
	       (let ((participant (cdr (assoc nickname-or-jid participants))))
		 (unless participant
		   (error "Couldn't find %s in group %s" nickname-or-jid group))
		 (or (plist-get participant 'jid)
		     (error "JID of %s in group %s is unknown" nickname-or-jid group))))
	   nickname-or-jid)))
    (jabber-send-iq jc group "set"
		    `(query ((xmlns . "http://jabber.org/protocol/muc#admin"))
			    (item ((jid . ,jid)
				   (affiliation . ,affiliation))
				  ,(unless (zerop (length reason))
				     `(reason () ,reason))))
		    'jabber-report-success "Affiliation change"
		    'jabber-report-success "Affiliation change")))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Invite someone to chatroom" 'jabber-muc-invite))

(defun jabber-muc-invite (jc jid group reason)
  "Invite JID to GROUP, stating REASON."
  (interactive
   (list (jabber-read-account)
	 (jabber-read-jid-completing
          "Invite whom: "
          ;; The current room is _not_ a good default for whom to invite.
          (remq (jabber-jid-symbol jabber-group) (jabber-concat-rosters)))
	 (jabber-muc-read-completing "To group: ")
	 (jabber-read-with-input-method "Reason: ")))
  (jabber-send-sexp
   jc
   `(message ((to . ,group))
	     (x ((xmlns . "http://jabber.org/protocol/muc#user"))
		(invite ((to . ,jid))
			,(unless (zerop (length reason))
			   `(reason nil ,reason)))))))

(add-to-list 'jabber-body-printers 'jabber-muc-print-invite)

(defun jabber-muc-print-invite (xml-data who mode)
  "Print MUC invitation"
  (dolist (x (jabber-xml-get-children xml-data 'x))
    (when (string= (jabber-xml-get-attribute x 'xmlns) "http://jabber.org/protocol/muc#user")
      (let ((invitation (car (jabber-xml-get-children x 'invite))))
	(when invitation
	  (when (eql mode :insert)
	    (let ((group (jabber-xml-get-attribute xml-data 'from))
		  (inviter (jabber-xml-get-attribute invitation 'from))
		  (reason (car (jabber-xml-node-children (car (jabber-xml-get-children invitation 'reason))))))
	      ;; XXX: password
	      (insert "You have been invited to MUC room " (jabber-jid-displayname group))
	      (when inviter
		(insert " by " (jabber-jid-displayname inviter)))
	      (insert ".")
	      (when reason
		(insert "  Reason: " reason))
	      (insert "\n\n")

	      (let ((action
		     `(lambda (&rest ignore) (interactive)
			(jabber-muc-join jabber-buffer-connection ,group
					       (jabber-muc-read-my-nickname jabber-buffer-connection ,group)))))
		(if (fboundp 'insert-button)
		    (insert-button "Accept"
				   'action action)
		  ;; Simple button replacement
		  (let ((keymap (make-keymap)))
		    (define-key keymap "\r" action)
		    (insert (jabber-propertize "Accept"
					       'keymap keymap
					       'face 'highlight))))

		(insert "\t")

		(let ((action
		       `(lambda (&rest ignore) (interactive)
			  (let ((reason
				 (jabber-read-with-input-method
				  "Reason: ")))
			    (jabber-send-sexp
			     jabber-buffer-connection
			     (list 'message
				   (list (cons 'to ,group))
				   (list 'x
					 (list (cons 'xmlns "http://jabber.org/protocol/muc#user"))
					 (list 'decline
					       (list (cons 'to ,inviter))
					       (unless (zerop (length reason))
						 (list 'reason nil reason))))))))))
		  (if (fboundp 'insert-button)
		      (insert-button "Decline"
				     'action action)
		    ;; Simple button replacement
		    (let ((keymap (make-keymap)))
		      (define-key keymap "\r" action)
		      (insert (jabber-propertize "Decline"
						 'keymap keymap
						 'face 'highlight))))))))
	  (return t))))))

(defun jabber-muc-autojoin (jc)
  "Join rooms specified in account bookmarks and global `jabber-muc-autojoin'."
  (interactive (list (jabber-read-account)))
  (let ((nickname (plist-get (fsm-get-state-data jc) :username)))
    (when (bound-and-true-p jabber-muc-autojoin)
      (dolist (group jabber-muc-autojoin)
	(jabber-muc-join jc group (or
					 (cdr (assoc group jabber-muc-default-nicknames))
					 (plist-get (fsm-get-state-data jc) :username)))))
    (jabber-get-bookmarks
     jc
     (lambda (jc bookmarks)
       (dolist (bookmark bookmarks)
	 (setq bookmark (jabber-parse-conference-bookmark bookmark))
	 (when (and bookmark (plist-get bookmark :autojoin))
	   (jabber-muc-join jc (plist-get bookmark :jid)
				  (or (plist-get bookmark :nick)
				      (plist-get (fsm-get-state-data jc) :username)))))))))

;;;###autoload
(defun jabber-muc-message-p (message)
  "Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites."
  ;; Public groupchat messages have type "groupchat" and are from
  ;; room@server/nick.  Public groupchat errors have type "error" and
  ;; are from room@server.
  (let ((from (jabber-xml-get-attribute message 'from))
	(type (jabber-xml-get-attribute message 'type)))
    (or 
     (string= type "groupchat")
     (and (string= type "error")
	  (gethash (jabber-jid-symbol from) jabber-pending-groupchats))
     (jabber-xml-path message '(("http://jabber.org/protocol/muc#user" . "x") invite)))))

;;;###autoload
(defun jabber-muc-sender-p (jid)
  "Return non-nil if JID is a full JID of an MUC participant."
  (and (assoc (jabber-jid-user jid) *jabber-active-groupchats*)
       (jabber-jid-resource jid)))

;;;###autoload
(defun jabber-muc-private-message-p (message)
  "Return non-nil if MESSAGE is a private message in a groupchat."
  (let ((from (jabber-xml-get-attribute message 'from))
	(type (jabber-xml-get-attribute message 'type)))
    (and
     (not (string= type "groupchat"))
     (jabber-muc-sender-p from))))

(add-to-list 'jabber-jid-muc-menu
	     (cons "Open private chat" 'jabber-muc-private))

(defun jabber-muc-private (jc group nickname)
  "Open private chat with NICKNAME in GROUP."
  (interactive
   (jabber-muc-argument-list
    (list (jabber-muc-read-nickname jabber-group "Nickname: "))))
  (switch-to-buffer (jabber-muc-private-create-buffer jabber-buffer-connection group nickname)))

(defun jabber-muc-presence-p (presence)
  "Return non-nil if PRESENCE is presence from groupchat."
  (let ((from (jabber-xml-get-attribute presence 'from))
	(type (jabber-xml-get-attribute presence 'type))
	(muc-marker (find-if 
		     (lambda (x) (equal (jabber-xml-get-attribute x 'xmlns)
				   "http://jabber.org/protocol/muc#user"))
		     (jabber-xml-get-children presence 'x))))
    ;; This is MUC presence if it has an MUC-namespaced tag...
    (or muc-marker
	;; ...or if it is error presence from a room we tried to join.
	(and (string= type "error")
	     (gethash (jabber-jid-symbol from) jabber-pending-groupchats)))))

(defun jabber-muc-parse-affiliation (x-muc)
  "Parse X-MUC in the muc#user namespace and return a plist.
Return nil if X-MUC is nil."
  ;; XXX: parse <actor/> and <reason/> tags?  or maybe elsewhere?
  (apply 'nconc (mapcar (lambda (prop) (list (car prop) (cdr prop)))
			(jabber-xml-node-attributes
			 (car (jabber-xml-get-children x-muc 'item))))))

(defun jabber-muc-print-prompt (xml-data &optional local dont-print-nick-p)
  "Print MUC prompt for message in XML-DATA."
  (let ((nick (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
	(timestamp (jabber-message-timestamp xml-data)))
    (if (stringp nick)
	(insert (jabber-propertize
		 (format-spec jabber-groupchat-prompt-format
			      (list
			       (cons ?t (format-time-string 
					 (if timestamp
					     jabber-chat-delayed-time-format
					   jabber-chat-time-format)
					 timestamp))
			       (cons ?n (if dont-print-nick-p "" nick))
			       (cons ?u nick)
			       (cons ?r nick)
			       (cons ?j (concat jabber-group "/" nick))))
		 'face (if local        ;Message from you.
                           (if jabber-muc-colorize-local ;; If colorization enable...
                               ;; ...colorize nick
                               (list ':foreground (jabber-muc-nick-get-color nick))
                             ;; otherwise, use default face.
                             'jabber-chat-prompt-local)
                         ;; Message from other participant.
                         (if jabber-muc-colorize-foreign ;If colorization enable...
                             ;; ... colorize nick
                             (list ':foreground (jabber-muc-nick-get-color nick))
                           ;; otherwise, use default face.
                           'jabber-chat-prompt-foreign))
		 'help-echo (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " nick " in " jabber-group)))
      (jabber-muc-system-prompt))))

(defun jabber-muc-private-print-prompt (xml-data)
  "Print prompt for private MUC message in XML-DATA."
  (let ((nick (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
	(group (jabber-jid-user (jabber-xml-get-attribute xml-data 'from)))
	(timestamp (jabber-message-timestamp xml-data)))
    (insert (jabber-propertize
	     (format-spec jabber-muc-private-foreign-prompt-format
			  (list
			   (cons ?t (format-time-string 
				     (if timestamp
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n nick)
			   (cons ?g (or (jabber-jid-rostername group)
					(jabber-jid-username group)))))
	     'face 'jabber-chat-prompt-foreign
	     'help-echo (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " nick " in " jabber-group)))))

(defun jabber-muc-system-prompt (&rest ignore)
  "Print system prompt for MUC."
  (insert (jabber-propertize
	   (format-spec jabber-groupchat-prompt-format
			(list
			 (cons ?t (format-time-string jabber-chat-time-format))
			 (cons ?n "")
			 (cons ?u "")
			 (cons ?r "")
			 (cons ?j jabber-group)))
	   'face 'jabber-chat-prompt-system
	   'help-echo (format-time-string "System message on %Y-%m-%d %H:%M:%S"))))

(add-to-list 'jabber-message-chain 'jabber-muc-process-message)

(defun jabber-muc-process-message (jc xml-data)
  "If XML-DATA is a groupchat message, handle it as such."
  (when (jabber-muc-message-p xml-data)
    (let* ((from (jabber-xml-get-attribute xml-data 'from))
	   (group (jabber-jid-user from))
	   (nick (jabber-jid-resource from))
	   (error-p (jabber-xml-get-children xml-data 'error))
	   (type (cond 
		  (error-p :muc-error)
		  ((string= nick (cdr (assoc group *jabber-active-groupchats*)))
		   :muc-local)
		  (t :muc-foreign)))
	   (body-text (car (jabber-xml-node-children
			   (car (jabber-xml-get-children
				 xml-data 'body)))))

	   (printers (append jabber-muc-printers jabber-chat-printers)))

      (with-current-buffer (jabber-muc-create-buffer jc group)
	(jabber-muc-snarf-topic xml-data)
	;; Call alert hooks only when something is output
	(when (or error-p
		  (run-hook-with-args-until-success 'printers xml-data type :printp))
	  (jabber-maybe-print-rare-time
	   (ewoc-enter-last jabber-chat-ewoc (list type xml-data :time (current-time))))

	  ;; ...except if the message is part of history, in which
	  ;; case we don't want an alert.
	  (let ((children-namespaces (mapcar (lambda (x) (when (listp x) (jabber-xml-get-attribute x 'xmlns)))
					     (jabber-xml-node-children xml-data))))
	    (unless (or (member "urn:xmpp:delay" children-namespaces)
			(member "jabber:x:delay" children-namespaces))
	      (dolist (hook '(jabber-muc-hooks jabber-alert-muc-hooks))
		(run-hook-with-args hook
				    nick group (current-buffer) body-text
				    (funcall jabber-alert-muc-function
					     nick group (current-buffer) body-text))))))))))

(defun jabber-muc-process-presence (jc presence)
  (let* ((from (jabber-xml-get-attribute presence 'from))
	 (type (jabber-xml-get-attribute presence 'type))
	 (x-muc (find-if 
		 (lambda (x) (equal (jabber-xml-get-attribute x 'xmlns)
			       "http://jabber.org/protocol/muc#user"))
		 (jabber-xml-get-children presence 'x)))
	 (group (jabber-jid-user from))
	 (nickname (jabber-jid-resource from))
	 (symbol (jabber-jid-symbol from))
	 (our-nickname (gethash symbol jabber-pending-groupchats))
	 (item (car (jabber-xml-get-children x-muc 'item)))
	 (actor (jabber-xml-get-attribute (car (jabber-xml-get-children item 'actor)) 'jid))
	 (reason (car (jabber-xml-node-children (car (jabber-xml-get-children item 'reason)))))
	 (error-node (car (jabber-xml-get-children presence 'error)))
	 (status-codes (if error-node
			   (list (jabber-xml-get-attribute error-node 'code))
			 (mapcar
			  (lambda (status-element)
			    (jabber-xml-get-attribute status-element 'code))
			  (jabber-xml-get-children x-muc 'status)))))
    ;; handle leaving a room
    (cond 
     ((or (string= type "unavailable") (string= type "error"))
      ;; error from room itself? or are we leaving?
      (if (or (null nickname)
	      (member "110" status-codes)
	      (string= nickname our-nickname))
	  ;; Assume that an error means that we were thrown out of the
	  ;; room...
	  (let* ((leavingp t)
		 (message (cond
			   ((string= type "error")
			    (cond
			     ;; ...except for certain cases.
			     ((or (member "406" status-codes)
				  (member "409" status-codes))
			      (setq leavingp nil)
			      (concat "Nickname change not allowed"
				      (when error-node
					(concat ": " (jabber-parse-error error-node)))))
			     (t
			      (concat "Error entering room"
				      (when error-node
					(concat ": " (jabber-parse-error error-node)))))))
			   ((member "301" status-codes)
			    (concat "You have been banned"
				    (when actor (concat " by " actor))
				    (when reason (concat " - '" reason "'"))))
			   ((member "307" status-codes)
			    (concat "You have been kicked"
				    (when actor (concat " by " actor))
				    (when reason (concat " - '" reason "'"))))
			   (t
			    "You have left the chatroom"))))
	    (when leavingp
	      (jabber-muc-remove-groupchat group))
	    ;; If there is no buffer for this groupchat, don't bother
	    ;; creating one just to tell that user left the room.
	    (let ((buffer (get-buffer (jabber-muc-get-buffer group))))
	      (if buffer
		  (with-current-buffer buffer
		    (jabber-maybe-print-rare-time
		     (ewoc-enter-last jabber-chat-ewoc
				      (list (if (string= type "error")
						:muc-error
					      :muc-notice)
					    message
					    :time (current-time)))))
		(message "%s: %s" (jabber-jid-displayname group) message))))
	;; or someone else?
	(let* ((plist (jabber-muc-participant-plist group nickname))
	       (jid (plist-get plist 'jid))
	       (name (concat nickname
			     (when jid
			       (concat " <" 
				       (jabber-jid-user jid)
				       ">")))))
	  (jabber-muc-remove-participant group nickname)
	  (with-current-buffer (jabber-muc-create-buffer jc group)
	    (jabber-maybe-print-rare-time
	     (ewoc-enter-last
	      jabber-chat-ewoc
	      (list :muc-notice
		    (cond
		     ((member "301" status-codes)
		      (concat name " has been banned"
			      (when actor (concat " by " actor))
			      (when reason (concat " - '" reason "'"))))
		     ((member "307" status-codes)
		      (concat name " has been kicked"
			      (when actor (concat " by " actor))
			      (when reason (concat " - '" reason "'"))))
		     ((member "303" status-codes)
		      (concat name " changes nickname to "
			      (jabber-xml-get-attribute item 'nick)))
		     (t
		      (concat name " has left the chatroom")))
		    :time (current-time))))))))
     (t 
      ;; someone is entering

      (when (or (member "110" status-codes) (string= nickname our-nickname))
	;; This is us.  We just succeeded in entering the room.
	;;
	;; The MUC server is supposed to send a 110 code whenever this
	;; is our presence ("self-presence"), but at least one
	;; (ejabberd's mod_irc) doesn't, so check the nickname as well.
	;;
	;; This check might give incorrect results if the server
	;; changed our nickname to avoid collision with an existing
	;; participant, but even in this case the window where we have
	;; incorrect information should be very small, as we should be
	;; getting our own 110+210 presence shortly.
	(let ((whichgroup (assoc group *jabber-active-groupchats*)))
	  (if whichgroup
	      (setcdr whichgroup nickname)
	    (add-to-list '*jabber-active-groupchats* (cons group nickname))))
	;; The server may have changed our nick.  Record the new one.
	(puthash symbol nickname jabber-pending-groupchats))

      ;; Whoever enters, we create a buffer (if it didn't already
      ;; exist), and print a notice.  This is where autojoined MUC
      ;; rooms have buffers created for them.  We also remember some
      ;; metadata.
      (let ((old-plist (jabber-muc-participant-plist group nickname))
	    (new-plist (jabber-muc-parse-affiliation x-muc)))
	(jabber-muc-modify-participant group nickname new-plist)
	(let ((report (jabber-muc-report-delta nickname old-plist new-plist
					       reason actor)))
	  (when report
	    (with-current-buffer (jabber-muc-create-buffer jc group)
	      (jabber-maybe-print-rare-time
	       (ewoc-enter-last
		jabber-chat-ewoc
		(list :muc-notice report
		      :time (current-time))))
	      ;; Did the server change our nick?
	      (when (member "210" status-codes)
		(ewoc-enter-last
		 jabber-chat-ewoc
		 (list :muc-notice
		       (concat "Your nick was changed to " nickname " by the server")
		       :time (current-time))))
	      ;; Was this room just created?  If so, it's a locked
	      ;; room.  Notify the user.
	      (when (member "201" status-codes)
		(ewoc-enter-last
		 jabber-chat-ewoc
		 (list :muc-notice
		       (with-temp-buffer
			 (insert "This room was just created, and is locked to other participants.\n"
				 "To unlock it, ")
			 (insert-text-button
			  "configure the room"
			  'action (apply-partially 'call-interactively 'jabber-muc-get-config))
			 (insert " or ")
			 (insert-text-button
			  "accept the default configuration"
			  'action (apply-partially 'call-interactively 'jabber-muc-instant-config))
			 (insert ".")
			 (buffer-string))
		       :time (current-time))))))))))))
	      
(provide 'jabber-muc)

;;; arch-tag: 1ff7ab35-1717-46ae-b803-6f5b3fb2cd7d
