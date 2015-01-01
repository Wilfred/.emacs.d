;; jabber-roster.el - displaying the roster    -*- coding: utf-8; -*-

;; Copyright (C) 2009 - Kirill A. Korinskiy - catap@catap.ru
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

(require 'jabber-presence)
(require 'jabber-util)
(require 'jabber-alert)
(require 'jabber-keymap)
(require 'format-spec)
(require 'cl)				;for `find'
(require 'jabber-private)

(defgroup jabber-roster nil "roster display options"
  :group 'jabber)

(defcustom jabber-roster-line-format " %a %c %-25n %u %-8s  %S"
  "The format specification of the lines in the roster display.

These fields are available:

%a   Avatar, if any
%c   \"*\" if the contact is connected, or \" \" if not
%u   sUbscription state - see below
%n   Nickname of contact, or JID if no nickname
%j   Bare JID of contact (without resource)
%r   Highest-priority resource of contact
%s   Availability of contact as string (\"Online\", \"Away\" etc)
%S   Status string specified by contact

%u is replaced by one of the strings given by
`jabber-roster-subscription-display'."
  :type 'string
  :group 'jabber-roster)

(defcustom jabber-roster-subscription-display '(("none" . "   ")
						("from" . "<  ")
						("to" . "  >")
						("both" . "<->"))
  "Strings used for indicating subscription status of contacts.
\"none\" means that there is no subscription between you and the
contact.
\"from\" means that the contact has a subscription to you, but you
have no subscription to the contact.
\"to\" means that you have a subscription to the contact, but the
contact has no subscription to you.
\"both\" means a mutual subscription.

Having a \"presence subscription\" means being able to see the
other person's presence.

Some fancy arrows you might want to use, if your system can
display them: ← → ⇄ ↔"
  :type '(list (cons :format "%v" (const :format "" "none") (string :tag "None"))
	       (cons :format "%v" (const :format "" "from") (string :tag "From"))
	       (cons :format "%v" (const :format "" "to") (string :tag "To"))
	       (cons :format "%v" (const :format "" "both") (string :tag "Both")))
  :group 'jabber-roster)

(defcustom jabber-resource-line-format "     %r - %s (%S), priority %p"
  "The format specification of resource lines in the roster display.
These are displayed when `jabber-show-resources' permits it.

These fields are available:

%c   \"*\" if the contact is connected, or \" \" if not
%n   Nickname of contact, or JID if no nickname
%j   Bare JID of contact (without resource)
%p   Priority of this resource
%r   Name of this resource
%s   Availability of resource as string (\"Online\", \"Away\" etc)
%S   Status string specified by resource"
  :type 'string
  :group 'jabber-roster)

(defcustom jabber-roster-sort-functions
  '(jabber-roster-sort-by-status jabber-roster-sort-by-displayname)
  "Sort roster according to these criteria.

These functions should take two roster items A and B, and return:
<0 if A < B
0  if A = B
>0 if A > B"
  :type 'hook
  :options '(jabber-roster-sort-by-status
	     jabber-roster-sort-by-displayname
	     jabber-roster-sort-by-group)
  :group 'jabber-roster)

(defcustom jabber-sort-order '("chat" "" "away" "dnd" "xa")
  "Sort by status in this order.  Anything not in list goes last.
Offline is represented as nil."
  :type '(repeat (restricted-sexp :match-alternatives (stringp nil)))
  :group 'jabber-roster)

(defcustom jabber-show-resources 'sometimes
  "Show contacts' resources in roster?
This can be one of the following symbols:

nil       Never show resources
sometimes Show resources when there are more than one
always    Always show resources"
  :type '(radio (const :tag "Never" nil)
		(const :tag "When more than one connected resource" sometimes)
		(const :tag "Always" always))
  :group 'jabber-roster)

(defcustom jabber-show-offline-contacts t
  "Show offline contacts in roster when non-nil"
  :type 'boolean
  :group 'jabber-roster)

(defcustom jabber-remove-newlines t
  "Remove newlines in status messages?
Newlines in status messages mess up the roster display.  However,
they are essential to status message poets.  Therefore, you get to
choose the behaviour.

Trailing newlines are always removed, regardless of this variable."
  :type 'boolean
  :group 'jabber-roster)

(defcustom jabber-roster-show-bindings t
  "Show keybindings in roster buffer?"
  :type 'boolean
  :group 'jabber-roster)

(defcustom jabber-roster-show-title t
  "Show title in roster buffer?"
  :type 'boolean
  :group 'jabber-roster)

(defcustom jabber-roster-mode-hook nil
  "Hook run when entering Roster mode."
  :group 'jabber-roster
  :type 'hook)

(defcustom jabber-roster-default-group-name "other"
  "Default group name for buddies without groups."
  :group 'jabber-roster
  :type 'string
  :get '(lambda (var)
	  (let ((val (symbol-value var)))
	    (when (stringp val)
	      (set-text-properties 0 (length val) nil val))
	    val))
  :set '(lambda (var val)
          (when (stringp val)
	    (set-text-properties 0 (length val) nil val))
          (custom-set-default var val))
  )

(defcustom jabber-roster-show-empty-group nil
  "Show empty groups in roster?"
  :group 'jabber-roster
  :type 'boolean)

(defcustom jabber-roster-roll-up-group nil
  "Show empty groups in roster?"
  :group 'jabber-roster
  :type 'boolean)

(defface jabber-roster-user-online
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying online users"
  :group 'jabber-roster)

(defface jabber-roster-user-xa
  '((((background dark)) (:foreground "magenta" :weight normal :slant italic))
    (t (:foreground "black" :weight normal :slant italic)))
  "face for displaying extended away users"
  :group 'jabber-roster)

(defface jabber-roster-user-dnd
  '((t (:foreground "red" :weight normal :slant italic)))
  "face for displaying do not disturb users"
  :group 'jabber-roster)

(defface jabber-roster-user-away
  '((t (:foreground "dark green" :weight normal :slant italic)))
  "face for displaying away users"
  :group 'jabber-roster)

(defface jabber-roster-user-chatty
  '((t (:foreground "dark orange" :weight bold :slant normal)))
  "face for displaying chatty users"
  :group 'jabber-roster)

(defface jabber-roster-user-error
  '((t (:foreground "red" :weight light :slant italic)))
  "face for displaying users sending presence errors"
  :group 'jabber-roster)

(defface jabber-roster-user-offline
  '((t (:foreground "dark grey" :weight light :slant italic)))
  "face for displaying offline users"
  :group 'jabber-roster)

(defvar jabber-roster-debug nil
  "debug roster draw")

(defvar jabber-roster-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map jabber-common-keymap)
    (define-key map [mouse-2] 'jabber-roster-mouse-2-action-at-point)
    (define-key map (kbd "TAB") 'jabber-go-to-next-roster-item)
    (define-key map (kbd "S-TAB") 'jabber-go-to-previous-roster-item)
    (define-key map (kbd "M-TAB") 'jabber-go-to-previous-roster-item)
    (define-key map (kbd "<backtab>") 'jabber-go-to-previous-roster-item)
    (define-key map (kbd "RET") 'jabber-roster-ret-action-at-point)
    (define-key map (kbd "C-k") 'jabber-roster-delete-at-point)

    (define-key map "e" 'jabber-roster-edit-action-at-point)
    (define-key map "s" 'jabber-send-subscription-request)
    (define-key map "q" 'bury-buffer)
    (define-key map "i" 'jabber-get-disco-items)
    (define-key map "j" 'jabber-muc-join)
    (define-key map "I" 'jabber-get-disco-info)
    (define-key map "b" 'jabber-get-browse)
    (define-key map "v" 'jabber-get-version)
    (define-key map "a" 'jabber-send-presence)
    (define-key map "g" 'jabber-display-roster)
    (define-key map "S" 'jabber-ft-send)
    (define-key map "o" 'jabber-roster-toggle-offline-display)
    (define-key map "H" 'jabber-roster-toggle-binding-display)
    ;;(define-key map "D" 'jabber-disconnect)
    map))

(defun jabber-roster-ret-action-at-point ()
  "Action for ret. Before try to roll up/down group. Eval
chat-with-jid-at-point is no group at point"
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account))
        (jid-at-point (get-text-property (point)
					 'jabber-jid)))
    (if (and group-at-point account-at-point)
	(jabber-roster-roll-group account-at-point group-at-point)
      ;; Is this a normal contact, or a groupchat?  Let's ask it.
      (jabber-disco-get-info
       account-at-point (jabber-jid-user jid-at-point) nil
       #'jabber-roster-ret-action-at-point-1
       jid-at-point))))

(defun jabber-roster-ret-action-at-point-1 (jc jid result)
  ;; If we get an error, assume it's a normal contact.
  (if (eq (car result) 'error)
      (jabber-chat-with jc jid)
    ;; Otherwise, let's check whether it has a groupchat identity.
    (let ((identities (car result)))
      (if (find "conference" (if (sequencep identities) identities nil)
		:key (lambda (i) (aref i 1))
		:test #'string=)
	  ;; Yes!  Let's join it.
	  (jabber-muc-join jc jid
			   (jabber-muc-read-my-nickname jc jid t)
			   t)
	;; No.  Let's open a normal chat buffer.
	(jabber-chat-with jc jid)))))

(defun jabber-roster-mouse-2-action-at-point (e)
  "Action for mouse-2. Before try to roll up/down group. Eval
chat-with-jid-at-point is no group at point"
  (interactive "e")
  (mouse-set-point e)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(jabber-roster-roll-group account-at-point group-at-point)
      (jabber-popup-combined-menu))))

(defun jabber-roster-delete-at-point ()
  "Delete at point from roster.
Try to delete the group from all contaacs.
Delete a jid if there is no group at point."
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(let ((jids-with-group
	       (gethash group-at-point
			(plist-get
			 (fsm-get-state-data account-at-point)
			 :roster-hash))))
	  (jabber-roster-delete-group-from-jids account-at-point
						jids-with-group
						group-at-point))
      (jabber-roster-delete-jid-at-point))))

(defun jabber-roster-edit-action-at-point ()
  "Action for e. Before try to edit group name.
Eval `jabber-roster-change' is no group at point"
  (interactive)
  (let ((group-at-point (get-text-property (point)
					   'jabber-group))
	(account-at-point (get-text-property (point)
					     'jabber-account)))
    (if (and group-at-point account-at-point)
	(let ((jids-with-group
	       (gethash group-at-point
			(plist-get
			 (fsm-get-state-data account-at-point)
			 :roster-hash))))
	  (jabber-roster-edit-group-from-jids account-at-point
					      jids-with-group
					      group-at-point))
      (call-interactively 'jabber-roster-change))))

(defun jabber-roster-roll-group (jc group-name &optional set)
  "Roll up/down group in roster.
If optional SET is t, roll up group.
If SET is nor t or nil, roll down group."
  (let* ((state-data (fsm-get-state-data jc))
	 (roll-groups (plist-get state-data :roster-roll-groups))
         (new-roll-groups (if (find group-name roll-groups :test 'string=)
                              ;; group is rolled up, roll it down if needed
                              (if (or (not set) (and set (not (eq set t))))
                                  (remove-if-not (lambda (group-name-in-list)
                                                   (not (string= group-name
                                                                 group-name-in-list)))
                                                 roll-groups)
                                roll-groups)
                            ;; group is rolled down, roll it up if needed
                            (if (or (not set) (and set (eq set t)))
                                (append roll-groups (list group-name))
                              roll-groups))) )
    (unless (equal roll-groups new-roll-groups)
      (plist-put
       state-data :roster-roll-groups
       new-roll-groups)
      (jabber-display-roster))))

(defun jabber-roster-mode ()
  "Major mode for Jabber roster display.
Use the keybindings (mnemonic as Chat, Roster, Info, MUC, Service) to
bring up menus of actions.
\\{jabber-roster-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jabber-roster-mode
	mode-name "jabber-roster")
  (use-local-map jabber-roster-mode-map)
  (setq buffer-read-only t)
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'jabber-roster-mode-hook)
    (run-hooks 'jabber-roster-mode-hook)))

(put 'jabber-roster-mode 'mode-class 'special)

;;;###autoload
(defun jabber-switch-to-roster-buffer (&optional jc)
  "Switch to roster buffer.
Optional JC argument is ignored; it's there so this function can
be used in `jabber-post-connection-hooks'."
  (interactive)
  (if (not (get-buffer jabber-roster-buffer))
      (jabber-display-roster)
    (switch-to-buffer jabber-roster-buffer)))

(defun jabber-sort-roster (jc)
  "sort roster according to online status"
  (let ((state-data (fsm-get-state-data jc)))
    (dolist (group (plist-get state-data :roster-groups))
      (let ((group-name (car group)))
	(puthash group-name
		 (sort
		  (gethash group-name
			   (plist-get state-data :roster-hash))
		  #'jabber-roster-sort-items)
		 (plist-get state-data :roster-hash))))))

(defun jabber-roster-prepare-roster (jc)
  "make a hash based roster"
  (let* ((state-data (fsm-get-state-data jc))
	 (hash (make-hash-table :test 'equal))
	 (buddies (plist-get state-data :roster))
	 (all-groups '()))
    (dolist (buddy buddies)
      (let ((groups (get buddy 'groups)))
	(if groups
	    (progn
	      (dolist (group groups)
		(progn
		  (setq all-groups (append all-groups (list group)))
		  (puthash group
			   (append (gethash group hash)
				   (list buddy))
			   hash))))
	  (progn
	    (setq all-groups (append all-groups
				     (list jabber-roster-default-group-name)))
	    (puthash jabber-roster-default-group-name
		     (append (gethash jabber-roster-default-group-name hash)
			     (list buddy))
		     hash)))))

    ;; remove duplicates name of group
    (setq all-groups (sort
		      (remove-duplicates all-groups
					 :test 'string=)
		      'string<))

    ;; put to state-data all-groups as list of list
    (plist-put state-data :roster-groups
	       (mapcar #'list all-groups))

    ;; put to state-data hash-roster
    (plist-put state-data :roster-hash
	       hash)))

(defun jabber-roster-sort-items (a b)
  "Sort roster items A and B according to `jabber-roster-sort-functions'.
Return t if A is less than B."
  (dolist (fn jabber-roster-sort-functions)
    (let ((comparison (funcall fn a b)))
      (cond
       ((< comparison 0)
	(return t))
       ((> comparison 0)
	(return nil))))))

(defun jabber-roster-sort-by-status (a b)
  "Sort roster items by online status.
See `jabber-sort-order' for order used."
  (flet ((order (item) (length (member (get item 'show) jabber-sort-order))))
    (let ((a-order (order a))
	  (b-order (order b)))
      ;; Note reversed test.  Items with longer X-order go first.
      (cond
       ((< a-order b-order)
	1)
       ((> a-order b-order)
	-1)
       (t
	0)))))

(defun jabber-roster-sort-by-displayname (a b)
  "Sort roster items by displayed name."
  (let ((a-name (jabber-jid-displayname a))
	(b-name (jabber-jid-displayname b)))
    (cond
     ((string-lessp a-name b-name) -1)
     ((string= a-name b-name) 0)
     (t 1))))

(defun jabber-roster-sort-by-group (a b)
  "Sort roster items by group membership."
  (flet ((first-group (item) (or (car (get item 'groups)) "")))
    (let ((a-group (first-group a))
	  (b-group (first-group b)))
      (cond
       ((string-lessp a-group b-group) -1)
       ((string= a-group b-group) 0)
       (t 1)))))

(defun jabber-fix-status (status)
  "Make status strings more readable"
  (when status
    (when (string-match "\n+$" status)
      (setq status (replace-match "" t t status)))
    (when jabber-remove-newlines
      (while (string-match "\n" status)
	(setq status (replace-match " " t t status))))
    status))

(defvar jabber-roster-ewoc nil
  "Ewoc displaying the roster.
There is only one; we don't rely on buffer-local variables or
such.")

(defun jabber-roster-filter-display (buddies)
  "Filter BUDDIES for items to be displayed in the roster"
  (remove-if-not (lambda (buddy) (or jabber-show-offline-contacts
				     (get buddy 'connected)))
		 buddies))

(defun jabber-roster-toggle-offline-display ()
  "Toggle display of offline contacts.
To change this permanently, customize the `jabber-show-offline-contacts'."
  (interactive)
  (setq jabber-show-offline-contacts
	(not jabber-show-offline-contacts))
  (jabber-display-roster))

(defun jabber-roster-toggle-binding-display ()
  "Toggle display of the roster binding text."
  (interactive)
  (setq jabber-roster-show-bindings
	(not jabber-roster-show-bindings))
  (jabber-display-roster))

(defun jabber-display-roster ()
  "switch to the main jabber buffer and refresh the roster display to reflect the current information"
  (interactive)
  (with-current-buffer (get-buffer-create jabber-roster-buffer)
    (if (not (eq major-mode 'jabber-roster-mode))
	(jabber-roster-mode))
    (setq buffer-read-only nil)
    ;; line-number-at-pos is in Emacs >= 21.4.  Only used to avoid
    ;; excessive scrolling when updating roster, so not absolutely
    ;; necessary.
    (let ((current-line (and (fboundp 'line-number-at-pos) (line-number-at-pos)))
	  (current-column (current-column)))
      (erase-buffer)
      (setq jabber-roster-ewoc nil)
      (when jabber-roster-show-title
	(insert (jabber-propertize "Jabber roster" 'face 'jabber-title-large) "\n"))
      (when jabber-roster-show-bindings
	(insert "RET      Open chat buffer        C-k      Delete roster item
e        Edit item               s        Send subscription request
q        Bury buffer             i        Get disco items
I        Get disco info          b        Browse
j        Join groupchat (MUC)    v        Get client version
a        Send presence           o        Show offline contacts on/off
C-c C-c  Chat menu               C-c C-m  Multi-User Chat menu
C-c C-i  Info menu               C-c C-r  Roster menu
C-c C-s  Service menu

H        Toggle displaying this text
"))
      (insert "__________________________________\n\n")
      (if (null jabber-connections)
	  (insert "Not connected\n")
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-2] #'jabber-send-presence)
	  (insert (jabber-propertize (concat (format " - %s"
						     (cdr (assoc *jabber-current-show* jabber-presence-strings)))
					     (if (not (zerop (length *jabber-current-status*)))
						 (format " (%s)"
							 (jabber-fix-status *jabber-current-status*)))
					     " -")
				     'face (or (cdr (assoc *jabber-current-show* jabber-presence-faces))
					       'jabber-roster-user-online)
				     ;;'mouse-face (cons 'background-color "light grey")
				     'keymap map)
		  "\n")))

      (dolist (jc jabber-connections)
	;; use a hash-based roster
	(when (not (plist-get (fsm-get-state-data jc) :roster-hash))
	  (jabber-roster-prepare-roster jc))
	;; We sort everything before putting it in the ewoc
	(jabber-sort-roster jc)
	(let ((before-ewoc (point))
	      (ewoc (ewoc-create
		       (lexical-let ((jc jc))
			 (lambda (data)
			   (let* ((group (car data))
				  (group-name (car group))
				  (buddy (car (cdr data))))
			     (jabber-display-roster-entry jc group-name buddy))))
		     (concat
		      (jabber-propertize (concat
					  (plist-get (fsm-get-state-data jc) :username)
					  "@"
					  (plist-get (fsm-get-state-data jc) :server))
					 'face 'jabber-title-medium)
		      "\n__________________________________\n")
		     "__________________________________"))
	      (new-groups '()))
	  (plist-put(fsm-get-state-data jc) :roster-ewoc ewoc)
	  (dolist (group (plist-get (fsm-get-state-data jc) :roster-groups))
	    (let* ((group-name (car group))
		   (buddies (jabber-roster-filter-display
			    (gethash group-name
				     (plist-get (fsm-get-state-data jc) :roster-hash)))))
	      (when (or jabber-roster-show-empty-group
			(> (length buddies) 0))
		(let ((group-node (ewoc-enter-last ewoc (list group nil))))
		  (if (not (find
			    group-name
			    (plist-get (fsm-get-state-data jc) :roster-roll-groups)
			    :test 'string=))
		      (dolist (buddy (reverse buddies))
			(ewoc-enter-after ewoc group-node (list group buddy))))))))
	  (goto-char (point-max))
	  (insert "\n")
	  (put-text-property before-ewoc (point)
			     'jabber-account jc)))

      (goto-char (point-min))
      (setq buffer-read-only t)
      (if (interactive-p)
	  (dolist (hook '(jabber-info-message-hooks jabber-alert-info-message-hooks))
	    (run-hook-with-args hook 'roster (current-buffer) (funcall jabber-alert-info-message-function 'roster (current-buffer)))))
      (when current-line
	;; Go back to previous line - don't use goto-line, since it
	;; sets the mark.
	(goto-char (point-min))
	(forward-line (1- current-line))
	;; ...and go back to previous column
	(move-to-column current-column)))))

(defun jabber-display-roster-entry (jc group-name buddy)
  "Format and insert a roster entry for BUDDY at point.
BUDDY is a JID symbol."
  (if buddy
      (let ((buddy-str (format-spec
			jabber-roster-line-format
			(list
			 (cons ?a (jabber-propertize
				   " "
				   'display (get buddy 'avatar)))
			 (cons ?c (if (get buddy 'connected) "*" " "))
			 (cons ?u (cdr (assoc
					(or
					 (get buddy 'subscription) "none")
					jabber-roster-subscription-display)))
			 (cons ?n (if (> (length (get buddy 'name)) 0)
				      (get buddy 'name)
				    (symbol-name buddy)))
			 (cons ?j (symbol-name buddy))
			 (cons ?r (or (get buddy 'resource) ""))
			 (cons ?s (or
				   (cdr (assoc (get buddy 'show)
					       jabber-presence-strings))
				   (get buddy 'show)))
			 (cons ?S (if (get buddy 'status)
				      (jabber-fix-status (get buddy 'status))
				    ""))
			 ))))
	(add-text-properties 0
			     (length buddy-str)
			     (list
			      'face
			      (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
				  'jabber-roster-user-online)
			      ;;'mouse-face
			      ;;(cons 'background-color "light grey")
			      'help-echo
			      (symbol-name buddy)
			      'jabber-jid
			      (symbol-name buddy)
			      'jabber-account
			      jc)
			     buddy-str)
	(insert buddy-str)

	(when (or (eq jabber-show-resources 'always)
		  (and (eq jabber-show-resources 'sometimes)
		       (> (jabber-count-connected-resources buddy) 1)))
	  (dolist (resource (get buddy 'resources))
	    (when (plist-get (cdr resource) 'connected)
	      (let ((resource-str (format-spec jabber-resource-line-format
					       (list
						(cons ?c "*")
						(cons ?n (if (>
							      (length
							       (get buddy 'name)) 0)
							     (get buddy 'name)
							   (symbol-name buddy)))
						(cons ?j (symbol-name buddy))
						(cons ?r (if (>
							      (length
							       (car resource)) 0)
							     (car resource)
							   "empty"))
						(cons ?s (or
							  (cdr (assoc
								(plist-get
								 (cdr resource) 'show)
								jabber-presence-strings))
							  (plist-get
							   (cdr resource) 'show)))
						(cons ?S (if (plist-get
							      (cdr resource) 'status)
							     (jabber-fix-status
							      (plist-get (cdr resource)
									 'status))
							   ""))
						(cons ?p (number-to-string
							  (plist-get (cdr resource)
								     'priority)))))))
		(add-text-properties 0
				     (length resource-str)
				     (list
				      'face
				      (or (cdr (assoc (plist-get
						       (cdr resource)
						       'show)
						      jabber-presence-faces))
					  'jabber-roster-user-online)
				      'jabber-jid
				      (format "%s/%s" (symbol-name buddy) (car resource))
				      'jabber-account
				      jc)
				     resource-str)
		(insert "\n" resource-str))))))
    (let ((group-name (or group-name
			  jabber-roster-default-group-name)))
      (add-text-properties 0
			   (length group-name)
			   (list
			    'face 'jabber-title-small
			    'jabber-group group-name
			    'jabber-account jc)
			   group-name)
      (insert group-name))))

;;;###autoload
(defun jabber-roster-update (jc new-items changed-items deleted-items)
  "Update roster, in memory and on display.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols."
  (let* ((roster (plist-get (fsm-get-state-data jc) :roster))
	 (hash (plist-get (fsm-get-state-data jc) :roster-hash))
	 (ewoc (plist-get (fsm-get-state-data jc) :roster-ewoc))
	 (all-groups (plist-get (fsm-get-state-data jc) :roster-groups))
	 (terminator
	  (lambda (deleted-items)
	    (dolist (delete-this deleted-items)
	      (let ((groups (get delete-this 'groups))
		    (terminator
		     (lambda (g)
		       (let*
			   ((group (or g jabber-roster-default-group-name))
			    (buddies (gethash group hash)))
			 (when (not buddies)
			   (setq new-groups (append new-groups (list group))))
			 (puthash group
				  (delq delete-this buddies)
				  hash)))))
		(if groups
		    (dolist (group groups)
		      (terminator group))
		  (terminator groups)))))))

    ;; fix a old-roster
    (dolist (delete-this deleted-items)
      (setq roster (delq delete-this roster)))
    (setq roster (append new-items roster))
    (plist-put (fsm-get-state-data jc) :roster roster)

    ;; update a hash-roster
    (if (not hash)
	(jabber-roster-prepare-roster jc)

      (when jabber-roster-debug
	(message "update hash-based roster"))

      ;; delete items
      (dolist (delete-this (append deleted-items changed-items))
	(let ((jid (symbol-name delete-this)))
	  (when jabber-roster-debug
	    (message (concat "delete jid: " jid)))
	  (dolist (group (mapcar (lambda (g) (car g)) all-groups))
	    (when jabber-roster-debug
	      (message (concat "try to delete jid: " jid " from group " group)))
	    (puthash group
		     (delq delete-this (gethash group hash))
		     hash))))

      ;; insert changed-items
      (dolist (insert-this (append changed-items new-items))
	(let ((jid (symbol-name insert-this)))
	  (when jabber-roster-debug
	    (message (concat "insert jid: " jid)))
	  (dolist (group (or (get insert-this 'groups)
			     (list jabber-roster-default-group-name)))
	    (when jabber-roster-debug
	      (message (concat "insert jid: " jid " to group " group)))
	    (puthash group
		     (append (gethash group hash)
			     (list insert-this))
		     hash)
	    (setq all-groups (append all-groups (list (list group)))))))

      
      (when jabber-roster-debug
	(message "remove duplicates from new group"))
      (setq all-groups (sort
			(remove-duplicates all-groups
					   :test (lambda (g1 g2)
						   (let ((g1-name (car g1))
							 (g2-name (car g2)))
						     (string= g1-name
							      g2-name))))
			(lambda (g1 g2)
			  (let ((g1-name (car g1))
				(g2-name (car g2)))
			    (string< g1-name
				     g2-name)))))

      (plist-put (fsm-get-state-data jc) :roster-groups all-groups))


    (when jabber-roster-debug
      (message "re display roster"))

    ;; recreate roster buffer
    (jabber-display-roster)))

(defalias 'jabber-presence-update-roster 'ignore)
;;jabber-presence-update-roster is not needed anymore.
;;Its work is done in `jabber-process-presence'."
(make-obsolete 'jabber-presence-update-roster 'ignore)

(defun jabber-next-property (&optional prev)
  "Return position of next property appearence or nil if there is none.
If optional PREV is non-nil, return position of previous property appearence."
  (let ((pos (point))
        (found nil)
        (nextprev (if prev 'previous-single-property-change
                    'next-single-property-change)))
    (while (not found)
      (setq pos
            (let ((jid (funcall nextprev pos 'jabber-jid))
                  (group (funcall nextprev pos 'jabber-group)))
              (cond
               ((not jid) group)
               ((not group) jid)
               (t (funcall (if prev 'max 'min) jid group)))))
      (if (not pos)
          (setq found t)
        (setq found (or (get-text-property pos 'jabber-jid)
                        (get-text-property pos 'jabber-group)))))
    pos))

(defun jabber-go-to-next-roster-item ()
  "Move the cursor to the next jid/group in the buffer"
  (interactive)
  (let* ((next (jabber-next-property))
         (next (if (not next)
                   (progn (goto-char (point-min))
                          (jabber-next-property)) next)))
    (if next (goto-char next)
      (goto-char (point-min)))))

(defun jabber-go-to-previous-roster-item ()
  "Move the cursor to the previous jid/group in the buffer"
  (interactive)
  (let* ((previous (jabber-next-property 'prev))
         (previous (if (not previous)
                       (progn (goto-char (point-max))
                              (jabber-next-property 'prev)) previous)))
    (if previous (goto-char previous)
      (goto-char (point-max)))))

(defun jabber-roster-restore-groups (jc)
  "Restore roster's groups rolling state from private storage"
  (interactive (list (jabber-read-account)))
  (jabber-private-get jc 'roster "emacs-jabber"
                      'jabber-roster-restore-groups-1 'ignore))

(defun jabber-roster-restore-groups-1 (jc xml-data)
  "Parse roster groups and restore rolling state"
  (when (string= (jabber-xml-get-xmlns xml-data) "emacs-jabber")
    (let* ((data (car (last xml-data)))
           (groups (if (stringp data) (split-string data "\n") nil)))
      (dolist (group groups)
        (jabber-roster-roll-group jc group t)))))

(defun jabber-roster-save-groups ()
  "Save roster's groups rolling state in private storage"
  (interactive)
  (dolist (jc jabber-connections)
    (let* ((groups (plist-get (fsm-get-state-data jc) :roster-roll-groups))
           (roll-groups
            (if groups
                (mapconcat (lambda (a) (substring-no-properties a)) groups "\n")
              "")))
      (jabber-private-set jc
                          `(roster ((xmlns . "emacs-jabber"))
                                   ,roll-groups)
                          'jabber-report-success "Roster groups saved"
                          'jabber-report-success "Failed to save roster groups"))))

(provide 'jabber-roster)

;;; arch-tag: 096af063-0526-4dd2-90fd-bc6b5ba07d32
