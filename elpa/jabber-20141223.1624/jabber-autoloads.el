;;; jabber-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "jabber" "jabber.el" (21669 55012 932581 581000))
;;; Generated autoloads from jabber.el

(defvar jabber-account-list nil "\
List of Jabber accounts.
Each element of the list is a cons cell describing a Jabber account,
where the car is a JID and the CDR is an alist.

JID is a full Jabber ID string (e.g. foo@bar.tld). You can also
specify the resource (e.g. foo@bar.tld/emacs).
The following keys can be present in the alist:
:password is a string to authenticate ourself against the server.
It can be empty.
:network-server is a string identifying the address to connect to,
if it's different from the server part of the JID.
:port is the port to use (default depends on connection type).
:connection-type is a symbol. Valid symbols are `starttls',
`network' and `ssl'.

Only JID is mandatory.  The rest can be guessed at run-time.

Examples:

Two accounts without any special configuration:
\((\"foo@example.com\") (\"bar@example.net\"))

One disabled account with a non-standard port:
\((\"romeo@montague.net\" (:port . 5242) (:disabled . t)))

If you don't have SRV and STARTTLS capabilities in your Emacs,
configure a Google Talk account like this:
\((\"username@gmail.com\" 
  (:network-server . \"talk.google.com\")
  (:connection-type . ssl)))")

(custom-autoload 'jabber-account-list "jabber" t)

(defvar *jabber-current-status* nil "\
the users current presence status")

(defvar *jabber-current-show* nil "\
the users current presence show")

(defvar *jabber-current-priority* nil "\
the user's current priority")

(defconst jabber-presence-faces '(("" . jabber-roster-user-online) ("away" . jabber-roster-user-away) ("xa" . jabber-roster-user-xa) ("dnd" . jabber-roster-user-dnd) ("chat" . jabber-roster-user-chatty) ("error" . jabber-roster-user-error) (nil . jabber-roster-user-offline)) "\
Mapping from presence types to faces")

(autoload 'jabber-customize "jabber" "\
customize jabber options

\(fn)" t nil)

(autoload 'jabber-info "jabber" "\
open jabber.el manual

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jabber-activity" "jabber-activity.el" (21669
;;;;;;  55013 492555 542000))
;;; Generated autoloads from jabber-activity.el

(defvar jabber-activity-mode t "\
Non-nil if Jabber-Activity mode is enabled.
See the command `jabber-activity-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `jabber-activity-mode'.")

(custom-autoload 'jabber-activity-mode "jabber-activity" nil)

(autoload 'jabber-activity-mode "jabber-activity" "\
Toggle display of activity in hidden jabber buffers in the mode line.

With a numeric arg, enable this display if arg is positive.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "jabber-autoaway" "jabber-autoaway.el" (21669
;;;;;;  55013 72575 67000))
;;; Generated autoloads from jabber-autoaway.el

(autoload 'jabber-autoaway-start "jabber-autoaway" "\
Start autoaway timer.
The IGNORED argument is there so you can put this function in
`jabber-post-connect-hooks'.

\(fn &optional IGNORED)" t nil)

;;;***

;;;### (autoloads nil "jabber-bookmarks" "jabber-bookmarks.el" (21669
;;;;;;  55012 812587 167000))
;;; Generated autoloads from jabber-bookmarks.el

(autoload 'jabber-get-conference-data "jabber-bookmarks" "\
Get bookmark data for CONFERENCE-JID.
KEY may be nil or one of :name, :autojoin, :nick and :password.
If KEY is nil, a plist containing the above keys is returned.
CONT is called when the result is available, with JC and the
result as arguments.  If CONT is nil, return the requested data
immediately, and return nil if it is not in the cache.

\(fn JC CONFERENCE-JID CONT &optional KEY)" nil nil)

(autoload 'jabber-parse-conference-bookmark "jabber-bookmarks" "\
Convert a <conference/> tag into a plist.
The plist may contain the keys :jid, :name, :autojoin,
:nick and :password.

\(fn NODE)" nil nil)

(autoload 'jabber-get-bookmarks "jabber-bookmarks" "\
Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and the bookmark list.  CONT will be
called as the result of a filter function or a timer.
If REFRESH is non-nil, always fetch bookmarks.

\(fn JC CONT &optional REFRESH)" nil nil)

(autoload 'jabber-get-bookmarks-from-cache "jabber-bookmarks" "\
Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil.

\(fn JC)" nil nil)

(autoload 'jabber-edit-bookmarks "jabber-bookmarks" "\
Create a buffer for editing bookmarks interactively.

\(fn JC)" t nil)

;;;***

;;;### (autoloads nil "jabber-chat" "jabber-chat.el" (21669 55012
;;;;;;  652594 616000))
;;; Generated autoloads from jabber-chat.el

(defvar jabber-chatting-with nil "\
JID of the person you are chatting with")

(autoload 'jabber-chat-get-buffer "jabber-chat" "\
Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn CHAT-WITH)" nil nil)

;;;***

;;;### (autoloads nil "jabber-chatbuffer" "jabber-chatbuffer.el"
;;;;;;  (21669 55013 852538 798000))
;;; Generated autoloads from jabber-chatbuffer.el

(defvar jabber-buffer-connection nil "\
The connection used by this buffer.")

(make-variable-buffer-local 'jabber-buffer-connection)

;;;***

;;;### (autoloads nil "jabber-compose" "jabber-compose.el" (21669
;;;;;;  55012 22623 935000))
;;; Generated autoloads from jabber-compose.el

(autoload 'jabber-compose "jabber-compose" "\
Create a buffer for composing a Jabber message.

\(fn JC &optional RECIPIENT)" t nil)

;;;***

;;;### (autoloads nil "jabber-console" "jabber-console.el" (21669
;;;;;;  55012 592597 409000))
;;; Generated autoloads from jabber-console.el

(autoload 'jabber-process-console "jabber-console" "\
Log XML-DATA i/o as XML in \"*-jabber-console-JID-*\" buffer

\(fn JC DIRECTION XML-DATA)" nil nil)

;;;***

;;;### (autoloads nil "jabber-core" "jabber-core.el" (21669 55015
;;;;;;  12484 856000))
;;; Generated autoloads from jabber-core.el
 (autoload 'jabber-connect-all "jabber" "Connect to all configured Jabber accounts.\nSee `jabber-account-list'.\nIf no accounts are configured (or ARG supplied), call `jabber-connect' interactively." t)
 (autoload 'jabber-connect "jabber" "Connect to the Jabber server and start a Jabber XML stream.\nWith prefix argument, register a new account.\nWith double prefix argument, specify more connection details." t)

;;;***

;;;### (autoloads nil "jabber-disco" "jabber-disco.el" (21669 55011
;;;;;;  982625 795000))
;;; Generated autoloads from jabber-disco.el

(eval-after-load "jabber-core" '(add-to-list 'jabber-presence-chain #'jabber-process-caps))

(autoload 'jabber-process-caps "jabber-disco" "\
Look for entity capabilities in presence stanzas.

\(fn JC XML-DATA)" nil nil)

(autoload 'jabber-disco-advertise-feature "jabber-disco" "\


\(fn FEATURE)" nil nil)

(autoload 'jabber-caps-presence-element "jabber-disco" "\


\(fn JC)" nil nil)

(eval-after-load "jabber-presence" '(add-to-list 'jabber-presence-element-functions #'jabber-caps-presence-element))

;;;***

;;;### (autoloads nil "jabber-export" "jabber-export.el" (21669 55014
;;;;;;  572505 306000))
;;; Generated autoloads from jabber-export.el

(autoload 'jabber-export-roster "jabber-export" "\
Export roster for connection JC.

\(fn JC)" t nil)

(autoload 'jabber-import-roster "jabber-export" "\
Create buffer for roster import for connection JC from FILE.

\(fn JC FILE)" t nil)

;;;***

;;;### (autoloads nil "jabber-gmail" "jabber-gmail.el" (21669 55014
;;;;;;  902489 964000))
;;; Generated autoloads from jabber-gmail.el

(autoload 'jabber-gmail-subscribe "jabber-gmail" "\
Subscribe to gmail notifications.
See http://code.google.com/apis/talk/jep_extensions/usersettings.html#4

\(fn JC)" t nil)

(autoload 'jabber-gmail-query "jabber-gmail" "\
Request mail information from the Google Talk server (a.k.a. one shot query).
See http://code.google.com/apis/talk/jep_extensions/gmail.html#requestmail

\(fn JC)" t nil)

;;;***

;;;### (autoloads nil "jabber-keepalive" "jabber-keepalive.el" (21669
;;;;;;  55013 812540 661000))
;;; Generated autoloads from jabber-keepalive.el

(let ((loads (get 'jabber-keepalive 'custom-loads))) (if (member '"jabber-keepalive" loads) nil (put 'jabber-keepalive 'custom-loads (cons '"jabber-keepalive" loads))))

(autoload 'jabber-keepalive-start "jabber-keepalive" "\
Activate keepalive.
That is, regularly send a ping request to the server, and
disconnect if it doesn't answer.  See `jabber-keepalive-interval'
and `jabber-keepalive-timeout'.

The JC argument makes it possible to add this function to
`jabber-post-connect-hooks'; it is ignored.  Keepalive is activated
for all accounts regardless of the argument.

\(fn &optional JC)" t nil)

(autoload 'jabber-whitespace-ping-start "jabber-keepalive" "\
Start sending whitespace pings at regular intervals.
See `jabber-whitespace-ping-interval'.

The JC argument is ignored; whitespace pings are enabled for all
accounts.

\(fn &optional JC)" t nil)

;;;***

;;;### (autoloads nil "jabber-keymap" "jabber-keymap.el" (21669 55012
;;;;;;  252613 240000))
;;; Generated autoloads from jabber-keymap.el

(defvar jabber-global-keymap (let ((map (make-sparse-keymap))) (define-key map "" 'jabber-connect-all) (define-key map "" 'jabber-disconnect) (define-key map "" 'jabber-switch-to-roster-buffer) (define-key map "\n" 'jabber-chat-with) (define-key map "\f" 'jabber-activity-switch-to) (define-key map "" 'jabber-send-away-presence) (define-key map "" 'jabber-send-default-presence) (define-key map "" 'jabber-send-xa-presence) (define-key map "" 'jabber-send-presence) map) "\
Global Jabber keymap (usually under C-x C-j)")

(define-key ctl-x-map "\n" jabber-global-keymap)

;;;***

;;;### (autoloads nil "jabber-menu" "jabber-menu.el" (21669 55012
;;;;;;  382607 179000))
;;; Generated autoloads from jabber-menu.el

(defvar jabber-menu (let ((map (make-sparse-keymap "jabber-menu"))) (define-key-after map [jabber-menu-connect] '("Connect" . jabber-connect-all)) (define-key-after map [jabber-menu-disconnect] '(menu-item "Disconnect" jabber-disconnect :enable (bound-and-true-p jabber-connections))) (define-key-after map [jabber-menu-status] `(menu-item "Set Status" ,(make-sparse-keymap "set-status") :enable (bound-and-true-p jabber-connections))) (define-key map [jabber-menu-status jabber-menu-status-chat] '(menu-item "Chatty" (lambda nil (interactive) (jabber-send-presence "chat" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*)) :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "chat")))) (define-key map [jabber-menu-status jabber-menu-status-dnd] '(menu-item "Do not Disturb" (lambda nil (interactive) (jabber-send-presence "dnd" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*)) :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "dnd")))) (define-key map [jabber-menu-status jabber-menu-status-xa] '(menu-item "Extended Away" jabber-send-xa-presence :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "xa")))) (define-key map [jabber-menu-status jabber-menu-status-away] '(menu-item "Away" jabber-send-away-presence :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "away")))) (define-key map [jabber-menu-status jabber-menu-status-online] '(menu-item "Online" jabber-send-default-presence :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "")))) (define-key-after map [separator] '(menu-item "--")) (define-key-after map [jabber-menu-chat-with] '(menu-item "Chat with..." jabber-chat-with :enable (bound-and-true-p jabber-connections))) (define-key-after map [jabber-menu-nextmsg] '(menu-item "Next unread message" jabber-activity-switch-to :enable (bound-and-true-p jabber-activity-jids))) (define-key-after map [jabber-menu-send-subscription-request] '(menu-item "Send subscription request" jabber-send-subscription-request :enable (bound-and-true-p jabber-connections))) (define-key-after map [jabber-menu-roster] '("Switch to roster" . jabber-switch-to-roster-buffer)) (define-key-after map [separator2] '(menu-item "--")) (define-key-after map [jabber-menu-customize] '("Customize" . jabber-customize)) (define-key-after map [jabber-menu-info] '("Help" . jabber-info)) map))

(defvar jabber-display-menu 'maybe "\
Decide whether the \"Jabber\" menu is displayed in the menu bar.
If t, always display.
If nil, never display.
If maybe, display if jabber.el is installed under `package-user-dir', or
if any of `jabber-account-list' or `jabber-connections' is non-nil.")

(custom-autoload 'jabber-display-menu "jabber-menu" t)

(define-key-after (lookup-key global-map [menu-bar]) [jabber-menu] (list 'menu-item "Jabber" jabber-menu :visible (let ((user-installed-package (and (bound-and-true-p package-user-dir) (string= (file-name-as-directory (expand-file-name ".." (file-name-directory load-file-name))) (file-name-as-directory (expand-file-name package-user-dir)))))) `(or (eq jabber-display-menu t) (and (eq jabber-display-menu 'maybe) (or ,user-installed-package jabber-account-list (bound-and-true-p jabber-connections)))))))

;;;***

;;;### (autoloads nil "jabber-muc" "jabber-muc.el" (21669 55013 442557
;;;;;;  860000))
;;; Generated autoloads from jabber-muc.el

(defvar *jabber-active-groupchats* nil "\
alist of groupchats and nicknames
Keys are strings, the bare JID of the room.
Values are strings.")

(defvar jabber-muc-printers 'nil "\
List of functions that may be able to print part of a MUC message.
This gets prepended to `jabber-chat-printers', which see.")

(autoload 'jabber-muc-get-buffer "jabber-muc" "\
Return the chat buffer for chatroom GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP)" nil nil)

(autoload 'jabber-muc-private-get-buffer "jabber-muc" "\
Return the chat buffer for private chat with NICKNAME in GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP NICKNAME)" nil nil)

(autoload 'jabber-muc-vcard-get "jabber-muc" "\
Request vcard from chat with NICKNAME in GROUP.

\(fn JC GROUP NICKNAME)" t nil)

(autoload 'jabber-muc-message-p "jabber-muc" "\
Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites.

\(fn MESSAGE)" nil nil)

(autoload 'jabber-muc-sender-p "jabber-muc" "\
Return non-nil if JID is a full JID of an MUC participant.

\(fn JID)" nil nil)

(autoload 'jabber-muc-private-message-p "jabber-muc" "\
Return non-nil if MESSAGE is a private message in a groupchat.

\(fn MESSAGE)" nil nil)

;;;***

;;;### (autoloads nil "jabber-muc-nick-completion" "jabber-muc-nick-completion.el"
;;;;;;  (21669 55013 122572 742000))
;;; Generated autoloads from jabber-muc-nick-completion.el

(autoload 'jabber-muc-looks-like-personal-p "jabber-muc-nick-completion" "\
Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look.

\(fn MESSAGE &optional GROUP)" nil nil)

;;;***

;;;### (autoloads nil "jabber-presence" "jabber-presence.el" (21669
;;;;;;  55015 292471 856000))
;;; Generated autoloads from jabber-presence.el

(autoload 'jabber-send-presence "jabber-presence" "\
Set presence for all accounts.

\(fn SHOW STATUS PRIORITY)" t nil)

(autoload 'jabber-send-default-presence "jabber-presence" "\
Send default presence.
Default presence is specified by `jabber-default-show',
`jabber-default-status', and `jabber-default-priority'.

\(fn &optional IGNORE)" t nil)

;;;***

;;;### (autoloads nil "jabber-private" "jabber-private.el" (21669
;;;;;;  55014 722498 342000))
;;; Generated autoloads from jabber-private.el

(autoload 'jabber-private-get "jabber-private" "\
Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).

On success, SUCCESS-CALLBACK is called with JC and the retrieved
XML fragment.

On error, ERROR-CALLBACK is called with JC and the entire IQ
result.

\(fn JC NODE-NAME NAMESPACE SUCCESS-CALLBACK ERROR-CALLBACK)" nil nil)

(autoload 'jabber-private-set "jabber-private" "\
Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'.

\(fn JC FRAGMENT &optional SUCCESS-CALLBACK SUCCESS-CLOSURE-DATA ERROR-CALLBACK ERROR-CLOSURE-DATA)" nil nil)

;;;***

;;;### (autoloads nil "jabber-roster" "jabber-roster.el" (21669 55013
;;;;;;  642548 557000))
;;; Generated autoloads from jabber-roster.el

(autoload 'jabber-switch-to-roster-buffer "jabber-roster" "\
Switch to roster buffer.
Optional JC argument is ignored; it's there so this function can
be used in `jabber-post-connection-hooks'.

\(fn &optional JC)" t nil)

(autoload 'jabber-roster-update "jabber-roster" "\
Update roster, in memory and on display.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.

\(fn JC NEW-ITEMS CHANGED-ITEMS DELETED-ITEMS)" nil nil)

;;;***

;;;### (autoloads nil "jabber-rtt" "jabber-rtt.el" (21669 55012 292611
;;;;;;  376000))
;;; Generated autoloads from jabber-rtt.el

(eval-after-load "jabber-disco" '(jabber-disco-advertise-feature "urn:xmpp:rtt:0"))

(eval-after-load "jabber-core" '(add-to-list 'jabber-message-chain #'jabber-rtt-handle-message t))

(autoload 'jabber-rtt-handle-message "jabber-rtt" "\


\(fn JC XML-DATA)" nil nil)

(autoload 'jabber-rtt-send-mode "jabber-rtt" "\
Show text to recipient as it is being typed.
This lets the recipient see every change made to the message up
until it's sent.  The recipient's client needs to implement
XEP-0301, In-Band Real Time Text.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("fsm.el" "jabber-ahc-presence.el" "jabber-ahc.el"
;;;;;;  "jabber-alert.el" "jabber-avatar.el" "jabber-awesome.el"
;;;;;;  "jabber-browse.el" "jabber-chatstates.el" "jabber-conn.el"
;;;;;;  "jabber-events.el" "jabber-feature-neg.el" "jabber-festival.el"
;;;;;;  "jabber-ft-client.el" "jabber-ft-common.el" "jabber-ft-server.el"
;;;;;;  "jabber-history.el" "jabber-iq.el" "jabber-libnotify.el"
;;;;;;  "jabber-logon.el" "jabber-modeline.el" "jabber-muc-nick-coloring.el"
;;;;;;  "jabber-notifications.el" "jabber-osd.el" "jabber-ourversion.el"
;;;;;;  "jabber-ping.el" "jabber-pkg.el" "jabber-ratpoison.el" "jabber-register.el"
;;;;;;  "jabber-sasl.el" "jabber-sawfish.el" "jabber-screen.el" "jabber-search.el"
;;;;;;  "jabber-si-client.el" "jabber-si-common.el" "jabber-si-server.el"
;;;;;;  "jabber-socks5.el" "jabber-time.el" "jabber-tmux.el" "jabber-truncate.el"
;;;;;;  "jabber-util.el" "jabber-vcard-avatars.el" "jabber-vcard.el"
;;;;;;  "jabber-version.el" "jabber-watch.el" "jabber-widget.el"
;;;;;;  "jabber-wmii.el" "jabber-xmessage.el" "jabber-xml.el" "srv.el")
;;;;;;  (21669 55015 721080 740000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; jabber-autoloads.el ends here
