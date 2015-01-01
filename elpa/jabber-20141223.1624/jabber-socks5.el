;; jabber-socks5.el - SOCKS5 bytestreams by JEP-0065

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

(require 'jabber-iq)
(require 'jabber-disco)
(require 'jabber-si-server)
(require 'jabber-si-client)

(require 'fsm)
(eval-when-compile (require 'cl))

(defvar jabber-socks5-pending-sessions nil
  "List of pending sessions.

Each entry is a list, containing:
 * Stream ID
 * Full JID of initiator
 * State machine managing the session")

(defvar jabber-socks5-active-sessions nil
  "List of active sessions.

Each entry is a list, containing:
 * Network connection
 * Stream ID
 * Full JID of initiator
 * Profile data function")

(defcustom jabber-socks5-proxies nil
  "JIDs of JEP-0065 proxies to use for file transfer.
Put preferred ones first."
  :type '(repeat string)
  :group 'jabber
;  :set 'jabber-socks5-set-proxies)
  )

(defvar jabber-socks5-proxies-data nil
  "Alist containing information about proxies.
Keys of the alist are strings, the JIDs of the proxies.
Values are \"streamhost\" XML nodes.")

(jabber-disco-advertise-feature "http://jabber.org/protocol/bytestreams")

(add-to-list 'jabber-si-stream-methods
	     (list "http://jabber.org/protocol/bytestreams"
		   'jabber-socks5-client-1
		   'jabber-socks5-accept))

(defun jabber-socks5-set-proxies (symbol value)
  "Set `jabber-socks5-proxies' and query proxies.
This is the set function of `jabber-socks5-proxies-data'."
  (set-default symbol value)
  (when jabber-connections
    (jabber-socks5-query-all-proxies)))

(defun jabber-socks5-query-all-proxies (jc &optional callback)
  "Ask all proxies in `jabber-socks5-proxies' for connection information.
If CALLBACK is non-nil, call it with no arguments when all
proxies have answered."
  (interactive (list (jabber-read-account)))
  (setq jabber-socks5-proxies-data nil)
  (dolist (proxy jabber-socks5-proxies)
    (jabber-socks5-query-proxy jc proxy callback)))

(defun jabber-socks5-query-proxy (jc jid &optional callback)
  "Query the SOCKS5 proxy specified by JID for IP and port number."
  (jabber-send-iq jc jid "get"
		  '(query ((xmlns . "http://jabber.org/protocol/bytestreams")))
		  #'jabber-socks5-process-proxy-response (list callback t)
		  #'jabber-socks5-process-proxy-response (list callback nil)))

(defun jabber-socks5-process-proxy-response (jc xml-data closure-data)
  "Process response from proxy query."
  (let* ((query (jabber-iq-query xml-data))
	 (from (jabber-xml-get-attribute xml-data 'from))
	 (streamhosts (jabber-xml-get-children query 'streamhost)))

    (let ((existing-entry (assoc from jabber-socks5-proxies-data)))
      (when existing-entry
	(setq jabber-socks5-proxies-data
	      (delq existing-entry jabber-socks5-proxies-data))))

    (destructuring-bind (callback successp) closure-data
      (when successp
	(setq jabber-socks5-proxies-data
	      (cons (cons from streamhosts)
		    jabber-socks5-proxies-data)))
      (message "%s from %s.  %d of %d proxies have answered."
	       (if successp "Response" "Error") from
	       (length jabber-socks5-proxies-data) (length jabber-socks5-proxies))
      (when (and callback (= (length jabber-socks5-proxies-data) (length jabber-socks5-proxies)))
	(funcall callback)))))

(define-state-machine jabber-socks5
  :start ((jc jid sid profile-function role)
	  "Start JEP-0065 bytestream with JID.
SID is the session ID used.
PROFILE-FUNCTION is the function to call upon success.  See `jabber-si-stream-methods'.
ROLE is either :initiator or :target.  The initiator sends an IQ
set; the target waits for one."
	  (let ((new-state-data (list :jc jc
				      :jid jid
				      :sid sid
				      :profile-function profile-function
				      :role role))
		(new-state
		 ;; We want information about proxies; it might be needed in
		 ;; various situations.
		 (cond
		  ((null jabber-socks5-proxies)
		   ;; We know no proxy addresses.  Try to find them by disco.
		   'seek-proxies)
		  ((null jabber-socks5-proxies-data)
		   ;; We need to query the proxies for addresses.
		   'query-proxies)
		  ;; So, we have our proxies.
		  (t
		   'initiate))))
	    (list new-state new-state-data nil))))

(defun jabber-socks5-accept (jc jid sid profile-function)
  "Remember that we are waiting for connection from JID, with stream id SID"
  ;; asking the user for permission is done in the profile
  (add-to-list 'jabber-socks5-pending-sessions
	       (list sid jid (start-jabber-socks5 jc jid sid profile-function :target))))

(define-enter-state jabber-socks5 seek-proxies (fsm state-data)
  ;; Look for items at the server.
  (let* ((jc (plist-get state-data :jc))
	 (server (jabber-jid-server (jabber-connection-jid jc))))
    (jabber-disco-get-items jc
			    server
			    nil
			    (lambda (jc fsm result) 
			      (fsm-send-sync fsm (cons :items result)))
			    fsm))
  ;; Spend no more than five seconds looking for a proxy.
  (list state-data 5))

(define-state jabber-socks5 seek-proxies (fsm state-data event callback)
  "Collect disco results, looking for a bytestreams proxy."
  ;; We put the number of outstanding requests as :remaining-info in
  ;; the state-data plist.
  (cond
   ;; We're not ready to handle the IQ stanza yet
   ((eq (car-safe event) :iq)
    :defer)

   ;; Got list of items at the server.
   ((eq (car-safe event) :items)
    (dolist (entry (cdr event))
      ;; Each entry is ["name" "jid" "node"].  We send a disco info
      ;; request to everything without a node.
      (when (null (aref entry 2))
	(lexical-let ((jid (aref entry 1)))
	  (jabber-disco-get-info 
	   (plist-get state-data :jc)
	   jid nil
	   (lambda (jc fsm result)
	     (fsm-send-sync fsm (list :info jid result)))
	   fsm))))
    ;; Remember number of requests sent.  But if none, we just go on.
    (if (cdr event)
	(list 'seek-proxies (plist-put state-data :remaining-info (length (cdr event))) :keep)
      (list 'initiate state-data nil)))

   ;; Got disco info from an item at the server.
   ((eq (car-safe event) :info)
    (fsm-debug-output "got disco event")
    ;; Count the response.
    (plist-put state-data :remaining-info (1- (plist-get state-data :remaining-info)))
    (unless (eq (first (third event)) 'error)
      (let ((identities (first (third event))))
	;; Is it a bytestream proxy?
	(when (dolist (identity identities)
		(when (and (string= (aref identity 1) "proxy")
			   (string= (aref identity 2) "bytestreams"))
		  (return t)))
	  ;; Yes, it is.  Add it to the list.
	  (push (second event) jabber-socks5-proxies))))

    ;; Wait for more responses, if any are to be expected.
    (if (zerop (plist-get state-data :remaining-info))
	;; No more... go on to querying the proxies.
	(list 'query-proxies state-data nil)
      ;; We expect more responses...
      (list 'seek-proxies state-data :keep)))

   ((eq event :timeout)
    ;; We can't wait anymore...
    (list 'query-proxies state-data nil))))

(define-enter-state jabber-socks5 query-proxies (fsm state-data)
  (jabber-socks5-query-all-proxies
   (plist-get state-data :jc)
   (lexical-let ((fsm fsm))
     (lambda () (fsm-send-sync fsm :proxies))))
  (list state-data 5))

(define-state jabber-socks5 query-proxies (fsm state-data event callback)
  "Query proxies in `jabber-socks5-proxies'."
  (cond
   ;; Can't handle the iq stanza yet...
   ((eq (car-safe event) :iq)
    :defer)

   ((eq (car-safe event) :info)
    ;; stray event... do nothing
    (list 'query-proxies state-data :keep))

   ;; Got response/error from all proxies, or timeout
   ((memq event '(:proxies :timeout))
    (list 'initiate state-data nil))))

(define-enter-state jabber-socks5 initiate (fsm state-data)
  ;; Sort the alist jabber-socks5-proxies-data such that the
  ;; keys are in the same order as in jabber-socks5-proxies.
  (setq jabber-socks5-proxies-data
	(sort jabber-socks5-proxies-data
	      #'(lambda (a b)
		  (> (length (member (car a) jabber-socks5-proxies))
		     (length (member (car b) jabber-socks5-proxies))))))

  ;; If we're the initiator, send initiation stanza.
  (when (eq (plist-get state-data :role) :initiator)
    ;; This is where initiation of server sockets would go

    (jabber-send-iq
     (plist-get state-data :jc)
     (plist-get state-data :jid) "set"
     `(query ((xmlns . "http://jabber.org/protocol/bytestreams")
	      (sid . ,(plist-get state-data :sid)))
	     ,@(mapcar 
		#'(lambda (proxy)
		    (mapcar
		     #'(lambda (streamhost)
			 (list 'streamhost
			       (list (cons 'jid (jabber-xml-get-attribute streamhost 'jid))
				     (cons 'host (jabber-xml-get-attribute streamhost 'host))
				     (cons 'port (jabber-xml-get-attribute streamhost 'port)))
			       ;; (proxy ((xmlns . "http://affinix.com/jabber/stream")))
			       ))
		     (cdr proxy)))
		jabber-socks5-proxies-data)
	     ;; (fast ((xmlns . "http://affinix.com/jabber/stream")))
	     )
     (lexical-let ((fsm fsm))
       (lambda (jc xml-data closure-data)
	 (fsm-send-sync fsm (list :iq xml-data)))) 
     nil
     ;; TODO: error handling
     #'jabber-report-success "SOCKS5 negotiation"))

  ;; If we're the target, we just wait for an incoming stanza.
  (list state-data nil))

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "http://jabber.org/protocol/bytestreams" 'jabber-socks5-process))
(defun jabber-socks5-process (jc xml-data)
  "Accept IQ get for SOCKS5 bytestream"
  (let* ((jid (jabber-xml-get-attribute xml-data 'from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (query (jabber-iq-query xml-data))
	 (sid (jabber-xml-get-attribute query 'sid))
	 (session (dolist (pending-session jabber-socks5-pending-sessions)
		    (when (and (equal sid (nth 0 pending-session))
			       (equal jid (nth 1 pending-session)))
		      (return pending-session)))))
    ;; check that we really are expecting this session
    (unless session
      (jabber-signal-error "auth" 'not-acceptable))

    (setq jabber-socks5-pending-sessions (delq session jabber-socks5-pending-sessions))
    (fsm-send-sync (nth 2 session) (list :iq xml-data))

    ;; find streamhost to connect to
;;     (let* ((streamhosts (jabber-xml-get-children query 'streamhost))
;; 	   (streamhost (dolist (streamhost streamhosts)
;; 			 (let ((connection (jabber-socks5-connect streamhost sid jid (concat jabber-username "@" jabber-server "/" jabber-resource))))
;; 			   (when connection
;; 			     ;; We select the first streamhost that we are able to connect to.
;; 			     (push (list connection sid jid profile-data-function)
;; 				   jabber-socks5-active-sessions)
;; 			     ;; Now set the filter, for the rest of the output
;; 			     (set-process-filter connection #'jabber-socks5-filter)
;; 			     (set-process-sentinel connection #'jabber-socks5-sentinel)
;; 			     (return streamhost))))))
;;       (unless streamhost
;; 	(jabber-signal-error "cancel" 'item-not-found))
      
;;       ;; tell initiator which streamhost we use
;;       (jabber-send-iq jid "result"
;; 		      `(query ((xmlns . "http://jabber.org/protocol/bytestreams"))
;; 			      (streamhost-used ((jid . ,(jabber-xml-get-attribute streamhost 'jid)))))
;; 		      nil nil nil nil id)
;;       ;; now, as data is sent, it will be passed to the profile.
;;       )
    ))

(define-state jabber-socks5 initiate (fsm state-data event callback)
  (let* ((jc (plist-get state-data :jc))
	 (jc-data (fsm-get-state-data jc))
	 (our-jid (concat (plist-get jc-data :username) "@"
			  (plist-get jc-data :server) "/"
			  (plist-get jc-data :resource)))
	 (their-jid (plist-get state-data :jid))
	 (initiator-jid (if (eq (plist-get state-data :role) :initiator) our-jid their-jid))
	 (target-jid (if (eq (plist-get state-data :role) :initiator) their-jid our-jid)))
    (cond
     ;; Stray event...
     ((memq (car-safe event) '(:proxy :info))
      (list 'initiate state-data :keep))

     ;; Incoming IQ
     ((eq (car-safe event) :iq)
      (let ((xml-data (second event)))
	;; This is either type "set" (with a list of streamhosts to
	;; use), or a "result" (indicating the streamhost finally used
	;; by the other party).
	(cond
	 ((string= (jabber-xml-get-attribute xml-data 'type) "set")
	  ;; A "set" makes sense if we're the initiator and offered
	  ;; Psi's "fast mode".  We don't yet, though, so this is only
	  ;; for target.
	  (dolist (streamhost (jabber-xml-get-children (jabber-iq-query xml-data) 'streamhost))
	    (jabber-xml-let-attributes
	     (jid host port) streamhost
	     ;; This is where we would attempt to support zeroconf
	     (when (and jid host port)
	       (start-jabber-socks5-connection
		jc initiator-jid target-jid jid
		(plist-get state-data :sid) host port fsm))))

	  (list 'wait-for-connection (plist-put state-data :iq-id (jabber-xml-get-attribute xml-data 'id)) 30))

	 ((string= (jabber-xml-get-attribute xml-data 'type) "result")
	  ;; The other party has decided what streamhost to use.
	  (let* ((proxy-used (jabber-xml-get-attribute (jabber-xml-path xml-data '(query streamhost-used)) 'jid))
		 ;; If JID is our own JID, we have probably already detected
		 ;; what connection to use.  But that is a later problem...
		 (streamhosts (cdr (assoc proxy-used jabber-socks5-proxies-data))))
	    ;; Try to connect to all addresses of this proxy...
	    (dolist (streamhost streamhosts)
	      (jabber-xml-let-attributes
	       (jid host port) streamhost
	       (when (and jid host port)
		 (start-jabber-socks5-connection
		  jc initiator-jid target-jid jid
		  (plist-get state-data :sid) host port fsm)))))

	  (list 'wait-for-connection state-data 30))))))))

(define-state-machine jabber-socks5-connection
  :start
  ((jc initiator-jid target-jid streamhost-jid sid host port socks5-fsm)
   "Connect to a single JEP-0065 streamhost."
   (let ((coding-system-for-read 'binary)
	 (coding-system-for-write 'binary))
     ;; make-network-process, which we really want, for asynchronous
     ;; connection and such, was introduced in Emacs 22.
     (if (fboundp 'make-network-process)
	 (let ((connection
		(make-network-process 
		 :name "socks5" 
		 :buffer nil
		 :host host
		 :service (string-to-number port)
		 :nowait t
		 :filter (fsm-make-filter fsm)
		 :sentinel (fsm-make-sentinel fsm))))
	   (list 'wait-for-connection 
		 (list :jc jc
		       :connection connection
		       :initiator-jid initiator-jid
		       :target-jid target-jid
		       :streamhost-jid streamhost-jid
		       :sid sid
		       :socks5-fsm socks5-fsm)
		 30))
       ;; So we open a stream, and wait for the connection to succeed.
       (condition-case nil
	   (let ((connection
		  (open-network-stream "socks5" nil
				       host (string-to-number port))))
	     (set-process-filter connection (fsm-make-filter fsm))
	     (set-process-sentinel connection (fsm-make-sentinel fsm))
	     (list 'authenticate
		   (list :jc jc
			 :connection connection
			 :initiator-jid initiator-jid
			 :target-jid target-jid
			 :streamhost-jid streamhost-jid
			 :sid sid
			 :socks5-fsm socks5-fsm)
		   nil))
	 (error (list 'fail '() nil)))))))

(define-state jabber-socks5-connection wait-for-connection
  (fsm state-data event callback)
  (cond
   ((eq (car-safe event) :sentinel)
    (let ((string (third event)))
      (cond
       ;; Connection succeeded
       ((string= (substring string 0 4) "open")
	(list 'authenticate state-data nil))
       ;; Connection failed
       (t
	(list 'fail state-data nil)))))))

(define-enter-state jabber-socks5-connection authenticate
  (fsm state-data)
  "Send authenticate command."
  ;; version: 5.  number of auth methods supported: 1.
  ;; which one: no authentication.
  (process-send-string (plist-get state-data :connection) (string 5 1 0))
  (list state-data 30))

(define-state jabber-socks5-connection authenticate
  (fsm state-data event callback)
  "Receive response to authenticate command."
  (cond
   ((eq (car-safe event) :filter)
    (let ((string (third event)))
      ;; should return:
      ;; version: 5.  auth method to use: none
      (if (string= string (string 5 0))
	  ;; Authenticated.  Send connect command.
	  (list 'connect state-data nil)
	;; Authentication failed...
	(delete-process (second event))
	(list 'fail state-data nil))))

   ((eq (car-safe event) :sentinel)
    (list 'fail state-data nil))))

(define-enter-state jabber-socks5-connection connect (fsm state-data)
  "Send connect command."
  (let* ((sid (plist-get state-data :sid))
	 (initiator (plist-get state-data :initiator-jid))
	 (target (plist-get state-data :target-jid))
	 (hash (sha1-string (concat sid initiator target))))
    (process-send-string 
     (plist-get state-data :connection)
     (concat (string 5 1 0 3 (length hash))
	     hash
	     (string 0 0)))
    (list state-data 30)))

(define-state jabber-socks5-connection connect
  (fsm state-data event callback)
  "Receive response to connect command."
  (cond
   ((eq (car-safe event) :filter)
    (let ((string (third event)))
      (if (string= (substring string 0 2) (string 5 0))
	  ;; connection established
	  (progn
	    (fsm-send (plist-get state-data :socks5-fsm)
		      (list :connected
			    (plist-get state-data :connection)
			    (plist-get state-data :streamhost-jid)))
	    ;; Our work is done
	    (list 'done nil))
	(list 'fail state-data nil))))
   ((eq (car-safe event) :sentinel)
    (list 'fail state-data nil))))
       
(define-state jabber-socks5-connection done
  (fsm state-data event callback)
  ;; ignore all events
  (list 'done nil nil))

(define-enter-state jabber-socks5-connection fail (fsm state-data)
  ;; Notify parent fsm about failure
  (fsm-send (plist-get state-data :socks5-fsm)
	    :not-connected)
  (list nil nil))

(define-state jabber-socks5-connection fail
  (fsm state-data event callback)
  ;; ignore all events
  (list 'fail nil nil))

(define-state jabber-socks5 wait-for-connection
  (fsm state-data event callback)
  (cond
   ((eq (car-safe event) :connected)
    (destructuring-bind (ignored connection streamhost-jid) event
      (setq state-data (plist-put state-data :connection connection))
      ;; If we are expected to tell which streamhost we chose, do so.
      (let ((iq-id (plist-get state-data :iq-id)))
	(when iq-id
	  (jabber-send-iq 
	   (plist-get state-data :jc)
	   (plist-get state-data :jid) "result"
	   `(query ((xmlns . "http://jabber.org/protocol/bytestreams"))
		   (streamhost-used ((jid . ,streamhost-jid))))
	   nil nil nil nil
	   iq-id)))

      ;; If we are the initiator, we should activate the bytestream.
      (if (eq (plist-get state-data :role) :initiator)
	  (progn
	    (jabber-send-iq 
	     (plist-get state-data :jc)
	     streamhost-jid "set"
	     `(query ((xmlns . "http://jabber.org/protocol/bytestreams")
		      (sid . ,(plist-get state-data :sid)))
		     (activate nil ,(plist-get state-data :jid)))
	     (lambda (jc xml-data fsm) (fsm-send-sync fsm :activated)) fsm
	     (lambda (jc xml-data fsm) (fsm-send-sync fsm :activation-failed)) fsm)
	    (list 'wait-for-activation state-data 10))
	;; Otherwise, we just let the data flow.
	(list 'stream-activated state-data nil))))

   ((eq event :not-connected)
    ;; If we were counting the streamhosts, we would know when there
    ;; are no more chances left.
    (list 'wait-for-connection state-data :keep))

   ((eq event :timeout)
    (list 'fail (plist-put state-data :error "Timeout when connecting to streamhosts") nil))))

(define-state jabber-socks5 wait-for-activation
  (fsm state-data event callback)
  (cond
   ((eq event :activated)
    (list 'stream-activated state-data nil))
   ((eq event :activation-failed)
    (list 'fail (plist-put state-data :error "Proxy activation failed") nil))

   ;; Stray events from earlier state
   ((eq (car-safe event) :connected)
    ;; We just close the connection
    (delete-process (second event))
    (list 'wait-for-activation state-data :keep))
   ((eq event :not-connected)
    (list 'wait-for-activation state-data :keep))))

(define-enter-state jabber-socks5 stream-activated
  (fsm state-data)
  (let ((connection (plist-get state-data :connection))
	(jc (plist-get state-data :jc))
	(jid (plist-get state-data :jid))
	(sid (plist-get state-data :sid))
	(profile-function (plist-get state-data :profile-function)))
    (set-process-filter connection (fsm-make-filter fsm))
    (set-process-sentinel connection (fsm-make-sentinel fsm))
    ;; Call the profile function, passing the data send function, and
    ;; receiving the data receiving function.  Put the data receiving
    ;; function in the plist.
    (list (plist-put state-data
		     :profile-data-function
		     (funcall profile-function
			      jc jid sid
			      (lexical-let ((fsm fsm))
				(lambda (data)
				  (fsm-send fsm (list :send data))))))
	  nil)))
			    

(define-state jabber-socks5 stream-activated
  (fsm state-data event callback)
  (let ((jc (plist-get state-data :jc))
	(connection (plist-get state-data :connection))
	(profile-data-function (plist-get state-data :profile-data-function))
	(sid (plist-get state-data :sid))
	(jid (plist-get state-data :jid)))
    (cond
     ((eq (car-safe event) :send)
      (process-send-string connection (second event))
      (list 'stream-activated state-data nil))

     ((eq (car-safe event) :filter)
      ;; Pass data from connection to profile data function
      ;; If the data function requests it, tear down the connection.
      (unless (funcall profile-data-function jc jid sid (third event))
	(fsm-send fsm (list :sentinel (second event) "shutdown")))

      (list 'stream-activated state-data nil))

     ((eq (car-safe event) :sentinel)
      ;; Connection terminated.  Shuffle together the remaining data,
      ;; and kill the buffer.
      (delete-process (second event))
      (funcall profile-data-function jc jid sid nil)
      (list 'closed nil nil))

     ;; Stray events from earlier state
     ((eq (car-safe event) :connected)
      ;; We just close the connection
      (delete-process (second event))
      (list 'stream-activated state-data nil))
     ((eq event :not-connected)
      (list 'stream-activated state-data nil)))))

(define-enter-state jabber-socks5 fail (fsm state-data)
  "Tell our caller that we failed."
  (let ((jc (plist-get state-data :jc))
	(jid (plist-get state-data :jid))
	(sid (plist-get state-data :sid))
	(profile-function (plist-get state-data :profile-function))
	(iq-id (plist-get state-data :iq-id)))
    (funcall profile-function jc jid sid (plist-get state-data :error))

    (when iq-id
      (jabber-send-iq-error jc jid iq-id nil "cancel"
			    'remote-server-not-found)))
  (list nil nil))

(defun jabber-socks5-client-1 (jc jid sid profile-function)
  "Negotiate a SOCKS5 connection with JID.
This function simply starts a state machine."
  (add-to-list 'jabber-socks5-pending-sessions
	       (list sid jid (start-jabber-socks5 jc jid sid profile-function :initiator))))

;; (defun jabber-socks5-client-2 (xml-data jid sid profile-function)
;;   "Contact has selected a streamhost to use.  Connect to the proxy."
;;   (let* ((query (jabber-iq-query xml-data))
;; 	 (streamhost-used (car (jabber-xml-get-children query 'streamhost-used)))
;; 	 (proxy-used (jabber-xml-get-attribute streamhost-used 'jid))
;; 	 connection)
;;     (let ((streamhosts-left (cdr (assoc proxy-used jabber-socks5-proxies-data))))
;;       (while (and streamhosts-left (not connection))
;; 	(setq connection
;; 	      (jabber-socks5-connect (car streamhosts-left)
;; 				     sid
;; 				     (concat jabber-username "@" jabber-server "/" jabber-resource)
;; 				     jid))
;; 	(setq streamhosts-left (cdr streamhosts-left))))
;;     (unless connection
;;       (error "Couldn't connect to proxy %s" proxy-used))

;;     ;; Activation is only needed for proxies.
;;     (jabber-send-iq proxy-used "set"
;; 		    `(query ((xmlns . "http://jabber.org/protocol/bytestreams")
;; 			     (sid . ,sid))
;; 			    (activate () ,jid))
;; 		    (lexical-let ((jid jid) (sid sid) (profile-function profile-function)
;; 				  (connection connection))
;; 		      (lambda (xml-data closure-data)
;; 			(jabber-socks5-client-3 xml-data jid sid profile-function connection))) nil
;; 		       ;; TODO: report error to contact?
;; 		       #'jabber-report-success "Proxy activation")))

;; (defun jabber-socks5-client-3 (xml-data jid sid profile-function proxy-connection)
;;   "Proxy is activated.  Start the transfer."
;;   ;; The response from the proxy does not contain any interesting
;;   ;; information, beyond success confirmation.

;;   (funcall profile-function jid sid 
;; 	   (lexical-let ((proxy-connection proxy-connection))
;; 	     (lambda (data)
;; 	       (process-send-string proxy-connection data)))))

(provide 'jabber-socks5)

;;; arch-tag: 9e70dfea-2522-40c6-a79f-302c8fb82ac5
