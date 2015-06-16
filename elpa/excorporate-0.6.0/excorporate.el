;;; excorporate.el --- Exchange integration           -*- lexical-binding: t -*-

;; Copyright (C) 2014  Thomas Fitzsimmons

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Created: 2014-09-19
;; Keywords: calendar
;; Version: 0.6.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To create a connection to a web service:

;; M-x excorporate

;; Excorporate will prompt for an email address that it will use to
;; automatically discover settings.  Then it will connect to two
;; separate hosts: the autodiscovery host and the web service host, so
;; you may be prompted for your credentials twice.

;; You should see a message indicating that the connection is ready
;; either in the minibuffer or failing that in the *Messages* buffer.

;; Finally, run M-x calendar, and press 'e' to show today's meetings.

;; Please try autodiscovery first and report issues not yet listed
;; below.  When autodiscovery works it is very convenient; the goal is
;; to make it work for as many users as possible.

;; If autodiscovery fails, customize `excorporate-configuration' to
;; skip autodiscovery.

;; Autodiscovery will fail if:

;; - Excorporate is accessing the server through a proxy (Emacs
;;   bug#10).

;; - The server is not configured to support autodiscovery.

;; - The email address is at a different domain than the server, e.g.,
;;   user@domain1.com, autodiscover.domain2.com.

;; - Authentication is Kerberos/GSSAPI.

;; Excorporate does know about the special case where the mail address
;; is at a subdomain, e.g., user@sub.domain.com, and the server is at
;; the main domain, e.g., autodiscover.domain.com.  Autodiscovery will
;; work in that case.

;; Excorporate must be loaded before any other package that requires
;; `soap-client'.  The version of `soap-client' that Excorporate
;; bundles is backward compatible.

;; Acknowledgments:

;; Alexandru Harsanyi <AlexHarsanyi@gmail.com> provided help and
;; guidance on how to extend soap-client.el's WSDL and XSD handling,
;; enabling support for the full Exchange Web Services API.

;; Alex Luccisano <casual.lexicon@gmail.com> tested early versions of
;; this library against a corporate installation of Exchange and
;; provided feedback.

;; Jon Miller <jonebird@gmail.com> tested against Exchange 2013 and
;; provided feedback.  Jon also tracked down and reported a bad
;; interaction with other packages that require soap-client.

;; Nicolas Lamirault <nicolas.lamirault@gmail.com> tested the
;; autodiscovery feature and provided feedback.

;; Trey Jackson <bigfaceworm@gmail.com> confirmed autodiscovery worked
;; for him.

;; Joakim Verona <joakim@verona.se> tested autodiscovery in a
;; Kerberos/GSSAPI environment and provided feedback.

;;; Code:

;; Implementation-visible functions and variables.

;; Add NTLM authentication scheme.
(unless (and (boundp 'emacs-major-version)
	     (or (eq emacs-major-version 24)
		 (eq emacs-major-version 25)))
  (error "Excorporate: Unsupported Emacs version"))
;; Add NTLM url auth scheme.
(require 'exco-url-http-ntlm)
(unless (eq emacs-major-version 25)
  ;; Add NTLM2 session support.
  (require 'exco-ntlm))
;; Remove authentication after redirect.
(require (intern (format "exco-url-http-%d.%d"
			 emacs-major-version
			 emacs-minor-version)))
;; Same as 2014-12-08 tip of soap-client exchange-services-branch.
(require 'exco-soap-client)
;; Same as Jabber's fsm.el.
(require 'exco-fsm)

(defconst exco--autodiscovery-templates
  '("https://%s/autodiscover/autodiscover.svc"
    "https://autodiscover.%s/autodiscover/autodiscover.svc")
  "Autodiscovery URL templates.
URL templates to be formatted with a domain name, then searched
for autodiscovery files.")

(defvar exco--connections nil
  "A hash table of finite state machines.
The key is the identifier passed to `exco-connect'.  Each finite
state machine represents a service connection.")

(defvar exco--connection-identifiers nil
  "An ordered list of connection identifiers.
The `car' of this list is the default identifier used by
`exco-operate'.")

(defun exco--parse-xml-in-current-buffer ()
  "Decode and parse the XML contents of the current buffer."
  (let ((mime-part (mm-dissect-buffer t t)))
    (unless mime-part
      (error "Failed to decode response from server"))
    (unless (equal (car (mm-handle-type mime-part)) "text/xml")
      (error "Server response is not an XML document"))
    (with-temp-buffer
      (mm-insert-part mime-part)
      (prog1
	  (car (xml-parse-region (point-min) (point-max)))
	(kill-buffer)
	(mm-destroy-part mime-part)))))

(defun exco--bind-wsdl (wsdl service-url port-name target-namespace
			     binding-name)
  "Create a WSDL binding.
Create a binding port for WSDL from SERVICE-URL, PORT-NAME,
TARGET-NAMESPACE and BINDING-NAME."
  (let* ((namespace (soap-wsdl-find-namespace target-namespace wsdl))
	 (port (make-soap-port
		:name port-name
		:binding (cons target-namespace binding-name)
		:service-url service-url)))
    (soap-namespace-put port namespace)
    (push port (soap-wsdl-ports wsdl))
    (soap-resolve-references port wsdl)
    wsdl))

(defun exco--handle-url-error (url status)
  "Handle an error that occurred when retrieving URL.
The details of the error are in STATUS, in the same format as the
argument to a `url-retrieve' callback.  Return non-nil to retry,
nil to continue."
  (if (eq (third (plist-get status :error)) 500)
      ;; The server reported an internal server error.  Try to recover
      ;; by re-requesting the target URL and its most recent redirect.
      ;; I'm not sure what conditions cause the server to get into
      ;; this state -- it might be because the server has stale
      ;; knowledge of old keepalive connections -- but this should
      ;; recover it.  We need to disable ntlm in
      ;; url-registered-auth-schemes so that it doesn't prevent
      ;; setting keepalives to nil.
      (let ((url-registered-auth-schemes nil)
	    (url-http-attempt-keepalives nil)
	    (redirect (plist-get status :redirect)))
	(fsm-debug-output "exco--fsm received 500 error for %s" url)
	(url-http-debug "Excorporate attempting 500 recovery")
	(ignore-errors
	  ;; Emacs's url-retrieve does not respect the values of
	  ;; url-http-attempt-keepalives and
	  ;; url-registered-auth-schemes in asynchronous contexts.
	  ;; Unless url.el is eventually changed to do so, the
	  ;; following requests must be synchronous so that they run
	  ;; entirely within url-http-attempt-keepalives's dynamic
	  ;; extent.  These calls block the main event loop,
	  ;; unfortunately, but only in this rare error recovery
	  ;; scenario.
	  (url-retrieve-synchronously url)
	  (when redirect (url-retrieve-synchronously redirect)))
	(url-http-debug "Excorporate done 500 recovery attempt")
	;; Retry.
	t)
    ;; We received some other error, which just
    ;; means we should try the next URL.
    (fsm-debug-output "exco--fsm didn't find %s" url)
    ;; Don't retry.
    nil))

(defun exco--retrieve-next-import (fsm state-data return-for next-state)
  "Retrieve the next XML schema import.
FSM is the finite state machine, STATE-DATA is FSM's state data,
and RETURN-FOR is one of :enter or :event to indicate what return
type the calling function expects.  NEXT-STATE is the next state
the FSM should transition to on success."
  (let* ((url (plist-get state-data :service-url))
	 (xml (plist-get state-data :service-xml))
	 (wsdl (plist-get state-data :service-wsdl))
	 (imports (soap-wsdl-xmlschema-imports wsdl))
	 (next-state (if imports :parsing-service-wsdl next-state)))
    (when imports
      (let ((import-url (url-expand-file-name (pop imports) url)))
	(let ((url-request-method "GET")
	      (url-package-name "soap-client.el")
	      (url-package-version "1.0")
	      (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
	      (url-http-attempt-keepalives t))
	  (url-retrieve
	   import-url
	   (lambda (status)
	     (let ((data-buffer (current-buffer)))
	       (unwind-protect
		   (progn
		     (url-http-debug "Excorporate processing import %s" status)
		     (if (eq (car status) :error)
			 ;; There is an error.  It may be recoverable
			 ;; if it's HTTP 500 (internal server error).
			 (if (and (exco--handle-url-error import-url status)
				  ;; Only retry once.
				  (not (plist-get state-data :retrying)))
			     ;; We should retry.  Don't save the
			     ;; popped urls list to state-data, so
			     ;; that this :try-next-url will
			     ;; re-attempt to retrieve the same car as
			     ;; before.  Set the retry flag.
			     (progn
			       (plist-put state-data :retrying t))
			   ;; Save the popped urls list so that the next url
			   ;; is attempted, and clear the retry flag.
			   (plist-put state-data :retrying nil)
			   (setf (soap-wsdl-xmlschema-imports wsdl) imports)
			   (plist-put state-data :failure-message
				      (format "Failed to retrieve %s"
					      import-url))
			   (fsm-send fsm :unrecoverable-error))
		       ;; Success, parse WSDL.
		       (plist-put state-data :retrying nil)
		       (setf (soap-wsdl-xmlschema-imports wsdl) imports)
		       (soap-with-local-xmlns xml
			 (soap-wsdl-add-namespace
			  (soap-parse-schema (soap-parse-server-response) wsdl)
			  wsdl))
		       (plist-put state-data :service-wsdl wsdl)))
		 (and (buffer-live-p data-buffer)
		      (kill-buffer data-buffer))))
	     (fsm-send fsm t))))))
    (if (eq return-for :enter)
	(list state-data nil)
      (list next-state state-data nil))))

(define-state-machine exco--fsm :start
  ((identifier)
   "Start an Excorporate finite state machine."
   (if (stringp identifier)
       (let ((domain (cadr (split-string identifier "@"))))
	 (unless (and domain (not (equal domain "")))
	   (error "Invalid domain for address %s" identifier))
	 (list :retrieving-autodiscovery-xml
	       (list
		;; State machine data.
		;; Unique finite state machine identifier.  Either mail-address
		;; or (mail-address . service-url).  The latter allows multiple
		;; state machines to operate on the same service URL.  Login
		;; credentials are handled separately by auth-source and url,
		;; so these should be the only two identifier types needed here.
		:identifier identifier
		;; User data.
		:mail-address identifier
		;; Error recovery data.
		:retrying nil
		;; Autodiscovery data.
		:autodiscovery-urls
		(append (mapcar (lambda (template)
				  (format template domain))
				exco--autodiscovery-templates)
			;; Handle the user@sub.domain.com =>
			;; autodiscover.domain.com case reported by a
			;; user.  Only try one extra level.
			(let ((domain-parts (split-string domain "\\.")))
			  (when (> (length domain-parts) 2)
			    (mapcar (lambda (template)
				      (format template
					      (mapconcat
					       'identity
					       (cdr domain-parts) ".")))
				    exco--autodiscovery-templates))))
		;; Service data.
		:service-url nil
		:service-xml nil
		:service-wsdl nil
		;; State data.
		:next-state-after-success nil
		:failure-message nil
		:server-version nil)
	       ;; No timeout.
	       nil))
     ;; Go directly to :retrieving-service-xml, skipping autodiscovery.
     (list :retrieving-service-xml
	   (list
	    :identifier identifier
	    :mail-address (car identifier)
	    :retrying nil
	    :autodiscovery-urls nil
	    ;; Use service-url field from identifier.
	    :service-url (cdr identifier)
	    :service-xml nil
	    :service-wsdl nil
	    :next-state-after-success nil
	    :failure-message nil
	    :server-version nil)
	   ;; No timeout.
	   nil))))

(define-state exco--fsm :retrieving-autodiscovery-xml
  (fsm state-data event _callback)
  (case event
    (:try-next-url
     (let ((urls (plist-get state-data :autodiscovery-urls)))
       (if urls
	   (let ((url (pop urls)))
	     (fsm-debug-output "exco--fsm will probe %s" url)
	     (condition-case nil
		 (url-retrieve
		  url
		  (lambda (status)
		    (let ((data-buffer (current-buffer)))
		      (unwind-protect
			  (progn
			    (url-http-debug
			     "Excorporate processing status: %s" status)
			    (if (eq (car status) :error)
				(progn
				  (if (and
				       (exco--handle-url-error url status)
				       ;; Only retry once.
				       (not (plist-get state-data :retrying)))
				      ;; We should retry.  Don't save the popped
				      ;; urls list to state-data, so that this
				      ;; :try-next-url will re-attempt to
				      ;; retrieve the same car as before.  Set
				      ;; the retry flag.
				      (plist-put state-data :retrying t)
				    ;; Save the popped urls list so that the
				    ;; next url is attempted, and clear the
				    ;; retry flag.
				    (plist-put state-data :retrying nil)
				    (plist-put state-data
					       :autodiscovery-urls urls))
				  ;; Try next or retry.
				  (fsm-send fsm :try-next-url))
			      ;; Success, save URL and parse returned XML.
			      (message
			       "Excorporate: Found autodiscovery URL for %S: %s"
			       (plist-get state-data :identifier) url)
			      (plist-put state-data :retrying nil)
			      (plist-put state-data :service-url url)
			      (plist-put state-data :service-xml
					 (exco--parse-xml-in-current-buffer))
			      (fsm-send fsm :success))
			    (url-http-debug
			     "Excorporate done processing status"))
			(and (buffer-live-p data-buffer)
			     (kill-buffer data-buffer))))))
	       (error
		(fsm-debug-output "exco--fsm connection refused for %s" url)
		(plist-put state-data :retrying nil)
		(plist-put state-data :autodiscovery-urls urls)
		(fsm-send fsm :try-next-url)))
	     (list :retrieving-autodiscovery-xml state-data nil))
	 (plist-put state-data :failure-message
		    "Autodiscovery ran out of URLs to try")
	 (list :shutting-down-on-error state-data nil))))
    (:success
     (plist-put state-data :next-state-after-success :retrieving-service-xml)
     (list :parsing-service-wsdl state-data nil))))

(define-enter-state exco--fsm :shutting-down-on-error
  (_fsm state-data)
  (let ((failure-message (plist-get state-data :failure-message)))
    (exco-disconnect (plist-get state-data :identifier))
    (message "Excorporate: %s" failure-message)
    (url-http-debug "Excorporate failed: %s" failure-message)
    (fsm-debug-output "exco--fsm failed: %s" failure-message))
  (list state-data nil))

(define-state exco--fsm :shutting-down-on-error
  (_fsm state-data _event _callback)
  (list :shutting-down-on-error state-data nil))

(define-enter-state exco--fsm :retrieving-service-xml
  (fsm state-data)
  (when (stringp (plist-get state-data :identifier))
    (let* ((xml (plist-get state-data :service-xml))
	   (unbound-wsdl (plist-get state-data :service-wsdl))
	   (wsdl
	    (progn
	      ;; Skip soap-parse-wsdl-phase-fetch-schema to avoid
	      ;; synchronous URL fetches.
	      (soap-parse-wsdl-phase-finish-parsing xml unbound-wsdl)
	      (exco--bind-wsdl
	       (soap-wsdl-resolve-references unbound-wsdl)
	       (plist-get state-data :service-url)
	       "AutodiscoverServicePort"
	       "http://schemas.microsoft.com/exchange/2010/Autodiscover"
	       "DefaultBinding_Autodiscover"))))
      (soap-invoke-async
       (lambda (response)
	 (let ((result-url
		(exco-extract-value '(Response
				      UserResponses
				      UserResponse
				      UserSettings
				      UserSetting
				      Value)
				    response)))
	   (if result-url
	       (progn
		 (plist-put state-data :service-url result-url)
		 (message "Excorporate: Found service URL for %S: %s"
			  (plist-get state-data :identifier)
			  (plist-get state-data :service-url)))
	     ;; No result.  Check for error.
	     (let ((error-message
		    (exco-extract-value '(Response
					  UserResponses
					  UserResponse
					  ErrorMessage)
					response)))
	       (if error-message
		   (message "Excorporate: %s" error-message)
		 (message "Excorporate: Failed to find service URL"))))
	   (fsm-send fsm :retrieve-xml)))
       nil
       wsdl
       "AutodiscoverServicePort"
       "GetUserSettings"
       `((RequestedServerVersion . "Exchange2010")
	 (Request
	  (Users
	   (User
	    (Mailbox . ,(plist-get state-data :mail-address))))
	  (RequestedSettings
	   (Setting . "InternalEwsUrl")))))))
  (list state-data nil))

(define-state exco--fsm :retrieving-service-xml
  (fsm state-data event _callback)
  (case event
    (:unrecoverable-error
     (list :shutting-down-on-error state-data nil))
    (:retrieve-xml
     (let ((service-url (plist-get state-data :service-url)))
       (url-retrieve (concat service-url "?wsdl")
		     (lambda (status)
		       (let ((data-buffer (current-buffer)))
			 (unwind-protect
			     (if (eq (car status) :error)
				 (progn
				   (plist-put state-data :failure-message
					      (format "Failed to retrieve %s"
						      service-url))
				   (fsm-send fsm :unrecoverable-error))
			       (plist-put state-data
					  :service-xml
					  (exco--parse-xml-in-current-buffer))
			       (fsm-send fsm :success))
			   (and (buffer-live-p data-buffer)
				(kill-buffer data-buffer)))))))
     (list :retrieving-service-xml state-data nil))
    (:success
     (plist-put state-data :next-state-after-success :retrieving-data)
     (list :parsing-service-wsdl state-data nil))))

(define-enter-state exco--fsm :parsing-service-wsdl
  (fsm state-data)
  (let* ((url (plist-get state-data :service-url))
	 (xml (plist-get state-data :service-xml))
	 (next-state (plist-get state-data :next-state-after-success))
	 (wsdl (soap-make-wsdl url)))
    (soap-parse-wsdl-phase-validate-node xml)
    ;; Skip soap-parse-wsdl-phase-fetch-imports to avoid synchronous
    ;; fetches of import URLs.
    (soap-parse-wsdl-phase-parse-schema xml wsdl)
    (plist-put state-data :service-wsdl wsdl)
    (exco--retrieve-next-import fsm state-data :enter next-state)))

(define-state exco--fsm :parsing-service-wsdl
  (fsm state-data event _callback)
  (if (eq event :unrecoverable-error)
      (list :shutting-down-on-error state-data nil)
    (let ((next-state (plist-get state-data :next-state-after-success)))
      (exco--retrieve-next-import fsm state-data :event next-state))))

(defun exco--get-server-version (wsdl)
  "Extract server version from WSDL."
  (catch 'found
    (dolist (attribute
	     (soap-xs-type-attributes
	      (soap-xs-element-type
	       (soap-wsdl-get
		'("http://schemas.microsoft.com/exchange/services/2006/types"
		  . "RequestServerVersion")
		wsdl 'soap-xs-element-p))))
      (when (equal (soap-xs-attribute-name attribute) "Version")
	(throw 'found (soap-xs-attribute-default attribute))))
    (warn "Excorporate: Failed to determine server version")
    nil))

(define-enter-state exco--fsm :retrieving-data
  (_fsm state-data)
  (let ((wsdl (plist-get state-data :service-wsdl))
	(identifier (plist-get state-data :identifier)))
    ;; Skip soap-parse-wsdl-phase-fetch-schema to avoid synchronous
    ;; URL fetches.
    (soap-parse-wsdl-phase-finish-parsing (plist-get state-data :service-xml)
					  wsdl)
    (exco--bind-wsdl
     (soap-wsdl-resolve-references wsdl)
     (plist-get state-data :service-url)
     "ExchangeServicePort"
     "http://schemas.microsoft.com/exchange/services/2006/messages"
     "ExchangeServiceBinding")
    (plist-put state-data :server-version (exco--get-server-version wsdl))
    (message "Excorporate: Connection %S is ready" identifier))
  (list state-data nil))

(define-state exco--fsm :retrieving-data
  (_fsm state-data event _callback)
  (let* ((identifier (plist-get state-data :identifier))
	 (wsdl (plist-get state-data :service-wsdl))
	 (name (pop event))
	 (arguments (pop event))
	 (callback (pop event)))
    (apply #'soap-invoke-async
	   (lambda (response)
	     (funcall callback identifier response))
	   nil
	   wsdl
	   "ExchangeServicePort"
	   name
	   arguments))
  (list :retrieving-data state-data nil))

(defmacro exco--with-fsm (identifier &rest body)
  "With `fsm' set to IDENTIFIER, run BODY.
Run BODY with `fsm' set to the finite state machine specified by
IDENTIFIER."
  (declare (indent 1) (debug t))
  `(progn
     (unless exco--connection-identifiers
       (error "Excorporate: No connections exist.  Run M-x excorporate"))
     (let* ((identifier (or ,identifier (car exco--connection-identifiers)))
	    (fsm (gethash identifier exco--connections)))
       (unless fsm
	 (error "Excorporate: Connection %S does not exist" ,identifier))
       ,@body)))

;; Developer-visible functions and variables.

(defun exco-api-version ()
  "Return the Excorporate API version.
Return a non-negative integer representing the current
Excorporate application programming interface version.  Version 0
is subject to change."
  0)

(defun exco-connect (identifier)
  "Connect or reconnect to a web service.
IDENTIFIER is the mail address to use for autodiscovery or a
pair (mail-address . service-url)."
  (if (stringp identifier)
      (message "Excorporate: Starting autodiscovery for %S"
	       identifier))
  (let ((fsm (start-exco--fsm identifier)))
    (unless exco--connections
      (setq exco--connections (make-hash-table :test 'equal)))
    (when (gethash identifier exco--connections)
      (exco-disconnect identifier))
    (puthash identifier fsm exco--connections)
    (push identifier exco--connection-identifiers)
    (if (stringp identifier)
	(fsm-send fsm :try-next-url)
      (fsm-send fsm :retrieve-xml))
    nil))

(defun exco-operate (identifier name arguments callback)
  "Execute a service operation asynchronously.
IDENTIFIER is the connection identifier or nil to use the default
connection.  Execute operation NAME with ARGUMENTS then call
CALLBACK with two arguments, IDENTIFIER and RESPONSE."
  (exco--with-fsm identifier
    (fsm-send fsm (list name arguments callback)))
  nil)

(defun exco-server-version (identifier)
  "Return the server version for connection IDENTIFIER, as a string.
Examples are \"Exchange2010\", \"Exchange2010_SP1\",
\"Exchange2013\"."
  (exco--with-fsm identifier
    (plist-get (fsm-get-state-data fsm) :server-version)))

(defun exco-disconnect (identifier)
  "Disconnect from a web service.
IDENTIFIER is the mail address used to look up the connection."
  (exco--with-fsm identifier
    (setq exco--connection-identifiers
	  (delete identifier exco--connection-identifiers))
    (remhash identifier exco--connections))
  nil)

(defun exco-extract-value (path result)
  "Extract the value at PATH from RESULT.
PATH is an ordered list of node names."
  (let ((values (nreverse (car result))))
    (dolist (path-element path)
      (setq values (assoc path-element values)))
    (cdr values)))

;; Date-time utility functions.
(defun exco-extend-timezone (date-time-string)
  "Add a colon to the timezone in DATE-TIME-STRING.
DATE-TIME-STRING must be formatted as if returned by
`format-time-string' with FORMAT-STRING \"%FT%T%z\".  Web
services require the ISO8601 extended format of timezone, which
includes the colon."
  (concat
   (substring date-time-string 0 22) ":" (substring date-time-string 22)))

(defun exco-format-date-time (time-internal)
  "Convert TIME-INTERNAL to an XSD compatible date-time string."
  (exco-extend-timezone
   (format-time-string "%FT%T%z" time-internal)))

;; User-visible functions and variables.
(defgroup excorporate nil
  "Exchange support."
  :version "25.1"
  :group 'comm
  :group 'calendar)

;; Name the excorporate-configuration variable vaguely.  It is currently a
;; MAIL-ADDRESS string, a pair (MAIL-ADDRESS . SERVICE-URL), or nil.  In the
;; future it could allow a list of strings and pairs.
(defcustom excorporate-configuration nil
  "Excorporate configuration.
The mail address to use for autodiscovery."
  :type '(choice
	  (const
	   :tag "Prompt for Exchange mail address to use for autodiscovery" nil)
	  (string :tag "Exchange mail address to use for autodiscovery")
	  (cons :tag "Skip autodiscovery"
		(string :tag "Exchange mail address")
		(string :tag "Exchange Web Services URL"))))

;;;###autoload
(defun excorporate ()
  "Start Excorporate.
Prompt for a mail address to use for autodiscovery, with an
initial suggestion of `user-mail-address'.  However, if
`excorporate-configuration' is non-nil, `excorporate' will use
that without prompting."
  (interactive)
  (cond
   ((eq excorporate-configuration nil)
    (exco-connect (completing-read "Exchange mail address: "
				   (list user-mail-address)
				   nil nil user-mail-address)))
   ((stringp excorporate-configuration)
    (exco-connect excorporate-configuration))
   ((null (consp (cdr excorporate-configuration)))
    (exco-connect excorporate-configuration))
   (t
    (error "Excorporate: Invalid configuration"))))

(provide 'excorporate)

;;; excorporate.el ends here
