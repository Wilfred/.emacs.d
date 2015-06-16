;;; exco-url-http-ntlm.el --- NTLM authentication for the url library

;; Copyright (C) 2008  Tom Schutzer-Weissmann

;; Author: Tom Schutzer-Weissmann <tom@schutzer-weissmann.net>
;; Keywords: comm, data, processes, hypermedia

;; This program is free software; you can redistribute it and/or modify
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
;;
;; This package provides a NTLM handler for the URL package.
;; It supports one username and password per server.
;;
;; Installation:
;;
;; Add the directory containing this file to the load path and then
;; load the url-http-ntlm package.  One way would be to add something
;; like the lines below to your .emacs file:
;;
;; (add-to-list 'load-path ".emacs.d/url-http-ntlm")
;; (require 'url-http-ntlm)
;;
;; Excorporate changes:
;;
;; - Add exco- prefix for traceability
;; - Add auth-source support
;;
;; Acknowledgements:
;;
;; Taro Kawagishi <tarok@transpulse.org> wrote ntlm.el and md4.el,
;; which are parts of FLIM (Faithful Library about Internet Message).
;;
;; http://stuff.mit.edu/afs/sipb/contrib/emacs/packages/flim-1.14.7/ntlm.el
;; http://stuff.mit.edu/afs/sipb/contrib/emacs/packages/flim-1.14.7/md4.el

;;; Code:
(require 'url-auth)
;; Remove authentication after redirect.
(require (intern (format "exco-url-http-%d.%d"
                         emacs-major-version
                         emacs-minor-version)))
(require 'mail-parse)
(require 'exco-ntlm)
(require 'cl)


;; FIXME: Generalize url-http-ntlm to allow more than one
;; user/password combination to be stored per server.
(defvar url-http-ntlm-auth-storage nil
  "Authentication storage.

An alist that maps a server name to a pair of \(<username> <ntlm hashes>\).

The hashes are built using `ntlm-get-password-hashes'.
The username can contain the domain name, in the form \"user@domain\".

Note that for any server, only one user and password is ever stored.")


(defvar url-http-ntlm-loop-timer-counter nil
  "A hash table used to detect NTLM negotiation errors.
Keys are urls, entries are (START-TIME . COUNTER).")


(defun url-ntlm-detect-loop (url)
  "Detect potential infinite loop when NTLM fails on URL."
  (when (not url-http-ntlm-loop-timer-counter)
    (setq url-http-ntlm-loop-timer-counter (make-hash-table :test 'equal)))
  (let* ((url-string (url-recreate-url url))
         (last-entry (gethash url-string url-http-ntlm-loop-timer-counter))
         (start-time (car last-entry))
         (counter (cdr last-entry)))
    (if last-entry
        (progn
          (if (< (-  (float-time) start-time) 10.0)
              (if (< counter 20)
                  ;; Still within time window, so increment count.
                  (puthash url-string (cons start-time (1+ counter))
                           url-http-ntlm-loop-timer-counter)
                ;; Error detected, so remove entry and clear.
                (url-http-ntlm-authorisation url-string :clear)
                (remhash url-string url-http-ntlm-loop-timer-counter)
                (error
                 (format (concat "Access rate to %s is too high,"
                                 " indicating an NTLM failure;"
                                 " to debug, re-run with url-debug set to 1")
                         url-string)))
            ;; Timeout expired, so reset counter.
            (puthash url-string (cons (float-time) 0)
                     url-http-ntlm-loop-timer-counter)))
      ;; New access, so initialize counter to 0.
      (puthash url-string (cons (float-time) 0)
               url-http-ntlm-loop-timer-counter))))

(defun url-ntlm-auth (url &optional prompt overwrite realm args)
  "Return an NTLM Authorization header.

Get the contents of the Authorization header for a HTTP response
using NTLM authentication, to access URL.

Because NTLM is a two-step process, this function expects to be
called twice, first to generate the NTLM type 1
message (request), then to respond to the server's type 2
message (challenge) with a suitable response.

PROMPT, OVERWRITE, and REALM are ignored.

ARGS is expected to contain the WWW-Authentication header from
the server's last response.  These are used by
`url-http-get-stage' to determine what stage we are at."
  (url-ntlm-ensure-keepalive)
  (let ((stage (url-ntlm-get-stage args)))
    (case stage
      ;;
      ;; NTLM Type 1 message: the request
      (:request
       (url-ntlm-detect-loop url)
       (destructuring-bind (&optional server user hash)
           (url-http-ntlm-authorisation url nil realm)
         (when server
           (url-http-ntlm-string (ntlm-build-auth-request user server)))))
      ;;
      ;; NTLM Type 3 message: the response
      (:response
       (url-ntlm-detect-loop url)
       (let ((challenge (url-http-ntlm-get-challenge)))
         (destructuring-bind (server user hash)
             (url-http-ntlm-authorisation url nil realm)
           (url-http-ntlm-string (ntlm-build-auth-response challenge
                                                           user
                                                           hash)))))
      (:error
       (url-http-ntlm-authorisation url :clear)))))


(defun url-ntlm-ensure-keepalive ()
  "Report an error if `url-http-attempt-keepalives' is not set."
  (assert url-http-attempt-keepalives
          nil
          "NTLM authentication won't work unless\
 `url-http-attempt-keepalives' is set!"))


(defun url-ntlm-clean-headers ()
  "Remove Authorization element from `url-http-extra-headers' alist."
  (declare (special url-http-extra-headers))
  (setq url-http-extra-headers
        (url-http-ntlm-rmssoc "Authorization" url-http-extra-headers)))


(defvar url-ntlm-last-args nil
  "Stores the last ARGS argument to `url-ntlm-get-stage' and the return value.

This is used to detect multiple calls.")
(make-variable-buffer-local 'url-ntlm-last-args)


(defun url-ntlm-get-stage (args)
  "Determine what stage of the NTLM handshake we are at.

PROMPT and ARGS come from `url-ntlm-auth''s caller,
`url-get-authentication'.  Their meaning depends on the current
implementation - this function is well and truly coupled...

url-get-authentication' calls `url-ntlm-auth' once when checking
what authentication schemes are supported (PROMPT and ARGS are
nil), and then twice for every stage of the handshake: the first
time PROMPT is nil, the second, t; ARGS contains the server
response's \"WWW-Authenticate\" header, munged by
`url-parse-args'."
  (declare (special url-http-extra-headers))
  (let* ((response-rxp     "^NTLM TlRMTVNTUAADAAA")
         (challenge-rxp    "^TLRMTVNTUAACAAA")
         (auth-header      (assoc "Authorization" url-http-extra-headers))
         (case-fold-search t)
         stage)
    (if (eq args (car url-ntlm-last-args))
        ;; multiple calls, return the same argument we returned last time
        (cdr url-ntlm-last-args)
      ;;
      (let ((stage
             (cond ((and auth-header (string-match response-rxp
                                                   (cdr auth-header)))
                    :error)
                   ((and (= (length args) 2)
                         (destructuring-bind (challenge ntlm) args
                           (and (string-equal "ntlm" (car ntlm))
                                (string-match challenge-rxp
                                              (car challenge)))))
                    :response)
                   (t
                    :request))))
        (url-ntlm-clean-headers)
        (setq url-ntlm-last-args (cons args stage))
        stage))))


(defun url-http-ntlm-authorisation (url &optional clear realm)
  "Get or clear NTLM authentication details for URL.

If CLEAR is non-nil, clear any saved credentials for server.
Otherwise, return the credentials, prompting the user if
necessary.  REALM is appears in the prompt.

If URL contains a username and a password, they are used and
stored credentials are not affected.

Note that for any server, only one user and password is ever
stored."
  (let* ((href   (if (stringp url)
                     (url-generic-parse-url url)
                   url))
         (server (url-host href))
         (type   (url-type href))
         (user   (url-user href))
         (pass   (url-password href))
         (stored (assoc server url-http-ntlm-auth-storage))
         (both   (and user pass)))
    (if clear
        ;; clear
        (unless both
          (setq url-http-ntlm-auth-storage
                (url-http-ntlm-rmssoc server url-http-ntlm-auth-storage))
          nil)
      ;; get
      (if (or both
              (and stored user (not (equal user (second stored))))
              (not stored))
          (let* ((user*  (if both
                             user
                           (or (url-do-auth-source-search server type :user)
                               (read-string (url-auth-user-prompt url realm)
                                            (or user (user-real-login-name))))))
                 (pass*  (if both
                             pass
                           (or (url-do-auth-source-search server type :secret)
                               (read-passwd "Password: "))))
                 (entry `(,server . (,user*
                                     ,(ntlm-get-password-hashes pass*)))))
            (unless both
              (setq url-http-ntlm-auth-storage
                    (cons entry
                          (url-http-ntlm-rmssoc server
                                                url-http-ntlm-auth-storage))))
            entry)
        ;;
        stored))))


(defun url-http-ntlm-get-challenge ()
  "Return the NTLM Type-2 message in the WWW-Authenticate header, if present."
  (save-restriction
    (mail-narrow-to-head)
    (let ((www-authenticate (mail-fetch-field "www-authenticate")))
      (when (string-match "NTLM\\s-+\\(\\S-+\\)"
                          www-authenticate)
        (base64-decode-string (match-string 1 www-authenticate))))))


(defun url-http-ntlm-rmssoc (key alist)
  "Remove all elements whose `car' match KEY from ALIST."
  (let (result)
    (dolist (element alist)
      (when (not (equal (car element) key))
        (push element result)))
    (nreverse result)))


(defun url-http-ntlm-string (data)
  "Return DATA encoded as an NTLM string."
  (concat "NTLM " (base64-encode-string data :nobreak)))


(url-register-auth-scheme "ntlm" nil 8)


(provide 'exco-url-http-ntlm)
;;; exco-url-http-ntlm.el ends here
