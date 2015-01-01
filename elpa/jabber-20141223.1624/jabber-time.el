;; jabber-time.el - time reporting by XEP-0012, XEP-0090, XEP-0202

;; Copyright (C) 2006, 2010 - Kirill A. Kroinskiy - catap@catap.ru
;; Copyright (C) 2006 - Magnus Henoch - mange@freemail.hu

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'jabber-iq)
(require 'jabber-util)
(require 'jabber-autoaway)

(require 'time-date)

(add-to-list 'jabber-jid-info-menu (cons "Request time" 'jabber-get-time))

(defun jabber-get-time (jc to)
  "Request time"
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing "Request time of: "
                                                 nil nil nil 'full t)))

  (jabber-send-iq jc to "get"
                  '(time ((xmlns . "urn:xmpp:time")))
                  'jabber-silent-process-data 'jabber-process-time
                  'jabber-silent-process-data
                  (lambda (jc xml-data)
                    (let ((from (jabber-xml-get-attribute xml-data 'from)))
                      (jabber-get-legacy-time jc from)))))

(defun jabber-get-legacy-time (jc to)
  "Request legacy time"
  (interactive (list (jabber-read-account)
                     (jabber-read-jid-completing "Request time of: "
                                                 nil nil nil 'full t)))

  (jabber-send-iq jc to
                  "get"
                  '(query ((xmlns . "jabber:iq:time")))
                  'jabber-silent-process-data 'jabber-process-legacy-time
                  'jabber-silent-process-data "Time request failed"))


;; called by jabber-process-data
(defun jabber-process-time (jc xml-data)
  "Handle results from urn:xmpp:time requests."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
         (time (or (car (jabber-xml-get-children xml-data 'time))
                   ;; adium response of qeury
                   (car (jabber-xml-get-children xml-data 'query))))
         (tzo (car (jabber-xml-node-children
                    (car (jabber-xml-get-children time 'tzo)))))
         (utc (car (jabber-xml-node-children
                    (car (jabber-xml-get-children time 'utc))))))
    (when (and utc tzo)
      (format "%s has time: %s %s"
              from (format-time-string "%Y-%m-%d %T" (jabber-parse-time utc)) tzo))))

(defun jabber-process-legacy-time (jc xml-data)
  "Handle results from jabber:iq:time requests."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
         (query (jabber-iq-query xml-data))
         (display
          (car (jabber-xml-node-children
                (car (jabber-xml-get-children
                      query 'display)))))
         (utc
          (car (jabber-xml-node-children
                (car (jabber-xml-get-children
                      query 'utc)))))
         (tz
          (car (jabber-xml-node-children
                (car (jabber-xml-get-children
                      query 'tz))))))
    (format "%s has time: %s" from
           (cond
            (display display)
            (utc
             (concat
              (format-time-string "%Y-%m-%d %T" (jabber-parse-legacy-time utc))
              (when tz
                (concat " " tz))))))))

;; the only difference between these two functions is the
;; jabber-read-jid-completing call.
(defun jabber-get-last-online (jc to)
  "Request time since a user was last online, or uptime of a component."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Get last online for: "
						 nil nil nil 'bare-or-muc)))
  (jabber-send-iq jc to
		  "get"
		  '(query ((xmlns . "jabber:iq:last")))
		  #'jabber-silent-process-data #'jabber-process-last
		  #'jabber-silent-process-data "Last online request failed"))

(defun jabber-get-idle-time (jc to)
  "Request idle time of user."
  (interactive (list (jabber-read-account)
		     (jabber-read-jid-completing "Get idle time for: " 
						 nil nil nil 'full t)))
  (jabber-send-iq jc to
		  "get"
		  '(query ((xmlns . "jabber:iq:last")))
		  #'jabber-silent-process-data #'jabber-process-last
		  #'jabber-silent-process-data "Idle time request failed"))

(defun jabber-process-last (jc xml-data)
  "Handle resultts from jabber:iq:last requests."
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
	 (seconds (jabber-xml-get-attribute query 'seconds))
	 (message (car (jabber-xml-node-children query))))
    (cond
     ((jabber-jid-resource from)
      ;; Full JID: idle time
      (format "%s idle for %s seconds" from seconds))
     ((jabber-jid-username from)
      ;; Bare JID with username: time since online
      (concat
       (format "%s last online %s seconds ago" from seconds)
       (let ((seconds (condition-case nil
                          (string-to-number seconds)
                        (error nil))))
         (when (numberp seconds)
	   (concat
	    " - that is, at "
	    (format-time-string "%Y-%m-%d %T"
				(time-subtract (current-time)
					       (seconds-to-time seconds)))
	    "\n")))))
     (t
      ;; Only hostname: uptime
      (format "%s uptime: %s seconds" from seconds)))))

(add-to-list 'jabber-iq-get-xmlns-alist (cons "jabber:iq:time" 'jabber-return-legacy-time))
(jabber-disco-advertise-feature "jabber:iq:time")

(defun jabber-return-legacy-time (jc xml-data)
  "Return client time as defined in XEP-0090.  Sender and ID are
determined from the incoming packet passed in XML-DATA."
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq jc to "result"
		    `(query ((xmlns . "jabber:iq:time"))
			    ;; what is ``human-readable'' format?
			    ;; the same way as formating using by tkabber
			    (display () ,(format-time-string "%a %b %d %H:%M:%S %Z %Y"))
			    (tz () ,(format-time-string "%Z"))
			    (utc () ,(jabber-encode-legacy-time nil)))
		    nil nil nil nil
		    id)))

(add-to-list 'jabber-iq-get-xmlns-alist (cons "urn:xmpp:time" 'jabber-return-time))
(jabber-disco-advertise-feature "urn:xmpp:time")

(defun jabber-return-time (jc xml-data)
  "Return client time as defined in XEP-0202.  Sender and ID are
determined from the incoming packet passed in XML-DATA."
  (let ((to (jabber-xml-get-attribute xml-data 'from))
        (id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq jc to "result"
                    `(time ((xmlns . "urn:xmpp:time"))
                           (utc () ,(jabber-encode-time nil))
                           (tzo () ,(jabber-encode-timezone)))
                    nil nil nil nil
                    id)))

(add-to-list 'jabber-iq-get-xmlns-alist (cons "jabber:iq:last" 'jabber-return-last))
(jabber-disco-advertise-feature "jabber:iq:last")

(defun jabber-return-last (jc xml-data)
  (let ((to (jabber-xml-get-attribute xml-data 'from))
        (id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq jc to "result"
                    `(time ((xmlns . "jabber:iq:last")
			    ;; XEP-0012 specifies that this is an integer.
                            (seconds . ,(number-to-string
					 (floor (jabber-autoaway-get-idle-time))))))
                    nil nil nil nil
                    id)))


(provide 'jabber-time)

;; arch-tag: 5396bfda-323a-11db-ac8d-000a95c2fcd0
