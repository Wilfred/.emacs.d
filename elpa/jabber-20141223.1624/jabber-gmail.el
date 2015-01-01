;;; jabber-gmail.el --- Gmail notifications via emacs-jabber

;; Copyright (C) 2008  Magnus Henoch <mange@freemail.hu>
;; Copyright (C) 2007  Valery V. Vorotyntsev <valery.vv@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Usage:

;; Add the following line to your ~/.emacs:
;;
;;   (require 'jabber-gmail)
;;
;; If you prefer on demand loading
;; [http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html]:
;;
;;     (autoload 'jabber-gmail-query     "jabber-gmail")
;;     (autoload 'jabber-gmail-subscribe "jabber-gmail")
;;     (add-hook 'jabber-post-connect-hook 'jabber-gmail-subscribe)
;;
;; You may wish to bind a shortcut for `jabber-gmail-query'
;;
;;   (global-set-key (kbd "<f9> g") 'jabber-gmail-query)
;;
;; or to customize `jabber-gmail-dothreads'
;;
;;   (defun jabber-gmail-dothreads (ts)
;;     (let ((msg (format "%d new messages in gmail inbox" (length ts))))
;;       (message msg)
;;       (jabber-screen-message msg)))

;;;###autoload
(defun jabber-gmail-subscribe (jc)
  "Subscribe to gmail notifications.
See http://code.google.com/apis/talk/jep_extensions/usersettings.html#4"
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc (jabber-connection-bare-jid jc) "set"
		  '(usersetting ((xmlns . "google:setting"))
				(mailnotifications ((value . "true"))))
		  #'jabber-report-success "Gmail subscription"
		  #'jabber-process-data   "Gmail subscription")

  ;; Looks like "one shot" request is still needed to activate
  ;; notifications machinery.
  (jabber-gmail-query jc))

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "google:mail:notify" #'jabber-gmail-process-new-mail))
(defun jabber-gmail-process-new-mail (jc xml-sexp)
  "Process new gmail notification.
See http://code.google.com/apis/talk/jep_extensions/gmail.html#notifications"
  (let ((from (jabber-xml-get-attribute xml-sexp 'from))
	(id (jabber-xml-get-attribute xml-sexp 'id)))
    ;; respond to server
    (jabber-send-iq jc from "result" nil
		    nil nil nil nil
		    id))

  (jabber-gmail-query jc))

;;;###autoload
(defun jabber-gmail-query (jc)
  "Request mail information from the Google Talk server (a.k.a. one shot query).
See http://code.google.com/apis/talk/jep_extensions/gmail.html#requestmail"
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc (jabber-connection-bare-jid jc) "get"
		  '(query ((xmlns . "google:mail:notify")))
		  #'jabber-gmail-process-mailbox nil
		  #'jabber-process-data "Gmail query" "gmail-query"))

(defun jabber-gmail-process-mailbox (jc xml-sexp &rest ignore)
  "Process gmail query response.
See http://code.google.com/apis/talk/jep_extensions/gmail.html#response"
  (let ((ts (jabber-xml-node-children
	     (car (jabber-xml-get-children xml-sexp 'mailbox)))))
    (when ts (jabber-gmail-dothreads ts))))

(defun jabber-gmail-dothreads (threads)
  "Process <mail-thread-info/> elements.
THREADS is a list of XML sexps, corresponding to <mail-thread-info/> elements.
See http://code.google.com/apis/talk/jep_extensions/gmail.html#response"
  (message "%d new messages in gmail inbox" (length threads)))

(provide 'jabber-gmail)
;; arch-tag: 102bc8e4-e08f-11dc-ab66-000a95c2fcd0
