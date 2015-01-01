;; jabber-console.el - XML Console mode

;; Copyright (C) 2009, 2010 - Demyan Rogozhin <demyan.rogozhin@gmail.com>

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

;;; Commentary:

;; Use *-jabber-console-* for sending custom XMPP code. Be careful!

;;; Code:

(require 'jabber-keymap)
(require 'jabber-util)
(require 'ewoc)
(require 'sgml-mode) ;we base on this mode to hightlight XML

(defcustom jabber-console-name-format "*-jabber-console-%s-*"
  "Format for console buffer name. %s mean connection jid."
  :type 'string
  :group 'jabber-debug)

(defcustom jabber-console-truncate-lines 3000
  "Maximum number of lines in console buffer.
Not truncate if set to 0"
  :type 'integer
  :group 'jabber-debug)

(defvar jabber-point-insert nil
  "Position where the message being composed starts")

(defvar jabber-send-function nil
  "Function for sending a message from a chat buffer.")

(defvar jabber-console-mode-hook nil
  "Hook called at the end of `jabber-console-mode'.
Note that functions in this hook have no way of knowing
what kind of chat buffer is being created.")

(defvar jabber-console-ewoc nil
  "The ewoc showing the XML elements of this stream buffer.")

(defvar jabber-console-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jabber-common-keymap)
    (define-key map "\r" 'jabber-chat-buffer-send)
    map))

(defun jabber-console-create-buffer (jc)
  (with-current-buffer
	  (get-buffer-create (format jabber-console-name-format (jabber-connection-bare-jid jc)))
    (unless (eq major-mode 'jabber-console-mode)
      (jabber-console-mode))
    ;; Make sure the connection variable is up to date.
    (setq jabber-buffer-connection jc)
    (current-buffer)))

(defun jabber-console-send (jc data)
  ;; Put manual string into buffers ewoc
  (jabber-process-console jc "raw" data)
  ;; ...than sent it to server
  (jabber-send-string jc data))

(defun jabber-console-comment (str)
  "Insert comment into console buffer."
  (let ((string (concat
                 comment-start str "@" (jabber-encode-time (current-time)) ":"
                 comment-end "\n")))
    (when (stringp jabber-debug-log-xml)
      (jabber-append-string-to-file string jabber-debug-log-xml))
    (insert string)))

(defun jabber-console-pp (data)
  "Pretty Printer for XML-sexp and raw data"
  (let ((direction (car data))
        (xml-list (cdr data))
        (raw (cadr data)))
    (jabber-console-comment direction)
    (if (stringp raw)
        ;; raw code input
        (progn
          (insert raw)
          (when (stringp jabber-debug-log-xml)
            (jabber-append-string-to-file raw jabber-debug-log-xml)))
      ;; receive/sending
      (progn
        (xml-print xml-list)
        (when (stringp jabber-debug-log-xml)
          (jabber-append-string-to-file
           "\n" jabber-debug-log-xml 'xml-print xml-list))))))

(define-derived-mode jabber-console-mode sgml-mode "Jabber Console"
  "Major mode for debug XMPP protocol"
  ;; Make sure to set this variable somewhere
  (make-local-variable 'jabber-send-function)
  (make-local-variable 'jabber-point-insert)
  (make-local-variable 'jabber-console-ewoc)

  (setq jabber-send-function 'jabber-console-send)

  (unless jabber-console-ewoc
    (setq jabber-console-ewoc
	  (ewoc-create #'jabber-console-pp nil "<!-- + -->"))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))
    (setq jabber-point-insert (point-marker))))

(put 'jabber-console-mode 'mode-class 'special)

(defun jabber-console-sanitize (xml-data)
  "Sanitize XML-DATA for jabber-process-console"
  (if (listp xml-data)
      (jabber-tree-map (lambda (x) (if (numberp x) (format "%s" x) x)) xml-data)
    xml-data))

;;;###autoload
(defun jabber-process-console (jc direction xml-data)
  "Log XML-DATA i/o as XML in \"*-jabber-console-JID-*\" buffer"
  (let ((buffer (get-buffer-create (jabber-console-create-buffer jc))))
    (with-current-buffer buffer
      (progn
        (ewoc-enter-last jabber-console-ewoc (list direction (jabber-console-sanitize xml-data)))
		(when (< 1  jabber-console-truncate-lines)
		  (let ((jabber-log-lines-to-keep jabber-console-truncate-lines))
			(jabber-truncate-top buffer jabber-console-ewoc)))))))

(provide 'jabber-console)
;;; jabber-console.el ends here
