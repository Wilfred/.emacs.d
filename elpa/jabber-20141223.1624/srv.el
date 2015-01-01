;;; srv.el --- perform SRV DNS requests

;; Copyright (C) 2005, 2007  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>
;; Keywords: comm
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code implements RFC 2782 (SRV records).  It requires a version
;; of dns.el that supports SRV records; look in Gnus CVS if you don't
;; have one.

;;; Code:

(condition-case nil
    (require 'dns)
  (error nil))
(eval-when-compile (require 'cl))

(defun srv-lookup (target)
  "Perform SRV lookup of TARGET and return list of connection candidiates.
TARGET is a string of the form \"_Service._Proto.Name\".

Returns a list with elements of the form (HOST . PORT), where HOST is
a hostname and PORT is a numeric port.  The caller is supposed to
make connection attempts in the order given, starting from the beginning
of the list.  The list is empty if no SRV records were found."
  (unless (boundp 'dns-query-types)
    (error "No dns.el available"))
  (unless (assq 'SRV dns-query-types)
    (error "dns.el doesn't support SRV lookups"))
  ;; `dns-query' used to be `query-dns'.  Try both names for now.
  (let* ((result (srv--dns-query target))
	 (answers (mapcar #'(lambda (a)
			      (cadr (assq 'data a)))
			  (cadr (assq 'answers result))))
	 answers-by-priority weighted-result)
    (if (or (null answers)
	    ;; Special case for "service decidedly not available"
	    (and (eq (length answers) 1)
		 (string= (cadr (assq 'target (car answers))) ".")))
	nil
      ;; Sort answers into groups of same priority.
      (dolist (a answers)
	(let* ((priority (cadr (assq 'priority a)))
	       (entry (assq priority answers-by-priority)))
	  (if entry
	      (push a (cdr entry))
	    (push (cons priority (list a)) answers-by-priority))))
      ;; Sort by priority.
      (setq answers-by-priority
	    (sort answers-by-priority 
		  #'(lambda (a b) (< (car a) (car b)))))
      ;; Randomize by weight within priority groups.  See
      ;; algorithm in RFC 2782.
      (dolist (p answers-by-priority)
	(let ((weight-acc 0)
	      weight-order)
	  ;; Assign running sum of weight to each entry.
	  (dolist (a (cdr p))
	    (incf weight-acc (cadr (assq 'weight a)))
	    (push (cons weight-acc a) weight-order))
	  (setq weight-order (nreverse weight-order))

	  ;; While elements remain, pick a random number between 0 and
	  ;; weight-acc inclusive, and select the first entry whose
	  ;; running sum is greater than or equal to this number.
	  (while weight-order
	    (let* ((r (random (1+ weight-acc)))
		   (next-entry (dolist (a weight-order)
				 (if (>= (car a) r)
				     (return a)))))
	      (push (cdr next-entry) weighted-result)
	      (setq weight-order
		    (delq next-entry weight-order))))))
      ;; Extract hostnames and ports
      (mapcar #'(lambda (a) (cons (cadr (assq 'target a))
			     (cadr (assq 'port a))))
	      (nreverse weighted-result)))))

(defun srv--dns-query (target)
  ;; dns-query uses UDP, but that is not supported on Windows...
  (if (featurep 'make-network-process '(:type datagram))
      (if (fboundp 'query-dns)
          (query-dns target 'SRV t)
        (dns-query target 'SRV t))
    ;; ...so let's call nslookup instead.
    (srv--nslookup target)))

(defun srv--nslookup (target)
  (with-temp-buffer
    (call-process "nslookup" nil t nil "-type=srv" target)
    (goto-char (point-min))
    (let (results)
      (while (search-forward-regexp
              (concat "[\s\t]*priority += \\(.*\\)\r?\n"
                      "[\s\t]*weight += \\(.*\\)\r?\n"
                      "[\s\t]*port += \\(.*\\)\r?\n"
                      "[\s\t]*svr hostname += \\(.*\\)\r?\n")
              nil t)
        (push
         (list
          (list 'data
                (list
                 (list 'priority (string-to-number (match-string 1)))
                 (list 'weight (string-to-number (match-string 2)))
                 (list 'port (string-to-number (match-string 3)))
                 (list 'target (match-string 4)))))
         results))
      (list (list 'answers results)))))

(provide 'srv)
;; arch-tag: b43358f2-d241-11da-836e-000a95c2fcd0
;;; srv.el ends here
