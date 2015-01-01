;;; jabber-muc-nick-completion.el --- Add nick completion abilyty to emacs-jabber

;; Copyright (C) 2008 - Terechkov Evgenii - evg@altlinux.org
;; Copyright (C) 2007, 2008, 2010 - Kirill A. Korinskiy - catap@catap.ru
;; Copyright (C) 2007 - Serguei Jidkov - jsv@e-mail.ru

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

;;; User customizations here:
(defcustom jabber-muc-completion-delimiter ": "
  "String to add to end of completion line."
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-muc-looks-personaling-symbols '("," ":" ">")
  "Symbols for personaling messages"
  :type '(repeat string)
  :group 'jabber-chat)

(defcustom jabber-muc-personal-message-bonus (* 60 20)
  "Bonus for personal message, in seconds."
  :type 'integer
  :group 'jabber-chat)

(defcustom jabber-muc-all-string "all"
  "String meaning all conference members (to insert in completion). Note that \":\" or alike not needed (it appended in other string)"
  :type 'string
  :group 'jabber-chat)

;;; History:
;; 

;;; Code:

(require 'cl)
(require 'jabber-muc)
(require 'hippie-exp)

(defvar *jabber-muc-participant-last-speaking* nil
  "Global alist in form (group . ((member . time-of-last-speaking) ...) ...).")

(defun jabber-my-nick (&optional group)
  "Return my jabber nick in GROUP."
  (let ((room (or group jabber-group)))
    (cdr (or (assoc room *jabber-active-groupchats*)
             (assoc room jabber-muc-default-nicknames)))
    ))

;;;###autoload
(defun jabber-muc-looks-like-personal-p (message &optional group)
  "Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look."
  (if message (string-match (concat
		 "^"
		 (jabber-my-nick group)
		 (regexp-opt jabber-muc-looks-personaling-symbols))
		message)
    nil))

(defun jabber-muc-nicknames ()
  "List of conference participants, excluding self, or nil if we not in conference."
  (delete-if '(lambda (nick)
		 (string= nick (jabber-my-nick)))
	     (append (mapcar 'car (cdr (assoc jabber-group jabber-muc-participants))) (list jabber-muc-all-string))))

(defun jabber-muc-participant-update-activity (group nick time)
  "Updates NICK's time of last speaking in GROUP to TIME."
  (let* ((room (assoc group *jabber-muc-participant-last-speaking*))
         (room-activity (cdr room))
         (entry (assoc nick room-activity))
         (old-time (or (cdr entry) 0)))
    (when (> time old-time)
      ;; don't use put-alist for speed
      (progn
        (if entry (setcdr entry time)
          (setq room-activity
                (cons (cons nick time) room-activity)))
        (if room (setcdr room room-activity)
          (setq *jabber-muc-participant-last-speaking*
                (cons (cons group room-activity)
                      *jabber-muc-participant-last-speaking*)))))))

(defun jabber-muc-track-message-time (nick group buffer text &optional title)
  "Tracks time of NICK's last speaking in GROUP."
  (when nick
    (let ((time (float-time)))
      (jabber-muc-participant-update-activity
       group
       nick
       (if (jabber-muc-looks-like-personal-p text group)
	   (+ time jabber-muc-personal-message-bonus)
	 time)))))

(defun jabber-sort-nicks (nicks group)
  "Return list of NICKS in GROUP, sorted."
  (let ((times (cdr (assoc group *jabber-muc-participant-last-speaking*))))
    (flet ((fetch-time (nick) (or (assoc nick times) (cons nick 0)))
	   (cmp (nt1 nt2)
		(let ((t1 (cdr nt1))
		      (t2 (cdr nt2)))
		  (if (and (zerop t1) (zerop t2))
		      (string<
                       (car nt1) 
                       (car nt2))
		    (> t1 t2)))))
      (mapcar 'car (sort (mapcar 'fetch-time nicks)
			  'cmp)))))

(defun jabber-muc-beginning-of-line ()
  "Return position of line begining."
  (save-excursion
    (if (looking-back jabber-muc-completion-delimiter)
        (backward-char (+ (length jabber-muc-completion-delimiter) 1)))
    (skip-syntax-backward "^-")
    (point)))

;;; One big hack:
(defun jabber-muc-completion-delete-last-tried ()
  "Delete last tried competion variand from line."
  (let ((last-tried (car he-tried-table)))
    (when last-tried
      (goto-char he-string-beg)
      (delete-char (length last-tried))
      (ignore-errors (delete-char (length jabber-muc-completion-delimiter)))
      )))

(defun try-expand-jabber-muc (old)
  "Try to expand target nick in MUC according to last speaking time.
OLD is last tried nickname."
  (unless jabber-chatting-with
    (unless old
    (let ((nicknames (jabber-muc-nicknames)))
      (he-init-string (jabber-muc-beginning-of-line) (point))
      (setq he-expand-list (jabber-sort-nicks (all-completions he-search-string (mapcar 'list nicknames)) jabber-group))))
  
  (setq he-expand-list
	(delete-if '(lambda (x)
		       (he-string-member x he-tried-table))
		   he-expand-list))
  (if (null he-expand-list)
      (progn
	(when old
          ;; here and later : its hack to workaround
          ;; he-substitute-string work which cant substitute empty
          ;; lines
	  (if (string= he-search-string "")
	      (jabber-muc-completion-delete-last-tried)
	    (he-reset-string)))
	())
    (let ((subst (if (eq (line-beginning-position) (jabber-muc-beginning-of-line))
                     (concat (car he-expand-list) jabber-muc-completion-delimiter)
                   (car he-expand-list))))
      (if (not (string= he-search-string ""))
	  (he-substitute-string subst)
	(jabber-muc-completion-delete-last-tried)
	(progn
          (insert subst)
          (if (looking-back (concat "^" (regexp-quote (car he-expand-list))))
              (unless (looking-back (concat "^" (regexp-quote (car he-expand-list)) jabber-muc-completion-delimiter))
                (insert jabber-muc-completion-delimiter)))
          )
        ))
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (setq he-expand-list (cdr he-expand-list))
    t)))

(add-hook 'jabber-muc-hooks 'jabber-muc-track-message-time)
(fset 'jabber-muc-completion (make-hippie-expand-function '(try-expand-jabber-muc)))
(define-key jabber-chat-mode-map [?\t] 'jabber-muc-completion)

(provide 'jabber-muc-nick-completion)

;; arch-tag: 2a81ac72-d261-11dc-be91-000a95c2fcd0
;;; jabber-muc-completion.el ends here
