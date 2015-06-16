;;; excorporate-calfw.el --- Exchange calendar view   -*- lexical-binding: t -*-

;; Copyright (C) 2014  Thomas Fitzsimmons

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: calendar

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

;; Use the Calfw calendar framework to display daily meetings.

;;; Code:

(require 'calfw)
(require 'excorporate)
(eval-when-compile (require 'cl))

(defun exco-calfw-maybe-extra-argument (server-version)
  "If SERVER-VERSION is less than 2013 then return an empty list.
Otherwise return a list with one nil element."
  (if (< ( string-to-number (substring server-version 8 12)) 2013)
      '()
    '(nil)))

(defun exco-calfw-get-meetings-for-day (month day year callback)
  "Return the meetings for the specified day.
MONTH, DAY and YEAR are the meeting month, day and year.  Call
CALLBACK with two arguments, IDENTIFIER and RESPONSE."
  (let* ((start-of-day-time-internal
	  (apply #'encode-time `(0 0 0 ,day ,month ,year)))
	 (start-of-day-date-time
	  (exco-format-date-time start-of-day-time-internal))
	 (start-of-next-day-date-time
	  (exco-extend-timezone
	   (format-time-string "%FT00:00:00%z"
			       (time-add start-of-day-time-internal
					 (seconds-to-time 86400))))))
    (exco-operate
     nil
     "FindItem"
     `(;; Main arguments.
       ((Traversal . "Shallow")
	(ItemShape
	 (BaseShape . "AllProperties"))
	;; To aid productivity, excorporate-calfw automatically prunes your
	;; meetings to a maximum of 100 per day.
	(CalendarView (MaxEntriesReturned . "100")
		      (StartDate . ,start-of-day-date-time)
		      (EndDate . ,start-of-next-day-date-time))
	(ParentFolderIds
	 (DistinguishedFolderId (Id . "calendar"))))
       ;; Empty arguments.
       nil nil nil nil nil ,@(exco-calfw-maybe-extra-argument
			      (exco-server-version nil)))
     callback)))

(defun exco-calfw-show-day (month day year)
  "Show meetings for the date specified by MONTH DAY YEAR."
  ;; Binds buffer-local cfw:component.
  (with-current-buffer (get-buffer-create "*Excorporate*")
    (display-buffer (current-buffer))
    (cfw:create-calendar-component-buffer
     :date (cfw:date month day year) :view 'day
     :contents-sources (list (make-cfw:source :name "(updating...)"
					      :data (lambda (_b _e) nil)))
     :buffer (current-buffer)))
  (exco-calfw-get-meetings-for-day
   month day year
   (lambda (identifier response)
     (with-current-buffer (get-buffer-create "*Excorporate*")
       (let (event-list)
	 (dolist (calendar-item
		  (exco-extract-value '(ResponseMessages
					FindItemResponseMessage
					RootFolder
					Items)
				      response))
	   (let* ((subject (cdr (assoc 'Subject calendar-item)))
		  (start (cdr (assoc 'Start calendar-item)))
		  (start-list (decode-time (apply #'encode-time
						  (soap-decode-date-time
						   start 'dateTime))))
		  (end (cdr (assoc 'End calendar-item)))
		  (end-list (decode-time (apply #'encode-time
						(soap-decode-date-time
						 end 'dateTime))))
		  (location (cdr (assoc 'Location calendar-item)))
		  (main-invitees (cdr (assoc 'DisplayTo calendar-item)))
		  (optional-invitees (cdr (assoc 'DisplayCc calendar-item))))
	     (push
	      (make-cfw:event :title (concat
				      (format
				       "\n\t%s\n\tLocation: %s\n\tInvitees: %s"
				       subject location main-invitees)
				      (when optional-invitees
					(format "\n\tOptional: %s"
						optional-invitees)))
			      :start-date  (list (elt start-list 4)
						 (elt start-list 3)
						 (elt start-list 5))
			      :start-time  (list (elt start-list 2)
						 (elt start-list 1))
			      :end-date	 (list (elt end-list 4)
					       (elt end-list 3)
					       (elt end-list 5))
			      :end-time	 (list (elt end-list 2)
					       (elt end-list 1)))
	      event-list)))
	 (declare (special cfw:component))
	 ;; Don't call cfw:cp-set-contents-sources because it has a
	 ;; bug where its arguments are transposed.
	 (cfw:model-set-contents-sources
	  (list (make-cfw:source
		 :name (format "%S (as of %s)"
			       identifier
			       (format-time-string "%FT%T%z"))
		 :data (lambda (_b _e)
			 event-list)))
	  (cfw:component-model cfw:component)))
       (cfw:cp-add-selection-change-hook
	cfw:component
	(lambda ()
	  (apply #'exco-calfw-show-day (cfw:cursor-to-nearest-date))))
       (cfw:refresh-calendar-buffer nil)))))

(provide 'excorporate-calfw)

;;; excorporate-calfw.el ends here
