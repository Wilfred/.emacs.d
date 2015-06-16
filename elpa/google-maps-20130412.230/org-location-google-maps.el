;;; org-location-google-maps.el --- Show Google Maps' map for an Org entry location

;; Copyright (C) 2010 Julien Danjou

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Integrate google-maps into org-mode.
;;
;; This allows you to press C-c M-l on an Org entry to get the Google Map of
;; your appointment.
;;
;;; Code:

(require 'google-maps)
(require 'google-maps-geocode)
(require 'org)
(require 'org-agenda)

(defcustom org-google-maps-location-properties '((if (boundp 'org-contacts-coordinates-property)
						     org-contacts-coordinates-property
						   "COORDINATES")
						 "LOCATION"
						 (if (boundp 'org-contacts-address-property)
						     org-contacts-address-property
						   "ADDRESS"))
  "Evaluated list of ORG properties storing location informations.
'COORDINATES' or 'org-contacts-coordinates-property' store GPS coordinates.
'LOCATION' store GPS coordinates, for backward compatibility.
'ADDRESS' or 'org-contacts-address-property' store Google geocoded address.

Each element of the list is evaluated at run time by
'org-google-maps-get-location'.")

(defun org-google-maps-get-location ()
  "Return the location information of the current entry.
The first defined property of
'org-google-maps-location-properties' is used."
  (let ((properties org-google-maps-location-properties)
	location)
    (while (and properties (null location))
      (setq location (org-entry-get nil (eval (car properties)) t))
      (setq properties (cdr properties)))
    location))

(defun org-google-maps (location &optional with-current-location)
  "Run Google Maps for LOCATION.
If WITH-CURRENT-LOCATION prefix is set, add a marker with current
location."
  (let ((buffer (google-maps location)))
    (when with-current-location
      (with-current-buffer buffer
        (google-maps-static-add-home-marker)))))

;;;###autoload
(defun org-location-google-maps (&optional with-current-location)
  "Show Google Map for location of an Org entry in an org buffer.
If WITH-CURRENT-LOCATION prefix is set, add a marker with current
location."
  (interactive "P")
  (let ((location (org-google-maps-get-location)))
    (when location
      (org-google-maps location with-current-location))))

;;;###autoload
(defun org-agenda-location-google-maps (&optional with-current-location)
  "Show Google Map for location of an Org entry in an org-agenda buffer."
  (interactive "P")
  (let ((location
         (save-window-excursion
           (org-agenda-goto)
           (org-google-maps-get-location))))
    (when location
      (org-google-maps location with-current-location))))

;;;###autoload
(defun org-address-google-geocode-set (location)
  "Set address property to LOCATION address for current entry using Google Geocoding API."
  (interactive
   (list (read-string "Location: ")))
  (org-set-property (if (boundp 'org-contacts-address-property)
			org-contacts-address-property
		      "ADDRESS")
		    (cdr (assoc 'formatted_address
				(google-maps-geocode-location location)))))

;;;###autoload
(defun org-coordinates-google-geocode-set (location)
  "Set coordinates property to LOCATION coordinates for current entry using Google Geocoding API."
  (interactive
   (list (read-string "Location: ")))
  (org-set-property (if (boundp 'org-contacts-coordinates-property)
			org-contacts-coordinates-property
		      "COORDINATES")
		    (mapconcat 'number-to-string
			       (google-maps-geocode-location->coordinates location) ",")))

;;;###autoload
(defun org-google-maps-key-bindings ()
  (require 'org)
  (define-key org-mode-map "\C-c\M-c" 'org-coordinates-google-geocode-set)
  (define-key org-mode-map "\C-c\M-L" 'org-address-google-geocode-set)
  (define-key org-mode-map "\C-c\M-A" 'org-address-google-geocode-set)
  (define-key org-mode-map "\C-c\M-l" 'org-location-google-maps))

;;;###autoload
(defun org-agenda-google-maps-key-bindings ()
  (require 'org-agenda)
  (define-key org-agenda-mode-map "\C-c\M-c" 'org-coordinates-google-geocode-set)
  (define-key org-agenda-mode-map "\C-c\M-L" 'org-address-google-geocode-set)
  (define-key org-agenda-mode-map "\C-c\M-A" 'org-address-google-geocode-set)
  (define-key org-agenda-mode-map "\C-c\M-l" 'org-location-google-maps))

;;;###autoload(eval-after-load "org" '(org-google-maps-key-bindings))
(eval-after-load "org" '(org-google-maps-key-bindings))
;;;###autoload(eval-after-load "org-agenda" '(org-agenda-google-maps-key-bindings))
(eval-after-load "org-agenda" '(org-agenda-google-maps-key-bindings))

(provide 'org-location-google-maps)
