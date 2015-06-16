;;; google-maps-geocode.el --- Geocode address using Google Maps service

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
;; None
;;; Code:
(eval-when-compile
  (require 'cl))

(require 'json)
(require 'google-maps-base)

(defconst google-maps-geocode-uri
  "http://maps.google.com/maps/api/geocode/json"
  "Google Maps Geocoding API server.")

(defun google-maps-geocode-build-url (plist)
  "Built a Google Maps Geocode API request URL from PLIST."
  (concat
   google-maps-geocode-uri "?"
   (google-maps-urlencode-plist
    plist
    '((address . url-hexify-string)
      ;; Sensor is MANDATORY
      (sensor . (lambda (v)
                  (if v "true" "false")))
      (latlng)
      (region)
      (bounds . (lambda (v)
                  (mapconcat 'identity v "|")))
      (language)))))

(defun google-maps-geocode-request->status (data)
  "Return status of a `google-maps-geocode' request.
It should returned `ok' if everything went well."
  (intern (downcase (cdr (assoc 'status data)))))

(defun google-maps-geocode-request-is-ok (data)
  "Return t if the geocoding via `google-maps-geocode' went fine."
  (eq (google-maps-geocode-request->status data) 'ok))

(defun google-maps-geocode-request->results (data)
  "Return result list of a `google-maps-geocode' request."
  (cdr (assoc 'results data)))

(defun google-maps-geocode-request (&rest params)
  "Request geocoding of a location and return the request result.
Request status can be retrieved with
`google-maps-geocode-request->status'. Request results data can
be retrieve ed with `google-maps-geocode-request->results'.

Valid params are:

  :address  The address to geocode.
  :sensor   Boolean indicating if this call is used for a sensor
            device.
  :latlng   Coordinates.
  :region   Region.
  :bounds   Bounding box.
  :language Language to use in returned data."
  (json-read-from-string
   (google-maps-retrieve-data
    (google-maps-geocode-build-url
     (google-maps-build-plist params)))))

(defun google-maps-geocode-results->one-result-picked-by-user (results)
  "Make the user choose a result from RESULTS, and return it."
  (let ((location
         ;; `location' will contains what the user choosed as location.
         (completing-read
          "Precise location: "
          (mapcar
           (lambda (x)
             (cdr (assoc 'formatted_address x)))
           results)
          nil t)))
    ;; Find entry with that location
    (find-if
     `(lambda (entry)
        (string= (cdr (assoc 'formatted_address entry))
                 ,location))
     results)))

(defun google-maps-geocode-results->one-result (results)
  "Converts geocoding results list to one result only.
If there is several results, the user is asked to pick one via
`google-maps-geocode-results->one-result-picked-by-user'."
  (case (length results)
    (0 nil)
    (1 (elt results 0))
    (t (google-maps-geocode-results->one-result-picked-by-user results))))

(defun google-maps-geocode-location (location)
  (let* ((req (google-maps-geocode-request :address location))
         (status (google-maps-geocode-request->status req)))
    (unless (eq status 'ok)
      (error (format "Unable to geocode %s: %s" location status)))
    (google-maps-geocode-results->one-result
     (google-maps-geocode-request->results req))))

(defun google-maps-geocode-location->coordinates (location)
  "Return a list containing latitude and longitude."
  (let ((geocode-location (google-maps-geocode-location location))
	latitude longitude)
    (if (null (assoc 'geometry geocode-location))
	(error (format "No geometry information for location: %s" location)))
    (setq latitude (cdr (assoc 'lat (assoc 'location (assoc 'geometry geocode-location)))))
    (setq longitude (cdr (assoc 'lng (assoc 'location (assoc 'geometry geocode-location)))))
    (if (or (null latitude) (null longitude))
	(error (format "Null location coordinates: %s,%s" latitude longitude)))
    (list latitude longitude)))

;;;###autoload
(defun google-maps-geocode-replace-region (beg end)
  "Geocode region and replace it with a more accurate result."
  (interactive "r")
  (let ((location (cdr (assoc 'formatted_address
                              (google-maps-geocode-location
                               (buffer-substring beg end))))))
    (delete-region beg end)
    (insert location)))

(provide 'google-maps-geocode)
