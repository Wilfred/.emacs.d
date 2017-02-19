;;; google-maps-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "google-maps" "google-maps.el" (22697 45748
;;;;;;  648102 316000))
;;; Generated autoloads from google-maps.el

(autoload 'google-maps "google-maps" "\
Run Google Maps on LOCATION.
If NO-GEOCODING is t, then does not try to geocode the address
and do not ask the user for a more precise location.

\(fn LOCATION &optional NO-GEOCODING)" t nil)

;;;***

;;;### (autoloads nil "google-maps-geocode" "google-maps-geocode.el"
;;;;;;  (22697 45748 681435 231000))
;;; Generated autoloads from google-maps-geocode.el

(autoload 'google-maps-geocode-replace-region "google-maps-geocode" "\
Geocode region and replace it with a more accurate result.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil "google-maps-static" "google-maps-static.el"
;;;;;;  (22697 45748 538103 699000))
;;; Generated autoloads from google-maps-static.el

(autoload 'google-maps-static-mode "google-maps-static" "\
A major mode for Google Maps service

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-location-google-maps" "org-location-google-maps.el"
;;;;;;  (22697 45748 724768 20000))
;;; Generated autoloads from org-location-google-maps.el

(autoload 'org-location-google-maps "org-location-google-maps" "\
Show Google Map for location of an Org entry in an org buffer.
If WITH-CURRENT-LOCATION prefix is set, add a marker with current
location.

\(fn &optional WITH-CURRENT-LOCATION)" t nil)

(autoload 'org-agenda-location-google-maps "org-location-google-maps" "\
Show Google Map for location of an Org entry in an org-agenda buffer.

\(fn &optional WITH-CURRENT-LOCATION)" t nil)

(autoload 'org-address-google-geocode-set "org-location-google-maps" "\
Set address property to LOCATION address for current entry using Google Geocoding API.

\(fn LOCATION)" t nil)

(autoload 'org-coordinates-google-geocode-set "org-location-google-maps" "\
Set coordinates property to LOCATION coordinates for current entry using Google Geocoding API.

\(fn LOCATION)" t nil)

(autoload 'org-google-maps-key-bindings "org-location-google-maps" "\


\(fn)" nil nil)

(autoload 'org-agenda-google-maps-key-bindings "org-location-google-maps" "\


\(fn)" nil nil)

(eval-after-load 'org '(org-google-maps-key-bindings))

(eval-after-load 'org-agenda '(org-agenda-google-maps-key-bindings))

;;;***

;;;### (autoloads nil nil ("google-maps-base.el" "google-maps-pkg.el")
;;;;;;  (22697 45748 614769 403000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; google-maps-autoloads.el ends here
