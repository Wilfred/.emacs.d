;;; google-maps-static.el --- Access Google Maps Static from Emacs

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

;; All arguments are optional. Here is a full call example:
;;
;; (google-maps-static-show
;;  :center "Cimetière du Montparnasse"
;;  :maptype 'hybrid
;;  ;; :zoom 5
;;  :markers '((("Place Saint-Michel, Paris") . (:label ?M :color "blue"))
;;             (("Jardin du Luxembourg, Paris" "Parc Montsouris, Paris")
;;              . (:label ?P :color "green")))
;;  :visible '("44 rue de l'Ouest, Paris" "Montrouge")
;;  :paths '((("Tour Eiffel, Paris" "Arc de triomphe, Paris" "Panthéon, Paris")
;;            . (:weight 3 :color "black" :fillcolor "yellow"))))
;;
;; All addresses can be specified as a string or in the following format:
;;
;; (LOCATION_NAME ((lat . LATITUDE) (lng. LONGITUDE)))
;;

;;; TODO:

;; - Resize map if frame is resized
;; - Add interactive code to build path

;;; Code:

(require 'cl-lib)
(require 'google-maps-geocode)
(require 'url-util)

(defgroup google-maps-static nil
  "Google Maps Static."
  :group 'google-maps)

(defcustom google-maps-static-buffer-name "*Google Maps*"
  "Name of the Google Maps buffer."
  :group 'google-maps-static
  :type 'string)

(defcustom google-maps-static-default-zoom 15
  "Default zoom level when calling `google-maps-static-zoom' with no argument."
  :group 'google-maps-static
  :type 'integer)

(defcustom google-maps-static-move-step 0.00008
  "Value used to define a step value when moving."
  :group 'google-maps-static
  :type 'float)

(defcustom google-maps-static-home-marker ?H
  "Character used to mark home.
Used by `google-maps-static-add-home-marker'."
  :group 'google-maps-static
  :type 'character)

(defconst google-maps-static-uri
  "http://maps.google.com/maps/api/staticmap"
  "Google Maps API server.")

(defconst google-maps-static-minimum-zoom 0
  "Minimum zoom level.")

(defconst google-maps-static-maximum-zoom 21
  "Minimum zoom level.")

(defconst google-maps-static-maptypes '("roadmap" "satellite" "hybrid" "terrain")
  "Available map types.")

(defvar google-maps-static-mode-hook nil
  "Hook run by `google-maps-static-mode'.")

(defvar google-maps-static-params nil
  "Current parameters of the map.")
(make-variable-buffer-local 'google-maps-static-params)

(defun google-maps-static-urlencode-location (location)
  "Decode a location.
Location format can be either a string, which is returned as it
is, or a list in format:

  (LOCATION ((lat . LATVALUE) (lng . LNGVALUE)))

In such case, latvalue,lngvalue is returned."
  (when location
    (if (listp location)
        (format "%f,%f"
                (cdr (assoc 'lat (cadr location))) (cdr (assoc 'lng (cadr location))))
      (url-hexify-string location))))

(defun google-maps-static-marker-to-url-parameters (marker)
  (let ((prop (google-maps-urlencode-plist
               (cdr marker)
               '((size)
                 (color)
                 (label . (lambda (label)
                            (when label (char-to-string label)))))
               ":"
               "|")))
    (concat
     (when prop (concat prop "|"))
     (mapconcat
      'google-maps-static-urlencode-location
      (car marker)
      "|"))))

(defun google-maps-static-markers-to-url-parameters (markers)
  "From MARKERS, build parameters for a Google Static Maps URL.
MARKERS should have the form
'(((\"loc1\" \"loc2\") . (:size tiny :color \"blue\" :label ?X)))"
  (mapconcat 'google-maps-static-marker-to-url-parameters
             markers
             "&markers="))

(defun google-maps-static-visible-to-url-parameters (visible)
  "From VISIBLE, build parameters for a Google Static Maps URL.
VISIBLE should have the form '(\"loc1\" \"loc2\" ... \"locN\")."
  (mapconcat 'google-maps-static-urlencode-location
             visible
             "|"))

(defun google-maps-static-path-to-url-parameters (path)
  (let ((prop (google-maps-urlencode-plist
               (cdr path)
               '((weight . (lambda (weight)
                             (when weight
                               (number-to-string weight))))
                 (color)
                 (fillcolor))
               ":"
               "|")))
    (concat
     (when prop (concat prop "|"))
     (mapconcat
      'google-maps-static-urlencode-location
      (car path)
      "|"))))

(defun google-maps-static-paths-to-url-parameters (paths)
  "From PATH, build parameters for a Google Static Maps URL.
PATHS should have the form
'(((\"loc1\" \"loc2\") . (:weight 5 :color \"red\" :fillcolor \"black\")))"
  (mapconcat 'google-maps-static-path-to-url-parameters
             paths
             "&path="))

(defun google-maps-static-set-size (plist)
  "adapt size to current window settings"
  (let ((edges (window-inside-pixel-edges)))
    (plist-put
     (plist-put plist :height (- (nth 3 edges) (nth 1 edges)))
     :width (- (nth 2 edges) (nth 0 edges) ))))

(defun google-maps-static-refresh (&optional force)
  "Redisplay the map."
  (interactive "P")
  (let ((google-maps-cache-ttl (if force
                                   0
                                 google-maps-cache-ttl)))
  (apply 'google-maps-static-show google-maps-static-params)))

(defun google-maps-static-build-plist (plist)
  "Build a property list based on PLIST."
  ;; Make all markers upper case
  (let ((markers (plist-get plist :markers))
        (set-size (if (or (not (plist-get plist :width)) (not (plist-get plist :height)))
                      'google-maps-static-set-size
                    'identity)))
    (funcall set-size
             (google-maps-build-plist
              (if markers
                  (plist-put plist :markers
                             (mapcar
                              (lambda (marker)
                                (let ((props (cdr marker)))
                                  (when props
                                    (let ((label (plist-get props :label)))
                                      (when label
                                        (plist-put props :label (upcase label))))))
                                marker)
                              markers))
                plist)))))

(defun google-maps-static-build-url (plist)
  "Build a URL to request a static Google Map."
  (concat
   google-maps-static-uri "?"
   (google-maps-urlencode-plist
    plist
    `((format)
      (center . google-maps-static-urlencode-location)
      (size . ,(format "%dx%d"
                       (plist-get plist :width)
                       (plist-get plist :height)))
      (maptype)
      (mobile . ,(lambda (v)
                  (if v "true" nil)))
      ;; Sensor is MANDATORY
      (sensor . ,(lambda (v)
                  (if v "true" "false")))
      (zoom . ,(lambda (zoom)
                 (when zoom (number-to-string zoom))))
      (format)
      (language)
      (markers . ,(google-maps-static-markers-to-url-parameters (plist-get plist :markers)))
      (visible . ,(google-maps-static-visible-to-url-parameters (plist-get plist :visible)))))
   (let ((paths (plist-get plist :paths)))
     (if paths
         (concat "&path=" (google-maps-static-paths-to-url-parameters paths))
     ""))))

(defun google-maps-static-build-location-string (location)
  "Build a string to display from a LOCATION."
  (if (listp location)
      (format "%s (%f, %f)"
              (car location)
              (cdr (assoc 'lat (cadr location)))
              (cdr (assoc 'lng (cadr location))))
    location))

(defun google-maps-static-build-info-string (plist)
  "Build a informative string describin PLIST."
  (let ((center (plist-get plist :center))
        (visible (plist-get plist :visible))
        (markers (plist-get plist :markers))
        (paths (plist-get plist :paths))
        (zoom (plist-get plist :zoom)))
    (concat
     (when zoom
       (format "Zoom level: %d\n" zoom))
     (when center
       (format "Center: %s\n" (google-maps-static-build-location-string center)))
     (when visible
       (format "Visible: %s\n" (mapconcat 'google-maps-static-build-location-string visible ", ")))
     (when markers
       (format "Markers:\n%s\n"
               (mapconcat (lambda (marker)
                            (let* ((prop (cdr marker))
                                   (label (plist-get prop :label))
                                   (size (plist-get prop :size))
                                   ;; If size is small or tiny, label is not visible
                                   (label (cond ((or (eq size 'small)
                                                     (eq size 'tiny))
                                                 ?○)
                                                ((null label) ?●)
                                                ((and
                                                  (>= label ?A)
                                                  (<= label ?Z))
                                                 (+ label 9333))
                                                ((and
                                                  (>= label ?1)
                                                  (<= label ?9))
                                                 (+ label 9263))
                                                (t ?●))))
                              (concat " " (char-to-string label) ":\t"
                                      (mapconcat 'google-maps-static-build-location-string
                                                 (car marker) "\n\t"))))
                          markers
                          "\n")))
     (when paths
       (format "Paths:\n%s\n"
               (mapconcat (lambda (path)
                            (concat "\t"
                                    (mapconcat 'google-maps-static-build-location-string
                                               (car path) " → ")))
                          paths
                          "\n"))))))

(defun google-maps-static-insert-image-at-point (start image format help-echo)
  "Insert an IMAGE with FORMAT at point START."
  (goto-char start)
  (insert "Map")
  (add-text-properties
   start (point)
   `(display
     ,(create-image image format t)
     help-echo ,help-echo
     read-only t
     rear-nonsticky (display read-only))))

(defun google-maps-static-show (&rest plist)
  "Open a new buffer with a Google Map.

PLIST can contains this properties:

 :center   Where to center the map.
           This is either a string, or a list with format:
           (LOCATION_NAME ((lat . LATVALUE) (lng . LNGVALUE)))
 :zoom     Zoom level on the map.
 :sensor   Specifies whether the application requesting the static
           map is using a sensor to determine the user's
           location.
 :mobile   Specifies whether the map will be displayed on a mobile
           device.
 :format   Defines the format of the resulting image.
 :width    Image width.
 :height   Image height.
 :language Defines the language to use for display of labels on
           map tiles.
 :visible  A list of location that should stay visible on the map.
 :markers  An alist of markers.
           Format for a marker is
           ((\"Location 1\" \"Location 2\" ... \"Location N\") . options)
           OPTIONS is not mandatory. If set, it should be a list
           with any number of options as above:
           (:size 2 :color \"blue\" :label ?J).
 :paths    An a list of path to draw.
           Format for a path is
           ((\"Location 1\" \"Location 2\" ... \"Location N \") . options)
           OPTIONS is not mandatory. If set, it should be a list
           with any number of options as above:
           (:fillcolor \"blue\" :weight 5 :color \"yellow\").
 :cache    Cache TTL, default to `google-maps-cache-ttl'.

This function returns the buffer where the map is displayed."
  (let ((buffer (get-buffer-create google-maps-static-buffer-name)))
    (unless (eq (current-buffer) buffer)
      (switch-to-buffer-other-window buffer))
    (google-maps-static-mode)
    (let* ((inhibit-read-only t)
           (plist (google-maps-static-build-plist plist))
           (url (google-maps-static-build-url plist)))
      (setq google-maps-static-params plist)
      (delete-region (point-min) (point-max))
      (google-maps-static-insert-image-at-point
       (point-min)
       (google-maps-retrieve-data url (or (plist-get plist :cache) google-maps-cache-ttl))
       (plist-get plist :format)
       (google-maps-static-build-info-string plist)))
    buffer))

(defvar google-maps-static-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") 'google-maps-static-zoom-in)
    (define-key map (kbd ">") 'google-maps-static-zoom-in)
    (define-key map (kbd ".") 'google-maps-static-zoom-in)
    (define-key map (kbd "-") 'google-maps-static-zoom-out)
    (define-key map (kbd "<") 'google-maps-static-zoom-out)
    (define-key map (kbd ",") 'google-maps-static-zoom-out)
    (define-key map (kbd "z") 'google-maps-static-zoom)
    (define-key map (kbd "Z") 'google-maps-static-zoom-remove)
    (define-key map (kbd "q") 'google-maps-static-quit)
    (define-key map (kbd "w") 'google-maps-static-copy-url)
    (define-key map (kbd "m") 'google-maps-static-manage-marker)
    (define-key map (kbd "v") 'google-maps-static-manage-visible)
    (define-key map (kbd "c") 'google-maps-static-center)
    (define-key map (kbd "C") 'google-maps-static-center-remove)
    (define-key map (kbd "t") 'google-maps-static-set-maptype)
    (define-key map (kbd "g") 'google-maps-static-refresh)
    (define-key map (kbd "h") 'google-maps-static-add-home-marker)
    (define-key map (kbd "<up>") 'google-maps-static-move-north)
    (define-key map (kbd "<down>") 'google-maps-static-move-south)
    (define-key map (kbd "<left>") 'google-maps-static-move-west)
    (define-key map (kbd "<right>") 'google-maps-static-move-east)
    (define-key map [mouse-4] 'google-maps-static-zoom-mouse-in)
    (define-key map [mouse-5] 'google-maps-static-zoom-mouse-out)
    map)
  "Keymap for `google-maps-static-mode'.")

(require 'easymenu)
(defun google-maps-check-maptype (mtype &optional default)
  "Helper function. Checks whether :maptype is MTYPE."
  (let ((maptype (plist-get google-maps-static-params :maptype)))
    (if maptype
        (string= maptype mtype)
        default)))

(easy-menu-define google-maps-static-mode-menu google-maps-static-mode-map
  "Google Maps Menu"
  '("Google Maps"
    ["Zoom In" google-maps-static-zoom-in t]
    ["Zoom Out" google-maps-static-zoom-out t]
    ["Zoom" google-maps-static-zoom t]
    "--"
    ["Manage Marker" google-maps-static-manage-marker t]
    ["Manage Visible" google-maps-static-manage-visible t]
    ["Add Home Marker" google-maps-static-add-home-marker t]
    ["Center" google-maps-static-center t]
    ["Center Remove" google-maps-static-center-remove t]
    "--"
    ["Move North" google-maps-static-move-north t]
    ["Move South" google-maps-static-move-south t]
    ["Move East" google-maps-static-move-east t]
    ["Move West" google-maps-static-move-west t]
    "--"
    ("Map Type"
     ["Roadmap" (google-maps-static-set-maptype "roadmap") :style radio :selected (google-maps-check-maptype "roadmap" t)]
     ["Satellite" (google-maps-static-set-maptype "satellite") :style radio :selected (google-maps-check-maptype "satellite")]
     ["Hybrid" (google-maps-static-set-maptype "hybrid") :style radio :selected (google-maps-check-maptype "hybrid")]
     ["Terrain" (google-maps-static-set-maptype "terrain") :style radio :selected (google-maps-check-maptype "terrain")])
    ["Copy URL" google-maps-static-copy-url t]
    ["Refresh" google-maps-static-refresh t]
    ["Quit" google-maps-static-quit t]))

;;;###autoload
(define-derived-mode google-maps-static-mode fundamental-mode "Google Maps"
  "A major mode for Google Maps service"
  :group 'comm
  (setq cursor-type nil)
  (setq buffer-read-only t))

(defun google-maps-static-zoom (level)
  "Zoom a Google map."
  (interactive (list (or (when current-prefix-arg
                           (prefix-numeric-value current-prefix-arg))
                         (read-number "Zoom level: " google-maps-static-default-zoom))))
  (let ((plist google-maps-static-params))
    (apply 'google-maps-static-show
           (if level
               (plist-put plist
                          :zoom
                          (max (min level google-maps-static-maximum-zoom)
                               google-maps-static-minimum-zoom))
             (plist-put plist :zoom nil)))))

(defun google-maps-static-zoom-remove ()
  "Remove zoom level from a Google map."
  (interactive)
  (let ((plist google-maps-static-params))
    (apply 'google-maps-static-show
           (google-maps-plist-delete plist :zoom))))

(defun google-maps-static-zoom-in ()
  "Zoom a Google map in."
  (interactive)
  (unless (plist-get google-maps-static-params :zoom)
    (error "Current zoom level is unknown, cannot zoom in."))
  (google-maps-static-zoom (1+ (plist-get google-maps-static-params :zoom))))

(defun google-maps-static-zoom-out ()
  "Zoom a Google map out."
  (interactive)
  (unless (plist-get google-maps-static-params :zoom)
    (error "Current zoom level is unknown, cannot zoom out."))
  (google-maps-static-zoom (1- (plist-get google-maps-static-params :zoom))))

(defun google-maps-static-quit ()
  "Kill Google maps buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun google-maps-static-copy-url ()
  "Kill Google maps buffer."
  (interactive)
  (kill-new (google-maps-static-build-url google-maps-static-params)))

(defun google-maps-static-geocode (location)
  "Geocode a location.
If location is already a list, it's geocode. Otherwise, geocode the string and returns

 (FORMATTED_ADDRESS ((lat . LATITUDE) (lng . LONGITUDE)))"
  (if (listp location)
      location
    (let ((data (google-maps-geocode-location location)))
      (list (cdr (assoc 'formatted_address data))
            (cdr (assoc 'location (assoc 'geometry data)))))))

(defun google-maps-static-add-visible (location)
  "Make LOCATION visible on the map."
  (interactive
   (list
    (read-string "Location to set visible: ")))
  (let* ((plist google-maps-static-params)
         (visible (plist-get plist :visible)))
    (apply 'google-maps-static-show
           (plist-put plist :visible (add-to-list 'visible
                                                  (google-maps-static-geocode location))))))

(defun google-maps-static-remove-visible (location)
  "Remove a visible LOCATION on the map."
  (interactive
   (let* ((plist google-maps-static-params)
          (visible (plist-get plist :visible)))
     (list
      (completing-read "Location to unset visible: " visible nil t))))
  (let* ((plist google-maps-static-params)
         (visible (plist-get plist :visible)))
    (apply 'google-maps-static-show
           (plist-put plist :visible
                      (cl-remove-if `(lambda (l) (string= l ,location)) visible)))))

(defun google-maps-static-manage-visible (remove)
  "Add or remove a visible location. If REMOVE is set, remove it."
  (interactive "P")
  (if remove
      (call-interactively 'google-maps-static-remove-visible)
    (call-interactively 'google-maps-static-add-visible)))

(defun google-maps-static-add-marker (location label &optional size color)
  "Add a marker on LOCATION on the map with LABEL. You can
specify SIZE and COLOR of the LABEL."
  (interactive
   (list
    (read-string "Location to mark: ")
    (read-char "Type a character to use as mark for location.")))
  (let ((plist google-maps-static-params))
    (apply 'google-maps-static-show
           (plist-put plist :markers
                      (append (plist-get plist :markers)
                              `(((,(google-maps-static-geocode location))
                                 . (:label ,label :size ,size :color ,color))))))))

(defun google-maps-static-remove-marker (label)
  "Remove a marker from the map."
  (interactive
   (list
    (read-char "Type the mark character to remove from the map.")))
  (let ((plist google-maps-static-params)
        (label (upcase label)))
    (apply 'google-maps-static-show (plist-put plist :markers
                                               (cl-remove-if
                                                (lambda (marker)
                                                  (eq (plist-get (cdr marker) :label) label))
                                                (plist-get plist :markers))))))

(defun google-maps-static-manage-marker (times)
  "Remove or add markers on the map.
If TIMES is positive, add this number of marker.
If TIMES is negative, then remove this number of markers."
  (interactive "p")
  (if (> times 0)
      (dotimes (x times)
        (call-interactively 'google-maps-static-add-marker))
    (dotimes (x (abs times))
      (call-interactively 'google-maps-static-remove-marker))))

(defun google-maps-static-add-home-marker ()
  "Add a marker for home.
It uses `calendar-location-name', `calendar-latitude' and
`calendar-longitude' to determine where home is."
  (interactive)
  (require 'solar)
  (if (and calendar-latitude
           calendar-longitude
           calendar-location-name)
      (google-maps-static-add-marker `(,(eval calendar-location-name)
                                       ((lat . ,(calendar-latitude))
                                        (lng . ,(calendar-longitude))))
                                     google-maps-static-home-marker)
    (error "Unable to determine home.")))

(defun google-maps-static-center (location)
  "Center the map on a LOCATION. If LOCATION is nil or an empty
string, it will remove centering."
  (interactive
   (list
    (read-string "Location to center the map on: ")))
  (let ((plist google-maps-static-params))
    (apply 'google-maps-static-show
           (plist-put plist :center (google-maps-static-geocode location)))))

(defun google-maps-static-center-remove ()
  "Do not center the map."
  (interactive)
  (let ((plist google-maps-static-params))
    (apply 'google-maps-static-show
           (plist-put plist :center nil))))

(defun google-maps-static-event-to-buffer (event)
  (window-buffer (posn-window (event-start event))))

(defun google-maps-static-zoom-mouse-in (event)
  "Zoom with the mouse."
  (interactive (list last-input-event))
    (with-current-buffer (google-maps-static-event-to-buffer event)
      (funcall 'google-maps-static-zoom-in)))

(defun google-maps-static-zoom-mouse-out (event)
  "Zoom with the mouse."
  (interactive (list last-input-event))
    (with-current-buffer (google-maps-static-event-to-buffer event)
      (funcall 'google-maps-static-zoom-out)))

(defun google-maps-static-save (filename)
  "Save the map into FILENAME."
  (interactive (list (read-file-name "Save the map to: ")))
  (let ((data (plist-get (cdr (get-text-property (point-min) 'display)) :data)))
    (with-temp-buffer
      (insert data)
      (write-file filename))))

(defun google-maps-static-set-maptype (maptype)
  "Set map type to MAPTYPE."
  (interactive
   (list
    (intern
     (completing-read "Map type: " google-maps-static-maptypes nil t))))
  (let ((plist google-maps-static-params))
    (apply 'google-maps-static-show
           (plist-put plist :maptype maptype))))

(defmacro google-maps-static-defun-move (direction lat-or-lng operation)
  `(defun ,(intern (concat "google-maps-static-move-" direction)) ()
     ,(concat "Move map towards " direction ".")
     (interactive)
     (let* ((plist google-maps-static-params)
            (center (plist-get plist :center))
            (zoom (plist-get plist :zoom)))
       (unless center
         (error
          (substitute-command-keys
           "The map is not centered. Press \\[google-maps-static-center] to center.")))
       (unless (listp center)
         (error "The center location has no coordinates."))
       (unless zoom
         (error
          (substitute-command-keys
           "The map has no zoom level. Press \\[google-maps-static-zoom] to set a zoom level.")))
       (let* ((coordinates (cl-copy-list (cadr center)))
              (value (assoc ,lat-or-lng coordinates))
              (coordinates (delq value coordinates)))
         ;; Zoom ratio seems to be 2, so `2^zoom * value' move the map quite
         ;; correctly.
         (apply
          'google-maps-static-show
          (plist-put
           plist :center
           (list "" (append
                     coordinates
                     (list
                      (cons ,lat-or-lng
                            (,operation
                             (cdr value)
                             (* google-maps-static-move-step
                                (expt 2
                                      (- google-maps-static-maximum-zoom zoom))))))))))))))

(google-maps-static-defun-move "north" 'lat +)
(google-maps-static-defun-move "south" 'lat -)
(google-maps-static-defun-move "west" 'lng -)
(google-maps-static-defun-move "east" 'lng +)

(provide 'google-maps-static)

;;; google-maps-static.el ends here
