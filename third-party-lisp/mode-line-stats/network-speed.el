;;; network-speed.el --- display network speed information  -*- coding: mule-utf-8 -*-

;; Copyright (C) 2009 Vicente Hernando Ara

;; Author: Vicente Hernando Ara <bizenton@gmail.com>

;; Created: 1 Jan 2010

;; Keywords: hardware

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.



;;; Commentary:

;; network-speed.el package displays network speed in emacs mode line for the interfaces the user selects.


;; There are several configurable options:

;;   network-speed-update_interval: time interval after which speed is calculated.

;;   network-speed-interface-list: list containing all network interfaces we want to show data about.

;;   network-speed-precision: every floating point number will show this number of figures after decimal point.

;;   network-speed-format-string: customizable string to be shown in the mode line.
;;   There are several scape strings which will be sustituted by data.
;;     %NI == network interface
;;     %RX == received bytes speed 
;;     %TX == transmitted bytes speed
;;     %AX == received + transmitted bytes speed
;;     %RB == total received bytes
;;     %TB == total transmitted bytes
;;     %AB == total received plus transmitted bytes
;;     %%% == % character"

;; After this file is loaded, it is necessary to call `network-speed-start' function.
;; To stop network-speed working  you need to call `network-speed-stop' function.

;;      
;; A lot of ideas were taken from `battery.el.gz' file.


;;; NETWORK-SPEED CONFIGURATION
;;
;; Add something like this in your .emacs file:
;;
;; ;; Custom variables:
;;  '(network-speed-update-interval 2)   ; Shows network speed every 2 seconds.
;;  '(network-speed-interface-list (list "ppp0" "eth0"))  ; Network interfaces to be shown in mode line.
;;  '(network-speed-precision 1)  ; Number of figures after decimal point.
;;  '(network-speed-format-string " [%NI total: %AX] ") ; String format.
;;
;; ;; network-speed configuration:
;; (add-to-list 'load-path "/path/to/network-speed.el")
;; (require 'network-speed)
;; (network-speed-start) ; 
;;
;; ;; to stop network-speed, call "network-speed-stop" interactive function.
;; ;; (network-speed-stop)


;;; Code:

(defgroup network-speed nil
  "Displays network interface speed."
  :group 'hardware)

;;; CUSTOM VARIABLES:

;; Network interface list to be shown in mode line.  
;;   E.g: `(list "ppp0" "eth0" "ppp1")'
(defcustom network-speed-interface-list (list "eth0") "Network interface list to be shown in mode line." 
  :type '(repeat string)
  :group 'network-speed )

(defcustom network-speed-update-interval 1 "Network speed is calculated every network-speed-update-interval seconds."
  :type 'number 
  :group 'network-speed )

(defcustom network-speed-precision 2 "Number of digits after point in numbers shown in mode line."
  :type 'integer 
  :group 'network-speed )

;; String used to build the output string shown in emacs mode line.
(defcustom network-speed-format-string " [%NI rx:%RX tx:%TX] " "Format string. 
%NI == network interface
%RX == received bytes speed 
%TX == transmitted bytes speed
%AX == received + transmitted bytes speed
%RB == total received bytes
%TB == total transmitted bytes
%AB == total received plus transmitted bytes
%%% == % character"
  :type 'string
  :group 'network-speed 
)


(defvar network-speed-update-timer nil)

;; List that contains vectors like this: [ network-interface-string received-bytes transmitted-bytes rx-speed tx-speed ]
;; This variable is only for internal use.
(defvar network-speed-data-list nil) 

;; We will parse /proc/net/dev file using the returned regexp.
(defun network-speed-get-regex (network-interface)
  "Returns the right regular expression needed to find received and transmitted bytes for NETWORK-INTERFACE."
  (concat "^\\( *" network-interface ":[ ]*\\)"
	  "\\([0-9]+\\)" ; received bytes.
	  "\\( +[0-9]+\\)\\{7\\} +"
	  "\\([0-9]+\\)"))  ; transmitted bytes.


(defun network-speed-get-rx-tx-bytes ()
  "Gets received and transmitted bytes for every required network interface and calculates received and transmitted speeds.
Received and transmitted bytes are read from `/proc/net/dev' file"
  (mapc
   (lambda (network-speed-data)
     (with-temp-buffer
       (ignore-errors (insert-file-contents "/proc/net/dev")) 
       (cond ((re-search-forward (network-speed-get-regex (aref network-speed-data 0)) (point-max) t)
	      (let ((old-received-bytes (aref network-speed-data 1)) ; get old received bytes.
		    (old-transmitted-bytes (aref network-speed-data 2)) ; get old transmitted bytes.
		    (received-bytes (string-to-number (match-string 2))) ; get updated received bytes.
		    (transmitted-bytes (string-to-number (match-string 4)))) ; get updated transmitted bytes.
		
		;; Save received and transmitted bytes.
		(aset network-speed-data 1 received-bytes) 
		(aset network-speed-data 2 transmitted-bytes)
		
		;; Save received and transmitted speed.
		(aset network-speed-data 3 (/ (float (- received-bytes old-received-bytes)) network-speed-update-interval))
		(aset network-speed-data 4 (/ (float (- transmitted-bytes old-transmitted-bytes)) network-speed-update-interval))))
	     (t 
	      (aset network-speed-data 3 -1 ) ; There is no network interface available.
	      (aset network-speed-data 4 -1 ) ; There is no network interface available.
	      ))))
   
   network-speed-data-list))


(defun network-speed-add-units (speed &optional add-seconds)
  "Returns speed in adequate units e.g: B/s, KB/s, MB/s, B, KB or MB."
  (let ((string-end ""))
    (when add-seconds
      (setq string-end "/s"))
    (cond 
     ((< speed 0) (format "*")) ; Network interface not available right now.
     ((< speed 1000) (format "%d B%s" speed string-end))  
     ((< speed 1000000) (format (concat "%." (number-to-string network-speed-precision) "f KB%s") (/ speed 1000) string-end))
     (t (format (concat "%." (number-to-string network-speed-precision) "f MB%s") (/ speed 1000000) string-end)))))


(defun network-speed-parse-format (vector-data)
  "Substitute %XX sequences in network-speed-format-string."
  (replace-regexp-in-string 
;;;   "%NI\\|%RX\\|%TX\\|%AX\\|%RB\\|%TB\\|%%%"
   "%\\(NI\\|RX\\|TX\\|AX\\|RB\\|TB\\|AB\\|%%\\)"
   (lambda (str)
     (cond 
      ((equal str "%NI") (aref vector-data 0))  ; network interface.
      ((equal str "%RX") (network-speed-add-units (aref vector-data 3) t))  ; received bytes speed.
      ((equal str "%TX") (network-speed-add-units (aref vector-data 4) t))  ; transmitted bytes speed.
      ((equal str "%AX") (network-speed-add-units (+ (aref vector-data 3) (aref vector-data 4)) t)) ; received + transmitted speed.
      ((equal str "%RB") (network-speed-add-units (aref vector-data 1))) ; received bytes.
      ((equal str "%TB") (network-speed-add-units (aref vector-data 2))) ; transmitted bytes.
      ((equal str "%AB") (network-speed-add-units (+ (aref vector-data 1) (aref vector-data 2)))) ; received + transmitted bytes.
      ((equal str "%%%") "%") ; % character.
      )
     )
   network-speed-format-string t t)
)


(defun network-speed-update-handler()
  "This function is called every `network-speed-update-interval' seconds to calculate data and show network speed in modeline."
  (network-speed-get-rx-tx-bytes)
  
  (setq network-speed-mode-line-string "")
  (mapc 
   (lambda (network-speed-data)
     (setq network-speed-mode-line-string 
	   (concat network-speed-mode-line-string 
		   (network-speed-parse-format network-speed-data))))  ; Obtains the string we will show.
   network-speed-data-list)

  (force-mode-line-update)
  (sit-for 0))


(defun network-speed-start ()
  "Shows network speed in mode line. 
This function initializes all needed data. 
Speed is not shown inmediately but after `network-speed-update-interval' seconds when first correct value is calculated."
  (interactive)

  ;; Adding network-speed-mode-line-string to global mode line.
  (setq network-speed-mode-line-string "")
  (add-to-list 'global-mode-string 'network-speed-mode-line-string t)

  ;; Initialize all needed vectors in `network-speed-data-list'.
  (setq network-speed-data-list ())
  (mapc 
   (lambda (x)
     (add-to-list 'network-speed-data-list (vector x 0 0 0 0) t))  ; Initialices and element of network-speed-data list.
   network-speed-interface-list)

  ;; Calculate speeds.
  (network-speed-get-rx-tx-bytes)

  ;; Create network-speed-update-timer. 
  (and network-speed-update-timer (cancel-timer network-speed-update-timer))
  ;; This does not inmediately show speeds but it waits `network-speed-update-interval' seconds to start showing it.
  (setq network-speed-update-timer (run-at-time network-speed-update-interval network-speed-update-interval 'network-speed-update-handler)))


(defun network-speed-stop ()
  "Stops displaying network speed in mode line."
  (interactive)
  ;; Delete the text from global mode line.
  (setq global-mode-string (delq 'network-speed-mode-line-string global-mode-string))

  ;; Cancel timer.
  (and network-speed-update-timer (cancel-timer network-speed-update-timer)))


(provide 'network-speed)

;;; filename ends here
