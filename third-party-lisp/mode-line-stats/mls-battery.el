;;; mls-battery.el --- display various battery stats in the mode-line  -*- coding: mule-utf-8 -*-

;; This file is not part of Emacs

;; Copyright (C) 2013 Andreu Gil Pàmies
;; Copyright (C) 2012 Kajetan Rzepecki

;; Author: Andreu Gil Pàmies <agpchil@gmail.com>

;; Created: 05-04-2013

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


;;; Usage:

;; (require 'mls-battery)
;; (setq mls-battery-format "%p")
;; (mls-battery-start)

;;; Commentary:

;;; Code:

(require 'cl)
(require 'mls-common)

(defvar mls-battery-formatters nil)
(defvar mls-battery-timer nil)
(defvar mls-battery-mode-line-string "")

(defvar mls-battery-battery-format "%c %r %B %d %L %p %m %h %t")

(defvar mls-battery-settings
  '((:formats
     ((:primary "&p{b}")
      (:secondary " BAT[%p{%%}]")
      (:monitor "&p")))
    (:levels
     (("%p" ((75.0 "norm")
             (35.0 "warn")
             (0.0  "crit"))))))
  "BATTERY stats settings.")

(defgroup mls-battery nil
  "Display various disk stats in the mode-line."
  :group 'mls-battery)

(defcustom mls-battery-update-interval 15
  "Number of seconds between disk stats recalculation."
  :type 'number
  :group 'mls-battery)


(defcustom mls-battery-format "%p %t %r"
  "Format string:
%c Current capacity (mAh or mWh)
%r Current rate of charge or discharge
%B Battery status (verbose)
%b Battery status: empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%L AC line status (verbose)
%p Battery load percentage
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  :type 'string
  :group 'mls-battery)

(defun mls-battery-update ()
  "Update stats."
  (setq mls-battery-mode-line-string (mls-battery-stats))
  (force-mode-line-update)
  (sit-for 0))

(defun mls-battery-start ()
  "Start displaying disk usage stats in the mode-line."
  (interactive)

  (setq battery-mode-line-format mls-battery-battery-format)
  (display-battery-mode)
  (setq global-mode-string (delq 'battery-mode-line-string
                                 global-mode-string))

  (setq mls-battery-mode-line-string "")
  (mls-set-timer 'mls-battery-timer
                 mls-battery-update-interval
                 'mls-battery-update))

(defun mls-battery-stop ()
  "Stop displaying disk usage stats in the mode-line."
  (interactive)
  (setq mls-battery-mode-line-string "")
  (mls-cancel-timer 'mls-battery-timer)
  (display-battery-mode -1))

(defun mls-battery-stats ()
  "Build stats."
  (let ((stats (mls-battery-fetch)))
    (mls-format-expand mls-battery-formatters mls-battery-format stats)))

(defun mls-battery-fetch ()
  "Return a bunch of disk stats in a form of an alist."
  (let ((stats (mapcar #'split-string
                       (split-string (substring-no-properties battery-mode-line-string) " "))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(setq mls-battery-formatters
  (list
    (cons "c" (lambda (stats)
                (car (nth 0 stats))))
    (cons "r" (lambda (stats)
                (car (nth 1 stats))))
    (cons "B" (lambda (stats)
                (car (nth 3 stats))))
    (cons "d" (lambda (stats)
                (car (nth 4 stats))))
    (cons "L" (lambda (stats)
                (car (nth 5 stats))))
    (cons "p" (lambda (stats)
                (car (nth 6 stats))))
    (cons "m" (lambda (stats)
                (car (nth 7 stats))))
    (cons "h" (lambda (stats)
                (car (nth 8 stats))))
    (cons "t" (lambda (stats)
                (car (nth 9 stats))))))

(provide 'mls-battery)
;;; mls-battery.el ends here
