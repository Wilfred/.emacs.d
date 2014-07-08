;;; mls-sensors.el --- display various sensors stats in the mode-line  -*- coding: mule-utf-8 -*-

;; This file is not part of Emacs

;; Copyright (C) 2013 Andreu Gil Pàmies

;; Author: Andreu Gil Pàmies <agpchil@gmail.com>

;; Created: 10-06-2013

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

;; (require 'mls-sensors)
;; (setq mls-sensors-patterns
;;   '(("c0" "Core 0")
;;     ("c1" "Core 1")))
;; (mls-sensors-start)

;;; Commentary:

;;; Code:
(require 'mls-common)

(defvar mls-sensors-formatters nil)
(defvar mls-sensors-timer nil)
(defvar mls-sensors-mode-line-string "")
(defvar mls-sensors-command "sensors")
(defvar mls-sensors-patterns
  '(("c0" "Core 0")
    ("c1" "Core 1"))
  "Alist of patterns.
First value is the formatter.
Second value is the `sensors` label.")

(defvar mls-sensors-settings
  '((:formats
     ((:primary "&c0{t}")
      (:secondary " SENSORS[%c0{ºC}]")
      (:monitor "&c0")))
    (:levels
     (("%c0" ((60.0 "crit")
              (40.0 "warn")
              (0.0  "norm"))))))
  "SENSORS stats settings.")

(defgroup mls-sensors nil
  "Display various sensors stats in the mode-line."
  :group 'mls-sensors)

(defcustom mls-sensors-update-interval 15
  "Number of seconds between sensors stats recalculation."
  :type 'number
  :group 'mls-sensors)

(defvar mls-sensors-format nil)

(defun mls-sensors-update ()
  "Update stats."
  (setq mls-sensors-mode-line-string (mls-sensors-stats))
  (force-mode-line-update)
  (sit-for 0))

(defun mls-sensors-start ()
  "Start displaying sensors usage stats in the mode-line."
  (interactive)
  (mls-sensors-formatters-init)

  (setq mls-sensors-mode-line-string "")
  (mls-set-timer 'mls-sensors-timer
                 mls-sensors-update-interval
                 'mls-sensors-update))

(defun mls-sensors-stop ()
  "Stop displaying sensors usage stats in the mode-line."
  (interactive)
  (setq mls-sensors-mode-line-string "")
  (mls-cancel-timer 'mls-sensors-timer))

(defun mls-sensors-stats ()
  "Build the stats."
  (let ((stats (mls-sensors-fetch)))
    (mls-format-expand mls-sensors-formatters mls-sensors-format stats)))

(defun mls-sensors-fetch ()
  "Return a bunch of sensors stats in a form of an alist."
  (let ((stats (mapcar #'(lambda (s) (split-string s ":"))
                 (delete "" (split-string
                   (shell-command-to-string mls-sensors-command)
                   "\n")))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(defun mls-sensors-formatters-init ()
  "Initialize the formatters."
  (setq mls-sensors-format (mapconcat #'(lambda (pattern)
                                            (concat "%" (car pattern)))
                                        mls-sensors-patterns
                                        " "))
  (setq mls-sensors-formatters
        (mapcar #'(lambda (pattern-list)
                    `(,(car pattern-list)
                           lambda (stats)
                           (number-to-string (cadr (assoc ,(cadr pattern-list) stats)))))
                mls-sensors-patterns)))

(provide 'mls-sensors)
;;; mls-sensors ends here
