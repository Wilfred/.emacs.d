;;; mls-disk.el --- display various disk stats in the mode-line  -*- coding: mule-utf-8 -*-

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

;; By default, the first device found in df will be used as
;; mls-disk-device. If you want to use a different one you
;; can set it before calling mls-disk-start.

;; (require 'mls-disk)
;; (setq mls-disk-format "%p")
;; (setq mls-disk-device "/dev/sda3")
;; (mls-disk-start)

;;; Code:

(require 'cl)
(require 'mls-common)

(defvar mls-disk-formatters nil)
(defvar mls-disk-device nil)
(defvar mls-disk-timer nil)
(defvar mls-disk-mode-line-string "")

(defvar mls-disk-settings
  '((:formats
     ((:primary "&p{d}")
      (:secondary " DISK[%p{%%}]")
      (:monitor "&p")))
    (:levels
     (("%p" ((90.0 "crit")
             (50.0 "warn")
             (0.0  "norm"))))))
  "DISK stats settings.")

(defgroup mls-disk nil
  "Display various disk stats in the mode-line."
  :group 'mls-disk)

(defcustom mls-disk-update-interval 15
  "Number of seconds between disk stats recalculation."
  :type 'number
  :group 'mls-disk)

(defcustom mls-disk-format "%p"
  "Format string:
%p - Percentile used DISK space.
%u# - Used space (where # is the unit: M, G or T). Default in KB.
%f# - Free space (where # is the unit: M, G or T). Default in KB.
%t# - Total space (where # is the unit: M, G or T). Default in KB."
  :type 'string
  :group 'mls-disk)

(defun mls-disk-update ()
  "Update stats."
  (setq mls-disk-mode-line-string (mls-disk-stats))
  (force-mode-line-update)
  (sit-for 0))

(defun mls-disk-start ()
  "Start displaying disk usage stats in the mode-line."
  (interactive)
  (unless mls-disk-device
    (setq mls-disk-device (caar (mls-disk-fetch))))

  (setq mls-disk-mode-line-string "")
  (mls-set-timer 'mls-disk-timer
                 mls-disk-update-interval
                 'mls-disk-update))

(defun mls-disk-stop ()
  "Stop displaying disk usage stats in the mode-line."
  (interactive)
  (setq mls-disk-mode-line-string "")
  (mls-cancel-timer 'mls-disk-timer))

(defun mls-disk-stats ()
  "Build stats."
  (let ((stats (mls-disk-fetch)))
    (mls-format-expand mls-disk-formatters mls-disk-format stats)))

(defun mls-disk-fetch ()
  "Returns a bunch of disk stats in a form of an alist."
  (let ((stats (mapcar #'split-string
                 (remove-if (lambda (str) (string= str ""))
                   (split-string
                     (shell-command-to-string (concat "df | tail -n +2"))
                     "\n")))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(defun mls-disk-normalize-value (value unit)
  "Normalize VALUE using UNIT magnitude (M G or T)."
  (cond ((equal unit 'M) (/ value 1024.0))
        ((equal unit 'G) (/ value 1048576.0))
        ((equal unit 'T) (/ value 1073741824.0))
        (t value)))

(defun mls-disk-build-formatter (formatter index &optional unit)
  "Builds a `mls-disk` FORMATTER with VALUE."
  (cons (concat formatter (when unit (symbol-name unit)))
                      `(lambda (stats)
                        (number-to-string
                         (mls-disk-normalize-value
                          (nth ,index (assoc mls-disk-device stats))
                          ',unit)))))

(setq mls-disk-formatters
  (list
   (mls-disk-build-formatter "p" 4)

   (mls-disk-build-formatter "u" 2 'M)
   (mls-disk-build-formatter "u" 2 'G)
   (mls-disk-build-formatter "u" 2 'T)
   (mls-disk-build-formatter "u" 2)

   (mls-disk-build-formatter "f" 3 'M)
   (mls-disk-build-formatter "f" 3 'G)
   (mls-disk-build-formatter "f" 3 'T)
   (mls-disk-build-formatter "f" 3)

   (mls-disk-build-formatter "t" 1 'M)
   (mls-disk-build-formatter "t" 1 'G)
   (mls-disk-build-formatter "t" 1 'T)
   (mls-disk-build-formatter "t" 1)))

(provide 'mls-disk)
;;; mls-disk ends here
