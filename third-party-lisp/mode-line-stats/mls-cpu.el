;;; mls-cpu.el --- display various CPU stats in the mode-line  -*- coding: mule-utf-8 -*-

;; Copyright (C) 2012 Kajetan Rzepecki

;; Author: Kajetan Rzepecki

;; Created: 1 Sep 2012

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

;; (require 'mls-cpu)
;; (mls-cpu-start)


;; There are a few variables to tweak:
;;   `mls-cpu-update-interval' - Time interval after which current CPU stats are updated.
;;   `mls-cpu-format' - A string format used in the mode-line.
;;                      Supports the following escape sequences:
;;      %A - Average CPU usage across multiple cores in percent.
;;      %C# - CPU usage of core number # (starting from 0) in percent.
;;      %_u - User CPU usage of core _ (either A or C#).
;;      %_s - System CPU usage of core _.
;;      %_i - IO CPU usage of core _.

;;; TODO:

;; CPU temperature, frequency, etc.

;;; Code:

(require 'cl)
(require 'mls-common)

(defvar *mls-cpu-previous-stats* nil)
(defvar mls-cpu-timer nil)
(defvar mls-cpu-mode-line-string "")
(defvar mls-cpu-formatters nil)

(defvar mls-cpu-settings
  '((:formats
     ((:primary "&A{c}")
      (:secondary " CPU[%C0{%%},%C1{%%}]")
      (:monitor "&A")))
    (:levels
     (("%A" ((90.0 "crit")
             (50.0 "warn")
             (0.0  "norm")))
      ("%C0" ((90.0 "crit")
              (50.0 "warn")
              (0.0  "norm")))
      ("%C1" ((90.0 "crit")
              (50.0 "warn")
              (0.0  "norm"))))))
  "CPU stats settings.")

(defgroup mls-cpu nil
  "Display various CPU stats in the mode-line."
  :group 'mls-cpu)

(defcustom mls-cpu-update-interval 2
  "Number of seconds between CPU stats recalculation."
  :type 'number
  :group 'mls-cpu)

(defcustom mls-cpu-format "%A %C0 %C1"
  "Format string:
%A - average CPU usage in percent.
%C# - CPU usage of #th core in percent.
%_u - User CPU usage of core _ (either \"A\" or \"C#\").
%_s - System CPU usage of core _.
%_i - IO CPU usage of core _."
  :type 'string
  :group 'mls-cpu)

(defun mls-cpu-update ()
  "Update stats."
  (setq mls-cpu-mode-line-string (mls-cpu-stats))
  (force-mode-line-update)
  (sit-for 0))

(defun mls-cpu-start ()
  "Start displaying CPU usage stats in the mode-line."
  (interactive)
  (setq mls-cpu-mode-line-string "")
  (setq *mls-cpu-previous-stats* (mls-cpu-read-stats))
  (mls-set-timer 'mls-cpu-timer
                 mls-cpu-update-interval
                 'mls-cpu-update))

(defun mls-cpu-stop ()
  "Stop displaying CPU usage stats in the mode-line."
  (interactive)
  (setq mls-cpu-mode-line-string "")
  (mls-cancel-timer 'mls-cpu-timer))

(defun mls-cpu-stats ()
  "Build stats."
  (let ((stats (mls-cpu-fetch)))
    (mls-format-expand mls-cpu-formatters mls-cpu-format stats)))

(defun mls-cpu-fetch ()
  "Returns a bunch of CPU stats for each core and averages in a form of an alist."
  ;; TODO Needs moar stats.
  (let* ((cores (remove-if-not (lambda (line)
                                 (string-prefix-p "cpu" (car line)))
                               (mls-cpu-read-stats)))
         (stats (mls-mapcar*
                        (lambda (cpu prev-cpu)
                          (let* ((norm-user (nth 1 cpu))
                                 (nice-user (nth 2 cpu))
                                 (user (+ norm-user nice-user))
                                 (sys (nth 3 cpu))
                                 (idle (nth 4 cpu))
                                 (iowait (or (nth 5 cpu) 0))
                                 (prev-norm-user (nth 1 prev-cpu))
                                 (prev-nice-user (nth 2 prev-cpu))
                                 (prev-user (+ prev-norm-user prev-nice-user))
                                 (prev-sys (nth 3 prev-cpu))
                                 (prev-idle (nth 4 prev-cpu))
                                 (prev-iowait (or (nth 5 prev-cpu) 0))
                                 (step-denom (float (- (+ user sys idle iowait)
                                                       (+ prev-user prev-sys prev-idle prev-iowait))))
                                 (user-result (/ (- user prev-user)
                                                 step-denom))
                                 (sys-result (/ (- sys prev-sys)
                                                step-denom))
                                 (io-result (/ (- iowait prev-iowait)
                                               step-denom)))
                          (list (car cpu) user-result sys-result io-result)))
                        cores *mls-cpu-previous-stats*)))
    (setq *mls-cpu-previous-stats* cores)
    stats))

(defun mls-cpu-read-stats ()
  "Reads /proc/stat and returns an alist of cores every `mls-cpu-update-interval'."
  (let ((stats (remove-if (lambda (str) (string= str ""))
                 (split-string
                   (shell-command-to-string "cat /proc/stat")
                   "\n"))))
    (mapcar (lambda (str)
              (let ((s (split-string str)))
                (cons (car s)
                      (mapcar #'string-to-number
                              (cdr s)))))
            stats)))

(defmacro mls-cpu-make-functions (name cpu-name)
  `(list
    ;; User CPU usage.
    (cons ,(concat name "u")
          (lambda (stats)
            (format "%.0f" (* 100 (nth 1 (assoc ,cpu-name stats))))))
    ;; System CPU usage.
    (cons ,(concat name "s")
          (lambda (stats)
            (format "%.0f" (* 100 (nth 2 (assoc ,cpu-name stats))))))
    ;; IO CPU usage.
    (cons ,(concat name "i")
          (lambda (stats)
            (format "%.0f" (* 100 (nth 3 (assoc ,cpu-name stats))))))
    ;; CPU usage.
    (cons ,name
          (lambda (stats)
            (format "%.0f" (* 100 (apply #'+ (cdr (assoc ,cpu-name stats)))))))))

(setq mls-cpu-formatters
  `(,@(mls-cpu-make-functions "A" "cpu")
    ,@(mls-cpu-make-functions "C0" "cpu0")
    ,@(mls-cpu-make-functions "C1" "cpu1")
    ,@(mls-cpu-make-functions "C2" "cpu2")
    ,@(mls-cpu-make-functions "C3" "cpu3")
    ,@(mls-cpu-make-functions "C4" "cpu4")
    ,@(mls-cpu-make-functions "C5" "cpu5")
    ,@(mls-cpu-make-functions "C6" "cpu6")
    ,@(mls-cpu-make-functions "C7" "cpu7")
    ; ...
))

(provide 'mls-cpu)
;;; mls-cpu.el ends here
