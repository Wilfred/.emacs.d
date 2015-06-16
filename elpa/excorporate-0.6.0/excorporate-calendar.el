;;; excorporate-calendar.el --- Exchange for calendar -*- lexical-binding: t -*-

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

;; Add a calendar keybinding for excorporate-calfw.el.

;;; Code:

;;;###autoload
(require 'calendar)
(require 'excorporate-calfw)

;;;###autoload
(defun exco-calendar-show-day ()
  "Show meetings for the selected date."
  (interactive)
  (apply #'exco-calfw-show-day (calendar-cursor-to-date t)))

;; I arrogantly claim "e" for now, but irresponsibly reserve the right
;; to change it later.
;;;###autoload
(define-key calendar-mode-map "e" #'exco-calendar-show-day)

(provide 'excorporate-calendar)

;;; excorporate-calendar.el ends here
