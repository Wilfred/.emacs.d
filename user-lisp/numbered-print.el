;;; numbered-print.el --- easily insert multiple numbered print statements  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Hughes, Wilfred (London)

;; Author: Hughes, Wilfred (London) <me@wilfred.me.uk>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:

(require 'f)
(require 'crux)
(eval-when-compile (require 'cl))

(defvar number-print-templates
  '((python-mode . "print \"%d: line %d in file %s\"")))

(defvar numbered-print-count
  1)

;;;###autoload
(defun numbered-print-insert ()
  (interactive)
  (let* ((template (cdr (assoc major-mode number-print-templates)))
         (raw-text (format template
                           numbered-print-count
                           (line-number-at-pos)
                           (f-filename (buffer-file-name))))
         (text (propertize raw-text 'numbered-print t)))
    (crux-smart-open-line-above)
    (insert text)
    (incf numbered-print-count)))

(defun numbered-print--update ()
  "Renumber all numbered print statements in the buffer."
  (interactive)
  (save-excursion
    nil))

(defun numbered-print-remove ()
  "Remove all numbered print statements from the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))

    ;; Loop over all the lines.
    (catch 'break
      (while t
        ;; The numbered print may be indented, so ensure point is on
        ;; it.
        (back-to-indentation)

        (when (get-text-property (point) 'numbered-print)
          ;; TODO: there must be a nicer way to delete the current line.
          (whole-line-or-region-delete nil))

        (unless (= (forward-line) 0)
          (throw 'break t))))))

(defun numbered-print-reset ()
  (interactive)
  (setq numbered-print-count 1)
  (message "Reset numbered-print-count to 1."))

(provide 'numbered-print)
;;; numbered-print.el ends here
