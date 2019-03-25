;;; resist-move.el --- Tactile feedback for moving past syntax  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(defvar resist--delay-count 0)
(defvar resist-delay 10)

(defun resist--at-boundary-p (forward)
  (let* ((ppss (syntax-ppss))
         (in-string-p (nth 3 ppss)))
    (and
     (not in-string-p)
     (if forward
         (looking-at ")")
       (looking-back (rx "(") (1- (point)))))))

(defun resist-move--direction (forward)
  (interactive)
  (if (or
       (>= resist--delay-count resist-delay)
       (not (resist--at-boundary-p forward)))
      (progn
        (setq resist--delay-count 0)
        (forward-char (if forward 1 -1)))
    (cl-incf resist--delay-count)))

(defun resist-move-forward ()
  "Move forward character-by-character, but delay on paren boundaries."
  (interactive)
  (resist-move--direction t))

(defun resist-move-backward ()
  "Move forward character-by-character, but delay on paren boundaries."
  (interactive)
  (resist-move--direction nil))

(global-set-key (kbd "C-f") #'resist-move-forward)
(global-set-key (kbd "C-b") #'resist-move-backward)
(global-set-key (kbd "C-f") #'forward-char)
(global-set-key (kbd "C-b") #'backward-char)

(provide 'resist-move)
;;; resist-move.el ends here
