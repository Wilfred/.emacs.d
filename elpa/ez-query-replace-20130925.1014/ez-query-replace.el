;;; ez-query-replace.el --- A helpful query-replace.

;; Copyright (C) 2013 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 21 August 2013
;; Version: 20130925.1014
;; X-Original-Version: 0.2
;; Package-Requires: ((dash "1.2.0"))

;;; Commentary:

;; ez-query-replace is a simple wrapper around `query-replace' that adds a
;; default search term, and allows you to conveniently replay old
;; replacements.

;;; Usage:

;; The ez-query-replace commands are autoloaded, so you don't need to
;; (require 'ez-query-replace). Just install ez-query-replace and bind
;; it to your desired keys. I prefer to override the default
;; query-replace bindings:

;; (define-key global-map (kbd "M-%") 'ez-query-replace)
;; (define-key global-map (kbd "C-c M-%") 'ez-query-replace-repeat)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'dash)
(require 'thingatpt)

(defun ez-query-replace/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;; todo: investigate whether we're reinventing the wheel, since query-replace-history already exists
(defvar ez-query-replace/history nil)

;;;###autoload
(defun ez-query-replace ()
  "Replace occurrences of FROM-STRING with TO-STRING, defaulting
to the symbol at point."
  (interactive)
  (let* ((from-string (read-from-minibuffer "Replace what? " (ez-query-replace/dwim-at-point)))
         (to-string (read-from-minibuffer
                     (format "Replace %s with what? " from-string))))

    ;; if we currently have point on a symbol we're replacing, go back
    (-when-let* ((current-symbol (symbol-at-point))
                 (current-symbol-name (symbol-name current-symbol))
                 (string-matches (string-equal current-symbol-name from-string)))
      (forward-symbol -1))

    (add-to-list 'ez-query-replace/history
                 (list (format "%s -> %s" from-string to-string)
                       from-string to-string))
    (perform-replace from-string to-string t nil nil)))

(eval-when-compile (require 'cl)) ; first, second

;;;###autoload
(defun ez-query-replace-repeat ()
  "Run `ez-query-replace' with an old FROM and TO value."
  (interactive)
  (unless ez-query-replace/history
    (error "You haven't used `ez-query-replace yet"))
  (let* ((choices (mapcar 'first ez-query-replace/history))
         (choice (ido-completing-read "Previous replaces: " choices))
         (from-with-to (cdr (assoc choice ez-query-replace/history)))
         (from-string (first from-with-to))
         (to-string (second from-with-to)))
    (perform-replace from-string to-string
                   t nil nil)))

(provide 'ez-query-replace)
;;; ez-query-replace.el ends here
