;;; regexp-utils.el --- making string regexps easier in Emacs

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 21 May 2012
;; Version: 1.0
;; Keywords: regexp, regular expression

;;; Comemntary:

;; Provides a collection of regular expression utilities for dealing
;; with strings. Based on the Perl/Python standard library. All
;; functions are case sensitive.

;; All functions are case sensitive. If you're also using Emacs'
;; standard regular expressions functions, note that these functions
;; change the match data. You can use 'save-match-data to fix this.

;; Functions of the form re--* are considered private. They're
;; probably not what you want to use.

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

(defun re-find-all (regexp string)
  "Find all matches of REGEXP in STRING, return a list of the
 matching substrings. Case sensitive."
  (let ((offset 0)
        (matches nil)
        (case-fold-search nil))
    (while (string-match regexp string offset)
      (push (substring string
                       (match-beginning 0)
                       (match-end 0))
            matches)
      (setq offset (match-end 0)))
    matches))


(defun re-split (regexp string)
  "Split STRING at separators REGEXP."
  (let ((case-fold-search nil))
    (split-string string regexp)))

(defun re-search-p (regexp string)
  "Returns t if REGEXP can be found anywhere inside STRING."
  (let ((case-fold-search nil))
    (if (string-match regexp string)
        t nil)))

(defun re-match-p (regexp string)
  "Returns t if REGEXP matches the start of STRING."
  (re-search-p (format "^%s" regexp) string))

(provide 'regexp-utils)
