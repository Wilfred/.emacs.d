;;; logstash-conf.el --- basic mode for editing logstash configuration

;; Copyright (C) 2014 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 21 October 2014
;; Version: 0.3
;; Package-Version: 20150308.518

;;; Commentary:
;; `conf-mode' offers adequate highlighting for Logstash configuration
;; files, but does not indent them correctly. This file defines a
;; simple `logstash-conf-mode' that both highlights and indents.

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
(require 'conf-mode)
(eval-when-compile
  (require 'cl-lib)) ;; cl-incf, cl-decf

(defgroup logstash nil
  "Major mode for editing Logstash configuration files."
  :group 'languages)

(defcustom logstash-indent 4
  "Indentation offset for `logstash-conf-mode'"
  :group 'logstash
  :type 'integer)

(defun logstash--get-faces (pos)
  "Get all the font faces at POS."
  (remq nil
        (list
         (get-char-property pos 'read-face-name)
         (get-char-property pos 'face)
         (plist-get (text-properties-at pos) 'face))))

(defvar logstash--open-parens
  '(?\{ ?\[))
(defvar logstash--close-parens
  '(?\} ?\]))

;; TODO: release these as a reusable package.
(defun logstash--comment-p (pos)
  "Return non-nil if POS is inside a comment."
  (nth 4 (syntax-ppss pos)))

(defun logstash--string-p (pos)
  "Return non-nil if POS is inside a string."
  (memq 'font-lock-string-face (logstash--get-faces pos)))

(defun logstash--open-paren-p ()
  "Return t if point is currently on an open paren."
  (and (looking-at (rx-to-string `(or ,@logstash--open-parens)))
       (not (logstash--comment-p (point)))
       (not (logstash--string-p (point)))))

(defun logstash--close-paren-p ()
  "Return t if point is currently on a close paren."
  (and (looking-at (rx-to-string `(or ,@logstash--close-parens)))
       (not (logstash--comment-p (point)))
       (not (logstash--string-p (point)))))

(defun logstash--open-paren-count ()
  "Return the number of open brackets before point."
  (let ((open-paren-count 0)
        (paren-pattern
         (rx-to-string `(or ,@logstash--open-parens ,@logstash--close-parens))))
    (save-excursion
      (while (search-backward-regexp paren-pattern nil t)
        (cond
         ((logstash--open-paren-p) (cl-incf open-paren-count))
         ((logstash--close-paren-p) (cl-decf open-paren-count)))))
    open-paren-count))

(defun logstash-indent-line ()
  (interactive)
  (let ((initial-column (current-column))
        initial-indentation
        correct-indentation-level)
    ;; Get the current indentation
    (back-to-indentation)
    (setq initial-indentation (current-column))

    ;; Remove it.
    (while (not (zerop (current-column)))
      (delete-char -1))

    ;; Step over trailing close curlies before counting.
    (save-excursion
      (while (looking-at "}")
        (forward-char 1))

      (setq correct-indentation-level (logstash--open-paren-count)))

    ;; Replace with the correct indentation.
    (dotimes (_ (* logstash-indent correct-indentation-level))
      (insert " "))

    ;; Restore point at the same offset on this line.
    (let ((point-offset (- initial-column initial-indentation)))
      (forward-char point-offset))))

;;;###autoload
(defun logstash-conf-mode ()
  (interactive)
  ;; It's a pain to use `define-derived-mode' with conf-mode, so just
  ;; call it directly instead.
  (conf-unix-mode)
  (setq indent-line-function 'logstash-indent-line)
  (setq mode-name "Logstash"))

(provide 'logstash-conf)
;;; logstash-conf.el ends here
