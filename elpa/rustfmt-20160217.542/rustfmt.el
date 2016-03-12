;;; rustfmt.el --- Format rust code using rustfmt    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Fredrik Bergroth

;; Author: Fredrik Bergroth <fbergroth@gmail.com>
;; URL: https://github.com/fbergroth/emacs-rustfmt
;; Package-Version: 20160217.542
;; Keywords: convenience
;; Version: 0
;; Package-Requires: ((emacs "24"))

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

;; *** OBSOLETE PACKAGE - This has been merged into rust-mode ***

;; Call `rustfmt-format-buffer' to format the current buffer using rustfmt. It is
;; convenient to bind it to a key, such as:
;;
;;    (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer)
;;
;; Alternatively, run rustfmt before saving rust buffers:
;;
;;    (add-hook 'rust-mode-hook #'rustfmt-enable-on-save)
;;
;; Errors and warnings will be visible in the `*rustfmt*' buffer.

;;; Code:

(defgroup rustfmt nil
  "Format rust buffers using rustfmt."
  :group 'convenience
  :prefix "rustfmt-")

(defcustom rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string)

(defcustom rustfmt-popup-errors nil
  "Display error buffer when rustfmt fails."
  :type 'boolean)

(defun rustfmt--call (buf)
  "Format BUF using rustfmt."
  (with-current-buffer (get-buffer-create "*rustfmt*")
    (erase-buffer)
    (insert-buffer-substring buf)
    (if (zerop (call-process-region (point-min) (point-max) rustfmt-bin t t nil))
        (progn (copy-to-buffer buf (point-min) (point-max))
               (kill-buffer))
      (when rustfmt-popup-errors
        (display-buffer (current-buffer)))
      (error "Rustfmt failed, see *rustfmt* buffer for details"))))

;;;###autoload
(defun rustfmt-format-buffer ()
  "Format the current buffer using rustfmt."
  (interactive)
  (unless (executable-find rustfmt-bin)
    (error "Could not locate executable \"%s\"" rustfmt-bin))

  (let ((cur-point (point))
        (cur-win-start (window-start)))
    (rustfmt--call (current-buffer))
    (goto-char cur-point)
    (set-window-start (selected-window) cur-win-start))
  (message "Formatted buffer with rustfmt."))

;;;###autoload
(defun rustfmt-enable-on-save ()
  "Run rustfmt when saving buffer."
  (interactive)
  (add-hook 'before-save-hook #'rustfmt-format-buffer nil t))

(provide 'rustfmt)
;;; rustfmt.el ends here
