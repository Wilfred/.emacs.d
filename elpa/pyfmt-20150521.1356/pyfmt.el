;;; pyfmt.el --- Emacs interface to pyfmt

;; Copyright (C) 2015 Alexandre Héaumé.

;; Author: Alexandre Héaumé <aheaume@gmail.com>
;; Created: 21 May 2015
;; Version: 0.0.1
;; Package-Version: 20150521.1356
;; Keywords: tools
;; Homepage: https://github.com/aheaume/pyfmt.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use pyfmt to format a Python buffer. Cf. https://github.com/Psycojoker/pyfmt

;;; Code:

(defgroup pyfmt nil
  "Emacs interface to pyfmt"
  :prefix "pyfmt-"
  :group 'tools)

(defcustom pyfmt-command "pyfmt"
  "The 'pyfmt' command."
  :type 'string
  :group 'pyfmt)

;;;###autoload
(defun pyfmt ()
  "Format the current buffer according to the pyfmt tool."
  (interactive)
  (unless (executable-find pyfmt-command)
    (error "Can't find %s in the exec path" pyfmt-command))
  (call-process-region (point-min) (point-max) pyfmt-command t t nil (buffer-file-name)))

(provide 'pyfmt)

;;; pyfmt.el ends here
