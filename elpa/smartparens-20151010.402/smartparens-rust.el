;;; smartparens-haskell.el --- Additional configuration for Haskell based modes.

;; Copyright (C) 2015 Wilfred Hughes

;; Created: 22 August 2015
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for Haskell based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-config)
;;
;; alternatively, you can explicitly load these preferences:
;;
;; (require 'smartparens-rust)
;;
;; in your configuration.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:
(require 'smartparens)

(sp-pair "'" nil :actions :rem)

(defun always-true ()
  ""
  t)

;; repeatedly evaluating with different arguments does not actually
;; change `sp-local-pairs'!
(sp-with-modes '(rust-mode)
  (sp-local-pair
   "'" "'")
  )

(provide 'smartparens-rust)

;;; smartparens-rust.el ends here

