;;; modalka.el --- Easily introduce native modal editing of your own design -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/modalka
;; Package-Version: 20150911.808
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: modal editing
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a building kit to help switch to modal editing in Emacs. Main
;; goal of the package is making modal editing in Emacs as natural and
;; native as possible. There is no hack, no corner cases, no emulation —
;; just start edit modally the way you want.
;;
;; More information is available in the README.md file.

;;; Code:

(require 'cl-lib)

(defgroup modalka nil
  "Easily introduce native modal editing of your own design"
  :group  'editing
  :tag    "Modalka"
  :prefix "modalka-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/modalka"))

(defcustom modalka-cursor-type t
  "Cursor type to use in `modalka-mode'.

See description of `cursor-type' for mode information, this
variable should follow the same conventions."
  :tag "Cursor Type"
  :type '(or (const :tag "use the cursor specified for the frame" t)
             (const :tag "don't display a cursor"                 nil)
             (const :tag "display a filled box cursor"            box)
             (const :tag "display a hollow box cursor"            hollow)
             (const :tag "display a vertical bar cursor with default width"
                    bar)
             (cons  :tag "display a vertical bar cursor with given width"
                    (const bar) (integer :tag "width of cursor"))
             (const :tag "display a horizontal bar cursor with default height"
                    hbar)
             (cons  :tag "display a horizontal bar cursor with given height"
                    (const hbar (integer :tag "height of cursor")))))

(defvar modalka-mode-map (make-sparse-keymap)
  "This is Modalka mode map, used to translate your keys.")

;;;###autoload
(defun modalka-define-key (actual-key target-key)
  "Register translation from ACTUAL-KEY to TARGET-KEY."
  (define-key
    modalka-mode-map
    actual-key
    (lambda ()
      (interactive)
      (let ((binding (key-binding target-key)))
        (unless (or (memq binding '(nil undefined))
                    (keymapp binding))
          (call-interactively binding))))))

;;;###autoload
(defun modalka-define-kbd (actual-kbd target-kbd)
  "Register translation from ACTUAL-KBD to TARGET-KBD.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode')."
  (modalka-define-key (kbd actual-kbd) (kbd target-kbd)))

;;;###autoload
(defun modalka-define-keys (&rest pairs)
  "Register many translations described by PAIRS.

Every pair should be of this form:

  (ACTUAL-KEY TARGET-KEY)"
  (dolist (args pairs)
    (apply #'modalka-define-key args)))

;;;###autoload
(defun modalka-define-kbds (&rest pairs)
  "Rester many translation described by PAIRS.

Every pair should be of this form:

  (ACTUAL-KEY TARGET-KEY)

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode')."
  (dolist (args pairs)
    (cl-destructuring-bind (actual . target) args
      (modalka-define-key (kbd actual) (kbd target)))))

;;;###autoload
(defun modalka-remove-key (key)
  "Unregister translation from KEY."
  (define-key modalka-mode-map key nil))

;;;###autoload
(defun modalka-remove-kbd (kbd)
  "Unregister translation from KBD.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode')."
  (modalka-remove-kbd (kbd kbd)))

;;;###autoload
(defun modalka-remove-keys (&rest keys)
  "Unregister translation for KEYS."
  (mapc #'modalka-remove-key keys))

;;;###autoload
(defun modalka-remove-kbds (&rest kbds)
  "Unregister translation for KBDS.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode')."
  (apply #'modalka-remove-keys (mapcar #'kbd kbds)))

;;;###autoload
(define-minor-mode modalka-mode
  "Toggle `modalka-mode' minor mode.

With a prefix argument ARG, enable `modalka-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode setups translation of key bindings according to
configuration created previously with `modalka-define-key' and
`modalka-define-keys'."
  nil "↑" modalka-mode-map
  (setq-local cursor-type
              (if modalka-mode
                  modalka-cursor-type
                (default-value 'cursor-type))))

(define-globalized-minor-mode modalka-global-mode
  modalka-mode modalka-mode)

(provide 'modalka)

;;; modalka.el ends here
