;;; jabber-muc-nick-coloring.el --- Add nick coloring abilyty to emacs-jabber

;; Copyright 2009, 2010, 2012, 2013 Terechkov Evgenii - evg@altlinux.org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))	;for ignore-errors
;; we need hexrgb-hsv-to-hex:
(eval-and-compile
  (or (ignore-errors (require 'hexrgb))
      ;; jabber-fallback-lib/ from jabber/lisp/jabber-fallback-lib
      (ignore-errors
        (let ((load-path (cons (expand-file-name
                                "jabber-fallback-lib"
                                (file-name-directory (locate-library "jabber")))
                               load-path)))
          (require 'hexrgb)))
      (error
       "hexrgb not found in `load-path' or jabber-fallback-lib/ directory.")))

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defcustom jabber-muc-participant-colors nil
  "Alist of used colors. Format is (nick . color). Color may be
  in #RGB or textual (like red or blue) notation. Colors will be
  added in #RGB notation for unknown nicks."
  :type '(alist :key-type string :value-type color)
  :group 'jabber-chat)

(defcustom jabber-muc-colorize-local nil
  "Colorize MUC messages from you."
  :type 'boolean
  :group 'jabber-chat)

(defcustom jabber-muc-colorize-foreign nil
  "Colorize MUC messages not from you."
  :type 'boolean
  :group 'jabber-chat)

(defcustom jabber-muc-nick-saturation 1.0
  "Default saturation for nick coloring."
  :type 'float
  :group 'jabber-chat)

(defcustom jabber-muc-nick-value 1.0
  "Default value for nick coloring."
  :type 'float
  :group 'jabber-chat)

(defun jabber-muc-nick-gen-color (nick)
  "Return good enough color from available pool"
  (let ((hue (/ (mod (string-to-number (substring (md5 nick) 0 6) 16) 360) 360.0)))
    (hexrgb-hsv-to-hex hue jabber-muc-nick-saturation jabber-muc-nick-value)))

(defun jabber-muc-nick-get-color (nick)
  "Get NICKs color"
  (let ((color (cdr (assoc nick jabber-muc-participant-colors))))
    (if color
        color
      (progn
        (unless jabber-muc-participant-colors )
        (push (cons nick (jabber-muc-nick-gen-color nick)) jabber-muc-participant-colors)
        (cdr (assoc nick jabber-muc-participant-colors))))))

(provide 'jabber-muc-nick-coloring)

;;; jabber-muc-nick-coloring.el ends here
