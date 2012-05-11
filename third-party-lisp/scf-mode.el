;;; scf-mode.el --- shorten file-names in compilation type buffers

;; this file is not part of Emacs

              ;;;;;;;;{ shorten compilation filename };;;;;;;;;
              ;; scf stands for shorten-compilation-filename ;;
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: minor-mode to shorten file-names in compilation derived major-modes
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sat Oct  1 03:07:18 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Tue Oct  4 13:39:53 2011 (+0800)
;;           By: Le Wang
;;     Update #: 25
;;          URL: https://github.com/lewang/scf-mode
;; Keywords: compilation
;; Compatibility: Emacs23.3+

;;; Installation:

;;    (require 'scf-mode)
;;    (define-key grep-mode-map [(s)] 'scf-mode)
;;

;;; [optional] to shorten grep output automatically:

;;    (add-hook 'grep-mode-hook (lambda () (scf-mode 1)))
;;

;;; Commentary:

;; Shorten long file-name targets to just the `base-name' without directories
;;
;; I only show how to install in `grep-mode', but scf-mode should work for all
;; compilation-mode derived major-modes.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'grep)

(provide 'scf-mode)

(defvar scf-invisible-overlays nil)

(defvar scf-minimum-hide-length 4
  "minimum length of the directory part of path that gets hidden")

(defvar scf-parsed-point-max nil)

(defun scf-mode-has-compilation (&optional mode)
  (setq mode (or mode major-mode))
  (memq 'compilation-mode (loop for parent = mode then (get parent 'derived-mode-parent)
                                while parent
                                collect parent into parents
                                finally return parents)))

(defun scf-add-invisible-overlay (begin end)
  "Add an overlay from `begin' to `end' in the current buffer.  Push the
overlay onto `scf-invisible-overlays'."
  (let ((overlay (make-overlay begin end)))
    (push overlay scf-invisible-overlays)
    (overlay-put overlay 'invisible '(scf . t))
    ;; (overlay-put overlay 'before-string "...")
    ))

(defun scf-has-face (face &optional pos)
  "return true if POS has face."
  (setq pos (or pos (point)))
  (let ((faces (get-text-property (point) 'face)))
    (setq faces (if (symbolp faces)
                    (list (symbol-name faces))
                  (mapcar 'symbol-name faces)))
    (dolist (f faces)
      (when (string-match (format "\\`%s" face) f)
        (return t)))))

;;;###autoload
(define-minor-mode scf-mode
  "shorten file names in a compilation buffer"
  nil
  " SCF"
  nil
  (if scf-mode
      (progn
        (unless (scf-mode-has-compilation major-mode)
          (error "only compilation derived modes need apply."))
        ;; if we are turning on the mode from a major-mode hook, then we need
        ;; to rerun after compilation finishes, as there is likely no output
        ;; yet.
        (add-hook 'compilation-finish-functions (lambda (buf msg)
                                                  (set-buffer buf)
                                                  (scf-mode 1))
                  nil t)
        ;; we don't want the `buffer-invisibility-spec' to grow indefinitely
        (remove-from-invisibility-spec '(scf . t))
        (add-to-invisibility-spec '(scf . t))
        (unless (local-variable-p 'scf-parsed-point-max)
          (set (make-local-variable 'scf-parsed-point-max) nil))
        (save-excursion
          (goto-char (or scf-parsed-point-max
                         (point-min)))
          (font-lock-fontify-region (point) (point-max))
          (while (not (eobp))
            (if (scf-has-face 'compilation-info)
                (let* ((start (point))
                       (fn (buffer-substring-no-properties
                            start
                            (loop until (progn
                                          (goto-char (or (next-single-property-change (point) 'face)
                                                         (point-max)))
                                          (not (scf-has-face 'compilation-info)))
                                  finally return (point))))
                       (base-name (file-name-nondirectory fn))
                       (end (- (+ start (length fn)) (length base-name))))
                  (when (>= (- end start) scf-minimum-hide-length)
                    (scf-add-invisible-overlay start end)))
              (goto-char (or (next-single-property-change (point) 'face)
                             (point-max)))))
          (setq scf-parsed-point-max (point))))
    (remove-from-invisibility-spec '(scf . t))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scf-mode.el ends here
