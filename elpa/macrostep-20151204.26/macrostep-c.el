;;; macrostep-c.el --- macrostep interface to C preprocessor

;; Copyright (C) 2015 Jon Oddie <j.j.oddie@gmail.com>

;; Author:     Jon Oddie <j.j.oddie@gmail.com>
;; Maintainer: Jon Oddie <j.j.oddie@gmail.com>
;; Created:    27 November 2015
;; Updated:    27 November 2015
;; Version:    0.9
;; Keywords:   c, languages, macro, debugging
;; Url:        https://github.com/joddie/macrostep

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; A thin wrapper around Emacs's built-in `cmacexp' library to provide
;; basic support for expanding C macros using the `macrostep' user
;; interface.  To use, position point on a macro use in a C buffer and
;; type `M-x macrostep-expand'.  The variables `c-macro-preprocessor'
;; and especially `c-macro-cppflags' may need to be set correctly for
;; accurate expansion.

;; This is fairly basic compared to the Emacs Lisp `macrostep'.  In
;; particular, there is no step-by-step expansion, since C macros are
;; expanded in a single "cpp" pass, and no pretty-printing.

;; To hide the buffer containing "cpp" warnings (not recommended), you
;; could do something like:
;;
;; (push `(,(regexp-quote macrostep-c-warning-buffer)
;;          (display-buffer-no-window))
;;       display-buffer-alist)

;;; Code:

(require 'macrostep)
(require 'cmacexp)
(require 'subr-x)                       ; string-trim
(require 'cl-lib)

(define-error 'macrostep-c-non-macro
    "Text around point is not a macro call.")

(define-error 'macrostep-c-expansion-failed
    "Macro-expansion failed.")

(defvar macrostep-c-warning-buffer "*Macroexpansion Warnings*")

;;;###autoload
(defun macrostep-c-mode-hook ()
  (setq macrostep-sexp-bounds-function
        #'macrostep-c-sexp-bounds)
  (setq macrostep-sexp-at-point-function
        #'macrostep-c-sexp-at-point)
  (setq macrostep-environment-at-point-function
        #'ignore)
  (setq macrostep-expand-1-function
        #'macrostep-c-expand-1)
  (setq macrostep-print-function
        #'macrostep-c-print-function)
  (add-hook 'macrostep-mode-off-hook
            #'macrostep-c-mode-off nil t))

(defun macrostep-c-mode-off (&rest ignore)
  (when (derived-mode-p 'c-mode)
    (let ((warning-window
           (get-buffer-window macrostep-c-warning-buffer)))
      (when warning-window
        (quit-window nil warning-window)))))

;;;###autoload
(add-hook 'c-mode-hook #'macrostep-c-mode-hook)

(defun macrostep-c-sexp-bounds ()
  (save-excursion
    (cl-loop
     (let ((region (macrostep-c-sexp-bounds-1)))
       (cond
         ((null region)
          (signal 'macrostep-c-non-macro nil))
         ((macrostep-c-expandable-p region)
          (cl-return region))
         (t
          (condition-case nil
              (progn
                (backward-up-list)
                (skip-syntax-backward "-"))
            (scan-error
             (signal 'macrostep-c-non-macro nil)))))))))

(defun macrostep-c-sexp-bounds-1 ()
  (let ((region (bounds-of-thing-at-point 'symbol)))
    (when region
      (cl-destructuring-bind (symbol-start . symbol-end) region
        (save-excursion
          (goto-char symbol-end)
          (if (looking-at "[[:space:]]*(")
              (cons symbol-start (scan-sexps symbol-end 1))
              region))))))

(defun macrostep-c-expandable-p (region)
  (cl-destructuring-bind (start . end) region
    (condition-case nil
        (cl-destructuring-bind (expansion warnings)
            (macrostep-c-expand-region start end)
          (declare (ignore warnings))
          (and (cl-plusp (length expansion))
               (not (string= expansion (buffer-substring start end)))))
      (macrostep-c-expansion-failed nil))))

(defun macrostep-c-sexp-at-point (start end)
  (cons start end))

(defun macrostep-c-expand-1 (region _ignore)
  (cl-destructuring-bind (start . end) region
    (cl-destructuring-bind (expansion warnings)
        (macrostep-c-expand-region start end)
      (when (cl-plusp (length warnings))
        (with-current-buffer
            (get-buffer-create macrostep-c-warning-buffer)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert warnings)
            (goto-char (point-min)))
          (special-mode)
          (display-buffer (current-buffer)
                          '(display-buffer-pop-up-window
                            (inhibit-same-window . t)
                            (allow-no-window . t)))))
      expansion)))

(defun macrostep-c-expand-region (start end)
  (let ((expansion
         (condition-case nil
             (c-macro-expansion start end
                                (concat c-macro-preprocessor " "
                                        c-macro-cppflags))
           (search-failed
            (signal 'macrostep-c-expansion-failed nil)))))
    (with-temp-buffer
      (save-excursion
        (insert expansion))
      (when (looking-at (regexp-quote "/*"))
        (search-forward "*/"))
      (let ((warnings (buffer-substring (point-min) (point)))
            (expansion (buffer-substring (point) (point-max))))
        (mapcar #'string-trim (list expansion warnings))))))

(defun macrostep-c-print-function (expansion &rest _ignore)
  (with-temp-buffer
    (insert expansion)
    (let ((exit-code
           (shell-command-on-region (point-min) (point-max) "indent" nil t)))
      (when (zerop exit-code)
        (setq expansion (string-trim (buffer-string))))))
  (insert expansion))

(provide 'macrostep-c)

;;; macrostep-c.el ends here
