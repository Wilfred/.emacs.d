;;; insight.el --- overview of elisp functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp

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

;; Inspired by <youtube link here> (C++ on lisp machine)

;;; Code:

;; TODO: Link to advice and symbol property info pages
;; TODO: ability to search for function in manual

(require 'dash)
(require 's)
(require 'trace)
(require 'elisp-refs)

(defun insight--source (fn-symbol)
  "Get the source of function named FN-SYMBOL as text,
plus the path of the containing file."
  (condition-case _err
      (pcase-let ((`(,buf . ,start-pos) (find-function-noselect fn-symbol)))
        (with-current-buffer buf
          (save-excursion
            (goto-char start-pos)
            (forward-sexp)
            (list
             :source
             (buffer-substring start-pos (point))
             :path
             (abbreviate-file-name (buffer-file-name))
             :line
             (line-number-at-pos start-pos)))))
    ;; Could not find source -- probably defined interactively.
    (error nil)))

(defun insight--syntax-highlight (source)
  "Apply font-lock properties to elisp SOURCE."
  ;; Load all of SOURCE in a emacs-lisp-mode buffer, and use its
  ;; highlighting.
  (with-temp-buffer
    (insert source)
    (delay-mode-hooks (emacs-lisp-mode))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun insight--indent-rigidly (string)
  "Indent every line in STRING by 2 spaces."
  (->> (s-lines string)
       (--map (format  "  %s" it))
       (s-join "\n")))

(defun insight--format-properties (symbol)
  "Return a string describing all the properties of SYMBOL."
  (let* ((syms-and-vals
          (-partition 2 (symbol-plist symbol)))
         (lines
          (--map
           (-let [(sym val) it] (format "%s: %s" sym val))
           syms-and-vals)))
    (when lines
      (s-join "\n" lines))))

(defvar-local insight--sym nil)

(defun insight-update ()
  (interactive)
  (insight--update insight--sym))

(defun insight--update (fn-symbol)
  "Update the current *insight* buffer to the latest state of FN-SYMBOL."
  (let ((source (insight--source fn-symbol))
        (inhibit-read-only t)
        (start-pos (point)))
    (erase-buffer)
    (insert
     (format "Symbol: %s\n" fn-symbol)
     "Calling convention: (foo ARG)\n"
     (format "Location: %s\n\n"
             (if source
                 (plist-get source :path)
               "Unknown"))
     "* Description\n"
     (or (documentation fn-symbol)
         "This function has no docstring.")
     "\n\n* Code\n"
     (if source
         (insight--syntax-highlight
          (insight--indent-rigidly
           (plist-get source :source)))
       "Could not find source.")
     "\n\n* Advice\n"
     "None\n\n"
     "* Symbol Properties\n"
     (or
      (insight--format-properties fn-symbol)
      "No properties defined.")
     "\n\n* Callers\n"
     "foo-bar (foo.el:21)
foo-baz (foo.el:31)

* Tools
\(edebug) (edebug once) "
     (insight--trace-button)
     " "
     (insight--forget-button))
    (goto-char start-pos)))

(define-button-type 'insight-forget-button
  'action #'insight--forget
  'follow-link t
  'help-echo "Unbind this function")

;; TODO: it would be nice to optionally delete the source code too.
(defun insight--forget (_button)
  "Unbind the current function."
  (message
   "Forgot function %s"
   (fmakunbound insight--sym)))

(defun insight--forget-button ()
  "Return a button that unbinds the current function."
  (with-temp-buffer
    (insert-text-button
     "Forget"
     :type 'insight-forget-button)
    (buffer-string)))

(define-button-type 'insight-trace-button
  'action #'insight--toggle-trace
  'follow-link t
  'help-echo "Trace calls to this function")

;; TODO: it would be nice to optionally delete the source code too.
(defun insight--toggle-trace (_button)
  "Toggle whether we trace calls to the current function."
  (if (trace-is-traced insight--sym)
      (progn
        (untrace-function insight--sym)
        (message "Stopped tracing %s" insight--sym))
    (trace-function insight--sym)
    (message "Calls to %s will be traced" insight--sym))
  (insight--update insight--sym))

(defun insight--trace-button ()
  "Return a button that toggles tracing."
  (let ((sym insight--sym))
    (with-temp-buffer
      (insert-text-button
       (if (trace-is-traced sym)
           "Stop Tracing"
         "Trace Calls")
       :type 'insight-trace-button)
      (buffer-string))))

(defun insight (fn-symbol)
  "Explore the definition and usage of function FN-SYMBOL."
  (interactive
   (list (elisp-refs--completing-read-symbol "Function: "
                                             #'functionp)))
  (let ((buf (get-buffer-create
              (format "*insight: %s*" fn-symbol))))
    (with-current-buffer buf
      (insight-mode)
      (setq-local insight--sym fn-symbol)
      (setq buffer-read-only t)
      (insight-update)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(define-derived-mode insight-mode outline-mode "Insight")

(define-key insight-mode-map (kbd "g") #'insight-update)

(provide 'insight)
;;; insight.el ends here
