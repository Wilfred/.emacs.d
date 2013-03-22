;;; with-namespace.el --- Poor-man's namespaces for elisp

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 1.1
;; Keywords: namespaces
;; Package-Requires: ((dash "1.1.0") (loop "1.1"))

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

;;; Commentary

;; This is basic defun/defvar rewriting to get most of the benefits of
;; namespaces (shorter symbols).

;;; Usage

;; (with-namespace "my-project"
;;     (defun foo (x) (1+ x))
;;     (defvar bar 3 "some docstring"))

;; should compile to:

;; (defun my-project-foo (x) (1+ x))
;; (defvar my-project-bar 3 "some docstring")

;;; Todo

;; * Document
;; * Unit test
;; * Explore private functions (presumably using a separate namespace
;;   separator i.e. --)
;; * Explore importing from other namespaces (everything, public only, named only)

;;; Changelog

;; 1.1 -- Added support for defmacro, defconst and defstruct, and
;; allow more to be added by the user
;; 1.0 -- Initial release

;;; Similar projects

;; * https://github.com/skeeto/elisp-fakespace/
;; * https://github.com/sigma/codex

(require 'loop)
(require 'dash)

(defun with-namespace--replace-nested-list (from to list)
  "Replace all occurrences of atom FROM with TO
in an (arbitrarily nested) proper LIST."
  (--map
   (cond
    ((consp it) (with-namespace--replace-nested-list from to it))
    ((eq it from) to)
    ('t it))
   list))

(defvar with-namespace--suported-defs
  (list 'defun 'defvar 'defmacro 'defconst 'defstruct))

;; nope, with-namespace isn't written with with-namespace. That'd be insane. There'd
;; also be bootstrapping issues.
(defun with-namespace--get-definitions (definitions)
  (let ((ns-symbols nil))
    (loop-for-each definition definitions
      (let ((definition-type (car definition))
            (new-symbol (cadr definition)))
        (unless (memq definition-type with-namespace--suported-defs)
          (error "with-namespace doesn't support %s definitions -- file a bug!" (car definition)))
        (add-to-list 'ns-symbols new-symbol)))
    ns-symbols))

(defvar with-namespace--separator "-")

(defmacro with-namespace (prefix &rest definitions)
  "Rewrite a list DEFINITIONS of defun or defvar sexps so their
symbol starts with PREFIX."
  (declare (indent defun))
  (let ((ns-symbols (with-namespace--get-definitions definitions)))
    (loop-for-each ns-symbol ns-symbols
      (let ((fully-qualified-symbol
             (intern
              (concat prefix
                      with-namespace--separator
                      (symbol-name ns-symbol)))))
        (setq definitions
              (with-namespace--replace-nested-list ns-symbol fully-qualified-symbol definitions))))
    `(progn ,@definitions)))

(provide 'with-namespace)
;;; with-namespace.el ends here
