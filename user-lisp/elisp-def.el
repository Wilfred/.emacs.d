;;; elisp-def.el --- macro-aware go-to-definition for elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes

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

;; Find the definition of the symbol at point, intelligently.
;;
;; TODO: features (require/provide)
;; TODO: macro-expand and work out what bindings we are in.
;; TODO: fonts

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'find-func)
(require 'thingatpt)

(defun elisp-def--flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 0.5 nil 'delete-overlay overlay)))

(defun elisp-def--find-library-name (path)
  "A wrapper around `find-library-name' that returns nil if PATH
has no library with that name.

This can happen when users have installed Emacs without its
source code: they have e.g. org.elc but no org.el."
  (condition-case _err
      (find-library-name path)
    (error nil)))

(defun elisp-def--primitive-p (sym callable-p)
  "Return t if SYM is defined in C."
  (if callable-p
      (subrp (indirect-function sym))
    (let ((filename (find-lisp-object-file-name sym 'defvar)))
      (or (eq filename 'C-source)
          (and (stringp filename)
               (equal (file-name-extension filename) "c"))))))

(defun elisp-def--find-global (sym callable-p)
  "Find the buffer and position where SYM is globally defined."
  (let ((primitive-p (elisp-def--primitive-p sym callable-p))
        (path nil)
        (buf nil)
        (pos nil))
    (when callable-p
      (-let [(base-sym . src-path) (find-function-library sym)]
        ;; `base-sym' is the underlying symbol if `sym' is an alias.
        (setq sym base-sym)
        (setq path src-path)))
    (when (and primitive-p path find-function-C-source-directory)
      ;; Convert "src/foo.c" to "".
      (setq path (f-expand path
                           (f-parent find-function-C-source-directory))))

    (cond
     ((and callable-p path)
      ;; Convert foo.elc to foo.el.
      (-when-let (src-path (elisp-def--find-library-name path))
        ;; Open `path' ourselves, so we can widen before searching.
        (setq buf (find-file-noselect src-path))

        ;; Based on `find-function-noselect'.
        (with-current-buffer buf
          ;; If the symbol is defined outside the current range, widen, otherwise preserve narrowing.
          (save-restriction
            (widen)
            (setq pos
                  (if primitive-p
                      (cdr (find-function-C-source sym path nil))
                    (cdr (find-function-search-for-symbol sym nil path)))))
          (when (or (< pos (point-min))
                    (> pos (point-max)))
            (widen)))))
     (callable-p
      ;; Functions defined interactively may have an edebug property
      ;; that contains the location of the definition.
      (-when-let (edebug-info (get sym 'edebug))
        (-let [marker (if (consp edebug-info)
                          (car edebug-info)
                        edebug-info)]
          (setq buf (marker-buffer marker))
          (setq pos (marker-position marker)))))
     ((not callable-p)
      (condition-case _err
          (-let [(sym-buf . sym-pos) (find-definition-noselect sym 'defvar)]
            (setq buf sym-buf)
            (setq pos sym-pos))
        (search-failed nil)
        ;; If your current Emacs instance doesn't match the source
        ;; code configured in find-function-C-source-directory, we can
        ;; get an error about not finding source. Try
        ;; `default-tab-width' against Emacs trunk.
        (error nil))))
    (list buf pos)))

(defun elisp-def--defined-in (sym)
  "All the namespaces that SYM is globally defined in.
Returns a list '(function variable).

Note that macros are in the same namespace as functions."
  (let (result)
    (when (boundp sym)
      (push 'variable result))
    ;; Function or macro.
    (when (fboundp sym)
      (push 'function result))
    result))

;; TODO: consider the following example:

;; (cl-labels ((foo (x y) (+ x y)))
;;   (foo 1 2))
;;
;; If you expand this, you can see `cl-labels' replaces foo with
;; --cl-foo-- so we can't identify where foo is defined or whether
;; it's a function call or a variable.

;; TODO: Handle point being on the # in #'foo.
(defun elisp-def--sharp-quoted-p ()
  "Is the symbol at point sharp quoted?"
  (save-excursion
    (-let [(sym-start . _sym-end) (bounds-of-thing-at-point 'symbol)]
      (when sym-start
        (goto-char sym-start)
        (condition-case _e
            (backward-char 2)
          (beginning-of-buffer nil))
        (looking-at (rx "#'"))))))

(defun elisp-def--namespace-at-point ()
  "Is the symbol at point a function/macro, a global variable, a
quoted variable, or a let-bound variable?"
  ;; If it's a sharp quoted symbol, we know it's a global function
  ;; reference.
  (if (elisp-def--sharp-quoted-p)
      'function
    ;; Otherwise, macro expand the source at point and look at how the
    ;; symbol is used.
    (let* ((src (elisp-def--source-with-placeholder))
           (form (read src))
           (expanded-form (macroexpand-all form))
           (use (elisp-def--use-position expanded-form 'elisp-def--placeholder)))
      use)))

(ert-deftest elisp-def--namespace-at-point ()
  ;; If it's the head of a sexp, this is a function.
  (with-temp-buffer
    (insert "(foo bar)")

    (goto-char (point-min))
    (search-forward "foo")
    (should
     (eq (elisp-def--namespace-at-point)
         'function)))
  ;; If it's an argument to a function, it's a variable.
  (with-temp-buffer
    (insert "(foo bar)")

    (goto-char (point-min))
    (search-forward "bar")
    (should
     (eq (elisp-def--namespace-at-point)
         'variable))))

;; TODO: handle declarations, which aren't usages at all.
;; (let ((FOO ...))) or (lambda (FOO) ...)
(defun elisp-def--use-position (form sym &optional quoted)
  "Is SYM being used as a function, a global variable, or a
quoted symbol in FORM?

Assumes FORM has been macro-expanded."
  (cond
   ((symbolp form)
    (if (eq form sym)
        ;; Normal reference to the variable we're looking for.
        (if quoted 'quoted 'variable)
      ;; Unrelated variable.
      nil))
   ((consp form)
    (cond
     ((eq (car form) sym)
      ;; Function call for the symbol we're looking for.
      (if quoted 'quoted 'function))
     ;; See if this is a quoted form that contains SYM.
     ((eq (car form) 'quote)
      (--any (elisp-def--use-position it sym t) (cdr form)))
     ;; Recurse on the form to see if any arguments contain SYM.
     (t
      (--any (elisp-def--use-position it sym quoted) form))))
   ((vectorp form)
    ;; All elements in a vector are quoted.
    (--any (elisp-def--use-position it sym t)
           (mapcar #'identity form)))))

(ert-deftest elisp-def--use-position ()
  (should
   (eq (elisp-def--use-position '(foo bar) 'foo)
       'function))
  (should
   (eq (elisp-def--use-position '(foo bar) 'bar)
       'variable))
  (should
   (eq (elisp-def--use-position [foo] 'foo)
       'quoted))
  (should
   (eq (elisp-def--use-position '(foo '(bar)) 'bar)
       'quoted))
  (should
   (eq (elisp-def--use-position '(let ((foo (bar)))) 'bar)
       'function)))

(defun elisp-def--source-with-placeholder ()
  "Return the source enclosing the symbol at point,
but with the symbol itself replaced by a placeholder."
  (let* ((placeholder-sym "elisp-def--placeholder")
         (start-pos (point))
         form-start form-end)
    ;; Find the limits of the top-level expression around point.
    (save-excursion
      (beginning-of-defun)
      (setq form-start (point))
      (end-of-defun)
      (setq form-end (point)))
    ;; Copy that top-level expression into a separate buffer, so we
    ;; can modify the source.
    (let ((src (buffer-substring-no-properties form-start form-end)))
      (with-temp-buffer
        (insert src)
        ;; Replace the original symbol at point with a placeholder, so
        ;; we can distinguish it from other occurrences of this symbol within
        ;; the sexp.
        (goto-char (- start-pos form-start))
        (-let [(sym-start . sym-end) (bounds-of-thing-at-point 'symbol)]
          (delete-region sym-start sym-end))
        (insert placeholder-sym)
        (buffer-string)))))

(defun elisp-def--join-and (items)
  "Join a list of strings with commas and \"and\"."
  (cond
   ((= (length items) 0)
    "")
   ((= (length items) 1)
    (car items))
   (t
    (format "%s and %s"
            (s-join ", " (-drop-last 1 items))
            (-last-item items)))))

(defun elisp-def--bound-syms (form sym &optional accum)
  "Return a list of bound symbols around the symbol XXX in FORM.

Assumes FORM has been fully macro-expanded."
  (catch 'done
    ;; If we've hit the symbol we're looking for, we can return the
    ;; bound symbols we found.
    (when (eq form sym)
      (throw 'done accum))

    (when (consp form)
      (let (bindings-found)
        ;; If this is a lambda form, the enclosed forms have the parameters
        ;; too.
        (cond
         ((eq (car form) 'lambda)
          (-let [(_ args . body) form]
            (setq args
                  (--remove (member it '(&optional &rest)) args))
            (setq bindings-found
                  (--map (elisp-def--bound-syms it sym (append accum args))
                         body))))
         ;; (let ((x y)) z)
         ;; We know that x is bound when we evaluate z, but not when we
         ;; evaluate y.
         ((eq (car form) 'let)
          (-let* (((_ var-vals . body) form)
                  (vars nil))
            (--each var-vals
              (if (consp it)
                  (-let [(var val) it]
                    (when (eq var 'XXX)
                      (throw 'done accum))
                    ;; `x' will be bound in the body.
                    (push var vars)
                    ;; `y' will be evaluated without `x' bound.
                    (push (elisp-def--bound-syms val sym accum)
                          bindings-found))
                ;; Otherwise, a variable without a binding, like `z' in
                ;; our example.
                (when (eq it 'XXX)
                  (throw 'done accum))
                (push it vars)))
            (setq vars (nreverse vars))
            (setq bindings-found
                  (append
                   (nreverse bindings-found)
                   (--map (elisp-def--bound-syms it sym (append accum vars))
                          body)))))
         ;; Handle `let*' forms, including bindings introduced by
         ;; previous vars.
         ((eq (car form) 'let*)
          (-let* (((_ var-vals . body) form)
                  (accum-with-vars accum)
                  (vars nil))
            (--each var-vals
              ;; E.g. (let* ((x a) (y b) z) c)
              (if (consp it)
                  (-let [(var val) it]
                    (when (eq var 'XXX)
                      (throw 'done accum))
                    ;; `x' will be bound in the body.
                    (push var vars)
                    ;; `a' will be evaluated without `x' bound.
                    (push (elisp-def--bound-syms val sym accum-with-vars)
                          bindings-found)
                    ;; `x' will be bound when evaluating `b' on the next
                    ;; iteration.
                    (setq accum-with-vars
                          (append accum-with-vars (list var))))
                ;; Otherwise, a variable without a binding, like `z' in
                ;; our example.
                (when (eq it 'XXX)
                  (throw 'done accum))
                (push it vars)))
            (setq vars (nreverse vars))
            (setq bindings-found
                  (append
                   (nreverse bindings-found)
                   (--map (elisp-def--bound-syms it sym (append accum vars))
                          body)))))
         ;; Handle `condition-case', the only other special form that
         ;; can introduce bindings.
         ((eq (car form) 'condition-case)
          (-let [(_ var bodyform . handlers) form]
            (when (eq var 'XXX)
              (throw 'done accum))
            (setq bindings-found
                  (cons
                   (elisp-def--bound-syms bodyform sym accum)
                   (--map (elisp-def--bound-syms it sym (append accum (list var)))
                          handlers)))))

         ;; For other forms (`progn' etc) then just recurse to see if it
         ;; contains XXX. We know that it introduces no new bindings. It is
         ;; actually possible to introduce a global with `setq', but we
         ;; ignore that.
         (t
          (setq bindings-found
                (--map (elisp-def--bound-syms it sym accum)
                       form))))

        ;; For any sublist that didn't contain XXX, we will have
        ;; returned nil. Find the non-empty list, if any.
        (-first #'consp bindings-found)))))

(ert-deftest elisp-def--bound-syms--lambda ()
  (should
   (equal
    (elisp-def--bound-syms '(lambda (x y) XXX) 'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms '(lambda (x &optional y &rest z) XXX) 'XXX)
    (list 'x 'y 'z))))

(ert-deftest elisp-def--bound-syms--let ()
  ;; Handle bindings introduced by let.
  (should
   (equal
    (elisp-def--bound-syms '(let (x y) XXX) 'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms '(let ((x 1) (y)) XXX)  'XXX)
    (list 'x 'y)))
  ;; Don't consider previous bindings in the same let.
  (should
   (equal
    (elisp-def--bound-syms
     '(let ((x 1))
        (let ((y 2)
              (z (+ XXX 1)))
          2))
     'XXX)
    (list 'x)))
  ;; If our placeholder is in the variable position, still consider
  ;; previous keybindings.
  (should
   (equal
    (elisp-def--bound-syms
     '(let ((x 1))
        (let (XXX)
          3))
     'XXX)
    (list 'x)))
  (should
   (equal
    (elisp-def--bound-syms
     '(let ((x 1))
        (let ((XXX 2))
          3))
     'XXX)
    (list 'x))))

(ert-deftest elisp-def--bound-syms--let* ()
  (should
   (equal
    (elisp-def--bound-syms '(let* (x y) XXX) 'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms '(let* ((x 1) (y)) XXX)  'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms
     '(let ((x 1))
        (let* ((y 2)
               (z (+ XXX 1)))
          2))
     'XXX)
    (list 'x 'y)))
  (should
   (equal
    (elisp-def--bound-syms
     '(let* ((x 1))
        (let* ((XXX 2))
          3))
     'XXX)
    (list 'x))))

(ert-deftest elisp-def--bound-syms--condition-case ()
  (should
   (equal
    (elisp-def--bound-syms
     '(condition-case x (XXX) y)
     'XXX)
    nil))
  (should
   (equal
    (elisp-def--bound-syms
     '(condition-case x y (XXX))
     'XXX)
    (list 'x))))

(ert-deftest elisp-def--bound-syms--progn ()
  (should
   (equal
    (elisp-def--bound-syms '(progn (x y) XXX) 'XXX)
    nil)))

(defun elisp-def ()
  "Go to the definition of the symbol at point."
  (interactive)
  (let* ((sym (symbol-at-point))
         (sym-name (symbol-name sym))
         ;; Try to find the namespace by macro expanding the code.
         (namespace (elisp-def--namespace-at-point)))
    ;; If we couldn't identify a function or variable, see which
    ;; namespaces this symbol is bound in.
    (when (eq namespace 'quoted)
      (-let [namespaces (elisp-def--defined-in sym)]
        ;; If the symbol is only bound in one namespace, use that.
        (if (= (length namespaces) 1)
            (setq namespace (car namespaces))
          ;; Otherwise, our static analysis has failed, so just ask
          ;; the user.
          (let* ((formatted-namespaces
                  (elisp-def--join-and
                   (--map (format "a %s" it) namespaces)))
                 (prompt (format "%s is %s, choose: "
                                 sym-name
                                 formatted-namespaces)))
            (setq namespace
                  (intern
                   (completing-read prompt namespaces nil t)))))))

    (-let [(buf pos) (elisp-def--find-global sym (eq namespace 'function))]
      (unless (and buf pos)
        ;; todo: mention if it's due to being a primitive
        (user-error "Could not find definition for %s %s"
                    namespace sym))

      (switch-to-buffer buf)
      (goto-char pos))
    ;; TODO: push position so we can pop.

    ;; POS is actually the start of line where SYM is defined. Work
    ;; out the exact position of SYM, and flash it.
    (let (start-pos end-pos)
      (save-excursion
        (search-forward sym-name)
        (setq end-pos (point))
        (setq start-pos (- end-pos (length sym-name))))
      (elisp-def--flash-region start-pos end-pos))))

;; Overriding xref-find-definitions.
(global-set-key (kbd "M-.") #'elisp-def)

(provide 'elisp-def)
;;; elisp-def.el ends here