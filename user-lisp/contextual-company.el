;; Ideas:

;; (defcustom racer-complete-in-comments
;;   nil
;;   "foo"
;;   :type 'boolean
;;   :group 'racer)
;;
;; Could complete group value, simple type values, and keys to
;; defcustom.

;; Completing functions and variables based on context.
;;
;; Auto insert space when completing (foo ) if foo takes multiple
;; args.
;;
;; Completion inside emacs style `backticks'.
;;
;; Even when we can't offer an exact set of options, offer the current
;; package as a prefix.
;;
;; Consider only offering autoloaded/required functions in the current
;; file.
;;
;; Offer sensible completion inside (declare ...) forms and
;; (interactive ...) forms.

;; Consider expanding macros to work out what's bound.

(defun wh/foo (x y)
  (let ((a (1+ x))
        b)
    (cl-destructuring-bind (f g) y
      (message "hello"))))

(defun wh/foo2 (x y)
  (let* ((a (1+ x))
         b)
    (-let [(p q) (some-func)]
      12345)))

(defun wh/foo3 (x y)
  ;; `-lambda' too.
  (--map (+ x it) y))

(defun wh/free-syms-at-point ()
  (interactive)
  (save-excursion
    (insert "XXX"))
  (delete-char 3)
  )

(defun wh/bound-syms (form &optional accum)
  "Return a list of bound symbols around the symbol XXX in FORM.

Assumes FORM has been fully macro-expanded."
  (cond
   ;; If we've hit the symbol we're looking for, we can return the
   ;; bound symbols we found.
   ((eq form 'XXX) accum)

   ((consp form)
    (let (bindings-found)
      ;; If this is a lambda form, the enclosed forms have the parameters
      ;; too. TODO: handle &optional and &rest.
      (cond
       ((eq (car form) 'lambda)
        (setq bindings-found
              (--map (wh/bound-syms it (append accum (cadr form)))
                     (cddr form))))
       ;; (let ((x y)) z)
       ;; We know that x is bound when we evaluate z, but not when we
       ;; evaluate y.
       ((eq (car form) 'let)
        (let* ((var-vals (nth 1 form))
               (vars (--map (if (consp it) (car it) it) var-vals))
               (vals (--map (if (consp it) (cadr it) nil) var-vals)))
          (setq bindings-found
                (append
                 (--map (wh/bound-syms it accum) vals)
                 (--map (wh/bound-syms it (append accum vars)) (-slice form 2))))))

       ;; For other forms (`progn' etc) then just recurse to see if it
       ;; contains XXX. We know that it introduces no new bindings. It is
       ;; actually possible to introduce a global with `setq', but we
       ;; ignore that.
       (t
        (setq bindings-found
              (--map (wh/bound-syms it accum)
                     (cdr form)))))

      (-first #'consp bindings-found)))))

(wh/bound-syms 'XXX '(a x))
(wh/bound-syms '(function (lambda (x y) XXX)) nil)
(wh/bound-syms '(lambda (x y) (progn x y XXX)) nil)

(wh/bound-syms '(lambda (x y) (progn x y (let ((z a) zz) XXX))) nil)

(macroexpand-all (symbol-function 'wh/foo))

(macroexpand-all (symbol-function 'wh/foo2))

(macroexpand-all (symbol-function 'wh/foo3))

;; Special forms (does not include lambda!)
;; 
;; defconst
;; let*
;; setq
;; if
;; or
;; while
;; quote
;; defvar
;; cond
;; save-excursion
;; let
;; setq-default
;; catch
;; interactive
;; save-restriction
;; and
;; prog1
;; prog2
;; progn
;; inline
;; save-current-buffer
;; unwind-protect
;; condition-case
;; function
