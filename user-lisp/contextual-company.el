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
        (-let* (((_ var-vals . body) form)
                ;; Handle both (let ((x 1)) _) and (let (x) _).
                (vars (--map (if (consp it) (car it) it) var-vals))
                (vals (--map (if (consp it) (cadr it) nil) var-vals)))
          (setq bindings-found
                (append
                 (--map (wh/bound-syms it accum) vals)
                 (--map (wh/bound-syms it (append accum vars)) body)))))

       ((eq (car form) 'let*)
        (-let* (((_ var-vals . body) form)
                (accum-with-vars accum)
                (body-vars nil))
          (--each var-vals
            ;; E.g. (let* ((x a) (y b) z) c)
            (if (consp it)
                (-let [(var val) it]
                  ;; `x' will be bound in the body.
                  (push var body-vars)
                  ;; `a' will be evaluated without `x' bound.
                  (push (wh/bound-syms val accum-with-vars)
                        bindings-found)
                  ;; `x' will be bound when evaluating `b' on the next
                  ;; iteration.
                  (setq accum-with-vars
                        (append accum-with-vars (list var))))
              ;; Otherwise, a variable without a binding, like `z' in
              ;; our example.
              (push it body-vars)))
          (setq body-vars (nreverse body-vars))
          (setq bindings-found
                (append
                 (nreverse bindings-found)
                 (--map (wh/bound-syms it (append accum body-vars))
                        body))))
        ((eq (car form) 'condition-case)
         (-let [(var bodyform . handlers)]
           (setq bindings-found
                 (cons
                  (wh/bound-syms bodyform accum)
                  (--map (wh/bound-syms it (append accum (list var)))
                         handlers))))))

       ;; For other forms (`progn' etc) then just recurse to see if it
       ;; contains XXX. We know that it introduces no new bindings. It is
       ;; actually possible to introduce a global with `setq', but we
       ;; ignore that.
       (t
        (setq bindings-found
              (--map (wh/bound-syms it accum)
                     (cdr form)))))

      ;; For any sublist that didn't contain XXX, we will have
      ;; returned nil. Find the non-empty list, if any.
      (-first #'consp bindings-found)))))

(wh/bound-syms 'XXX '(a x))
(wh/bound-syms '(function (lambda (x y) XXX)) nil)
(wh/bound-syms (macroexpand-all '(lambda (x y) (--map XXX y))))

(wh/bound-syms '(lambda (x y)
                  (progn x y
                         (let ((z a) zz (x))
                           XXX
                           (let (foo bar))
                           nil))))

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
