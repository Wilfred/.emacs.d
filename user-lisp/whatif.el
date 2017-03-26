(require 'cl-lib)

(defun whatif--simplify-progn-body (forms bindings)
  "Simplify all the forms in FORMS using partial application.
If any form evaluates to a simple value, discard it unless
it is is the final form."
  (let (simplified-exprs current)
    ;; Evaluate every expression in the progn body.
    (dolist (form forms)
      (setq current (whatif--simplify form bindings))
      ;; If we evaluated the expression to a value, just throw it
      ;; away.
      (pcase current
        (`(unknown ,subform)
         (push (list 'unknown subform) simplified-exprs))))
    ;; If the last expression was a value, we still need to return
    ;; it.
    (pcase current
      (`(value ,value)
       (push (list 'value value) simplified-exprs)))
    (nreverse simplified-exprs)))

(defun whatif--simplify (form bindings)
  "Simplify FORM in the context of BINDINGS using partial application.
Loops are not executed and side-effecting functions are not run.

Returns a list ('value VALUE) if we could simplify the entire
FORM to an expression, or a list ('unknown NEW-FORM) if some
parts of FORM could not be simplified."
  ;; TODO: replace 'unknown with 'incomplete or something similar.
  (pcase form
    ;; nil and t evaluate to themselves.
    (`nil (list 'value nil))
    (`t (list 'value t))
    ;; Literal keywords, strings and numbers evaluate to themselves.
    ((pred keywordp)
     (list 'value form))
    ((pred stringp)
     (list 'value form))
    ((pred numberp)
     (list 'value form))
    ;; We can evaluate a symbol if it is present in BINDINGS.
    ((pred symbolp)
     (if (assoc form bindings)
         (list 'value (alist-get form bindings))
       (list 'unknown form)))
    (`(if ,cond ,then)
     (whatif--simplify `(if ,cond ,then nil) bindings))
    (`(if ,cond ,then ,else)
     (setq cond (whatif--simplify cond bindings))
     (setq then (whatif--simplify then bindings))
     (setq else (whatif--simplify else bindings))
     (pcase cond
       ;; If we can evaluate the if condition, then simplify to just the
       ;; THEN or the ELSE.
       (`(value ,value)
        (if value then
          else))
       ;; Otherwise, return an if where we have simplified as much as
       ;; we can.
       (`(unknown ,_)
        (list 'unknown
              `(if ,(cl-second cond) ,(cl-second then) ,(cl-second else))))))
    ;; Remove pointless values in progn, e.g.
    ;; (progn nil (foo) (bar)) -> (progn (foo) (bar))
    (`(progn . ,exprs)
     (setq exprs (whatif--simplify-progn-body exprs bindings))
     (if (eq 1 (length exprs))
         (cl-first exprs)
       (list 'unknown `(progn ,@(mapcar #'cl-second exprs)))))
    
    (`(when ,cond . ,body)
     (setq cond (whatif--simplify cond bindings))
     (setq body (whatif--simplify-progn-body body bindings))
     (pcase cond
       (`(value ,value)
        (if value
            (list 'unknown `(progn ,@(mapcar #'cl-second body)))
          (list 'value nil)))
       (`(unknown ,_)
        (list 'unknown
              `(when ,(cl-second cond)
                 ,@(mapcar #'cl-second body))))))
    (`(unless ,cond . ,body)
     (setq cond (whatif--simplify cond bindings))
     (setq body (whatif--simplify-progn-body body bindings))
     (pcase cond
       (`(value ,value)
        (if value
            (list 'value nil)
          (list 'unknown `(progn ,@(mapcar #'cl-second body)))))
       (`(unknown ,_)
        (list 'unknown
              `(unless ,(cl-second cond)
                 ,@(mapcar #'cl-second body))))))
    ;; TODO: backquote.
    (`(quote ,sym)
     (list 'value sym))
    (`(,fn . ,args)
     (if (fboundp fn)
         (let ((simple-args
                (whatif--simplify-progn-body args bindings)))
           (list 'unknown `(,fn ,@(mapcar #'cl-second simple-args))))
       (list 'unknown form)))
    (_ (error "Don't know how to simplify: %s" form))))

(ert-deftest simplify--bool ()
  (should
   (equal
    (whatif--simplify 'nil nil)
    (list 'value nil)))
  (should
   (equal
    (whatif--simplify 't nil)
    (list 'value t))))

(ert-deftest simplify--number ()
  (should
   (equal
    (whatif--simplify 123 nil)
    (list 'value 123))))

(ert-deftest simplify--keyword ()
  (should
   (equal
    (whatif--simplify :foo nil)
    (list 'value :foo))))

(ert-deftest simplify--string ()
  (should
   (equal
    (whatif--simplify "foo" nil)
    (list 'value "foo"))))

(ert-deftest simplify--symbol ()
  (should
   (equal
    (whatif--simplify 'x nil)
    (list 'unknown 'x)))
  (should
   (equal
    (whatif--simplify 'x '((x . 42)))
    (list 'value 42))))

(ert-deftest simplify--quoted-symbol ()
  (should
   (equal
    (whatif--simplify ''x nil)
    (list 'value 'x))))

(ert-deftest simplify--if-by-condition ()
  "We should discard then THEN or the ELSE case
if we can evaluate the condition."
  (should
   (equal
    (whatif--simplify '(if x y z) nil)
    (list 'unknown '(if x y z))))
  (should
   (equal
    (whatif--simplify '(if x y z) '((x . 42)))
    (list 'unknown 'y))))

(ert-deftest simplify--if-body ()
  "We should always simplify the THEN and ELSE."
  (should
   (equal
    (whatif--simplify '(if x y z) '((y . 42) (z . 41)))
    (list 'unknown '(if x 42 41)))))

(ert-deftest simplify--if-condition ()
  "We should always simplify the COND."
  (should
   (equal
    (whatif--simplify '(if (if t x a) y z) '((y . 42) (z . 41)))
    (list 'unknown '(if x 42 41)))))

(ert-deftest simplify--if-without-else ()
  (should
   (equal
    (whatif--simplify '(if t y) nil)
    (list 'unknown 'y)))
  (should
   (equal
    (whatif--simplify '(if nil y) nil)
    (list 'value nil))))

(ert-deftest simplify--progn ()
  (should
   (equal
    (whatif--simplify '(progn 1) nil)
    (list 'value 1)))
  (should
   (equal
    (whatif--simplify '(progn nil y) nil)
    (list 'unknown 'y)))
  (should
   (equal
    (whatif--simplify '(progn x nil y) nil)
    (list 'unknown '(progn x y)))))

(ert-deftest simplify--when ()
  (should
   (equal
    (whatif--simplify '(when nil x y) nil)
    (list 'value nil)))
  (should
   (equal
    (whatif--simplify '(when t x y) nil)
    (list 'unknown '(progn x y))))
  (should
   (equal
    (whatif--simplify '(when x y) '((y . 1)))
    (list 'unknown '(when x 1)))))

(ert-deftest simplify--unless ()
  (should
   (equal
    (whatif--simplify '(unless nil x y) nil)
    (list 'unknown '(progn x y))))
  (should
   (equal
    (whatif--simplify '(unless t x y) nil)
    (list 'value nil)))
  (should
   (equal
    (whatif--simplify '(unless x y) '((y . 1)))
    (list 'unknown '(unless x 1)))))

(ert-deftest simplify--known-fn-call ()
  "If we know we're calling a function, we should simplify its
arguments."
  (should
   (equal
    (whatif--simplify '(message x) '((x . 1)))
    (list 'unknown '(message 1)))))

(ert-deftest simplify--unknown-fn-call ()
  "If we don't recognise the symbol, do nothing."
  (should
   (equal
    (whatif--simplify '(foo x) '((x . 1)))
    (list 'unknown '(foo x)))))
