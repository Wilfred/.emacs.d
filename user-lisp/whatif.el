(require 'cl-lib)

(defun whatif--simplify (form bindings)
  "Simplify FORM in the context of BINDINGS using partial application.
Loops are not executed and side-effecting functions are not run."
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
       ;; Otherwise, return the if where we have simplified as much as
       ;; we can.
       (`(unknown ,_)
        (list 'unknown
              `(if ,(cl-second cond) ,(cl-second then) ,(cl-second else))))))
    ;; Remove pointless values in progn, e.g.
    ;; (progn nil (foo) (bar)) -> (progn (foo) (bar))
    (`(progn . ,exprs)
     (let (simplified-exprs current)
       ;; Evaluate every expression in the progn body.
       (dolist (expr exprs)
         (setq current (whatif--simplify expr bindings))
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
       (setq simplified-exprs (nreverse simplified-exprs))

       (if (eq 1 (length simplified-exprs))
           (car simplified-exprs)
         (list 'unknown `(progn ,@(mapcar #'cl-second simplified-exprs))))))
    
    ;; Eliminate or simplify a when statement if we can evaluate the
    ;; condition.
    (`(when ,cond . ,body)
     (pcase (whatif--simplify cond bindings)
       (`(value ,value)
        (if value (whatif--simplify (cons 'progn body) bindings)
          (list 'value nil)))
       (`(unknown ,_) (list 'unknown form))))
    
    (_ (list 'unknown form))))

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
    (list 'unknown '(progn x y)))))
