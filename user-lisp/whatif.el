

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
     ;; If we can evaluate the if condition, then simplify to the THEN
     ;; or ELSE of the if.
     (pcase (whatif--simplify cond bindings)
       (`(value ,value)
        (if value (whatif--simplify then bindings)
          (whatif--simplify else bindings)))
       (`(unknown ,_) (list 'unknown form))))
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
         (list 'unknown `(progn ,@(--map (nth 1 it) simplified-exprs))))))
    
    ;; Eliminate or simplify a when statement if we can evaluate the
    ;; condition.
    (`(when ,cond . ,body)
     (pcase (whatif--simplify cond bindings)
       (`(value ,value)
        (if value (whatif--simplify (cons 'progn body) bindings)
          (list 'value nil)))
       (`(unknown ,_) (list 'unknown form))))
    
    (_ (list 'unknown form))))

(whatif--simplify '(if 1 2 3) nil)

(whatif--simplify 'x nil)

(whatif--simplify 1 nil)

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

(ert-deftest simplify--if ()
  (should
   (equal
    (whatif--simplify '(if x y z) nil)
    (list 'unknown '(if x y z))))
  (should
   (equal
    (whatif--simplify '(if x y z) '((x . 42)))
    (list 'unknown 'y))))

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
