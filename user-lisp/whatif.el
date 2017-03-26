

(defun whatif--simplify (form bindings)
  "Simplify FORM in the context of BINDINGS using partial application.
Loops are not executed and side-effecting functions are not run."
  (pcase form
    (`nil (list 'value nil))
    (`t (list 'value t))
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
