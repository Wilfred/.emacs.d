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
;; Completion inside emacs style `backticks' inside docstrings and
;; comments.
;;
;; Even when we can't offer an exact set of options, offer the current
;; package as a prefix.
;;
;; Consider only offering autoloaded/required functions in the current
;; file.
;;
;; Offer sensible completion inside (declare ...) forms and
;; (interactive ...) forms.
;; 
;; Consider expanding macros to work out what's bound.
;;
;; Offer &rest and &optional in parameter lists.
;;
;; Offer bound variables inside let/condition-case.
;;
;; (require ...) and (provide ...) should be trivially completable.
;;
;; define-key and global-set-key: offer keymaps and interactive
;; commands.
;;
;; Autocomplete keywords inside rx.
;;
;; Complete () to a function call, unless we're in quotes.
;;
;; Show num args (dropdown) and docstring (minibuffer) when completing.
;;
;; Sort prefix first, but allow substring, so we offer
;; `map-char-table' for input "char-table".
;;
;; Write a crude type inference (look at integer and list assignments)
;; and prioritise arguments to e.g. `nth' (based on its docstring
;; saying arg type) that are known to be integer/lists.
;;
;; Don't offer completion inside quoted forms, except `(foo,
;; bar). This again requires macro expansion.
;;
;; Don't offer completion for the first argument of var-val pairs in
;; `let', as they're usually new vars.
;;
;; Offer completion of FOO in a docstring when `foo' is an argument to
;; a function.

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

(lambda (x y)
  
  (-let [(p q) (some-func)]
    12345))

(defun wh/foo3 (x y)
  ;; `-lambda' too.
  (--map (+ x it) y))

;; TODO: Play with `company-diag'.

;; not relevant, this is for idle completion.
(setq company-minimum-prefix-length 3)
(setq-local company-minimum-prefix-length 3)



(defun company-my-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-my-backend))
    (`prefix (company-grab-symbol))
    (`candidates (list "foobar" "foobaz" "foobarbaz"))
    (`meta (format "This value is named %s" arg))))


(defun wh/company-contextual (command &optional arg &rest ignored)
  "Company backend for elisp that considers context."
  (interactive (list 'interactive))
  (message "command: %S arg: %S" command arg)
  #'
  (pcase command
    (`interactive (company-begin-backend 'wh/company-contextual))
    (`prefix (or (symbol-name (symbol-at-point)) ""))
    (`candidates
     ;; todo: looking back at #'.
     (-map #'symbol-name (wh/bound-syms-at-point)))))

(defun wh/in-obarray-p (sym)
  ab
  (not (null (intern-soft sym))))

(defun wh/bound-syms-at-point ()
  (interactive)
  (let (form)
    (save-excursion
      ;; Insert a placeholder at point.
      (insert " XXX ")
      ;; Move out of the outermost enclosing form.
      (while (> (nth 0 (syntax-ppss)) 0)
        (backward-up-list))
      ;; Read and expand this form.
      (setq form
            (macroexpand-all (read (current-buffer)))))
    (delete-char 5)
    ;; Filter out any symbols created by `make-symbol'. This allows
    ;; us to find deliberate bindings, such as `it' with `--map', but
    ;; not consider symbols from `gensym'.
    (-filter #'wh/in-obarray-p (wh/bound-syms form))))

(defun wh/bound-syms (form &optional accum)
  "Return a list of bound symbols around the symbol XXX in FORM.

Assumes FORM has been fully macro-expanded."
  (catch 'done
    ;; If we've hit the symbol we're looking for, we can return the
    ;; bound symbols we found.
    (when (eq form 'XXX)
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
                  (--map (wh/bound-syms it (append accum args))
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
                    (push (wh/bound-syms val accum)
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
                   (--map (wh/bound-syms it (append accum vars))
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
                    (push (wh/bound-syms val accum-with-vars)
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
                   (--map (wh/bound-syms it (append accum vars))
                          body)))))
         ;; Handle `condition-case', the only other special form that
         ;; can introduce bindings.
         ((eq (car form) 'condition-case)
          (-let [(_ var bodyform . handlers) form]
            (when (eq var 'XXX)
              (throw 'done accum))
            (setq bindings-found
                  (cons
                   (wh/bound-syms bodyform accum)
                   (--map (wh/bound-syms it (append accum (list var)))
                          handlers)))))

         ;; For other forms (`progn' etc) then just recurse to see if it
         ;; contains XXX. We know that it introduces no new bindings. It is
         ;; actually possible to introduce a global with `setq', but we
         ;; ignore that.
         (t
          (setq bindings-found
                (--map (wh/bound-syms it accum)
                       form))))

        ;; For any sublist that didn't contain XXX, we will have
        ;; returned nil. Find the non-empty list, if any.
        (-first #'consp bindings-found)))))

(ert-deftest wh/bound-syms-lambda ()
  (should
   (equal
    (wh/bound-syms '(lambda (x y) XXX))
    (list 'x 'y)))
  (should
   (equal
    (wh/bound-syms '(lambda (x &optional y &rest z) XXX))
    (list 'x 'y 'z))))

(ert-deftest wh/bound-syms-let ()
  ;; Handle bindings introduced by let.
  (should
   (equal
    (wh/bound-syms '(let (x y) XXX))
    (list 'x 'y)))
  (should
   (equal
    (wh/bound-syms '(let ((x 1) (y)) XXX))
    (list 'x 'y)))
  ;; Don't consider previous bindings in the same let.
  (should
   (equal
    (wh/bound-syms '(let ((x 1))
                      (let ((y 2)
                            (z (+ XXX 1)))
                        2)))
    (list 'x)))
  ;; If our placeholder is in the variable position, still consider
  ;; previous keybindings.
  (should
   (equal
    (wh/bound-syms '(let ((x 1))
                      (let (XXX)
                        3)))
    (list 'x)))
  (should
   (equal
    (wh/bound-syms '(let ((x 1))
                      (let ((XXX 2))
                        3)))
    (list 'x))))

(ert-deftest wh/bound-syms-let* ()
  (should
   (equal
    (wh/bound-syms '(let* (x y) XXX))
    (list 'x 'y)))
  (should
   (equal
    (wh/bound-syms '(let* ((x 1) (y)) XXX))
    (list 'x 'y)))
  (should
   (equal
    (wh/bound-syms '(let ((x 1))
                      (let* ((y 2)
                             (z (+ XXX 1)))
                        2)))
    (list 'x 'y)))
  (should
   (equal
    (wh/bound-syms '(let* ((x 1))
                      (let* ((XXX 2))
                        3)))
    (list 'x))))

(ert-deftest wh/bound-syms-condition-case ()
  (should
   (equal
    (wh/bound-syms '(condition-case x (XXX) y))
    nil))
  (should
   (equal
    (wh/bound-syms '(condition-case x y (XXX)))
    (list 'x))))

(ert-deftest wh/bound-syms-progn ()
  (should
   (equal
    (wh/bound-syms '(progn (x y) XXX))
    nil)))

