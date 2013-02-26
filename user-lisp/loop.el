;;; loop.el -- modern friendly loop structures

;;; Commentary:

;; Emacs lisp is missing loop structures familiar to users of newer
;; languages. This library adds a selection of popular loop structures
;; as well as break and continue.

;;; Todo:

;; * Implement
;; * Document
;; * Examples
;; * Unit test

;; Things to implement:

;; loop-while, loop-for-each, loop-do-while, loop-for
;; loop-break, loop-continue, loop-return

(defmacro loop-while (condition &rest body)
  `(catch 'loop-break
     (while ,condition ,@body)))

(defmacro loop-do-while (condition &rest body)
  (let ((is-first-iteration-var (gensym)))
    `(catch 'loop-break
       (progn
         ,@body
         (while ,condition
           ,@body)))))

;; todo: support vectors and strings
(defmacro loop-for-each (var-with-list &rest body)
  (let ((var (car var-with-list))
        (list (cadr var-with-list))
        (list-var (gensym)))
    `(catch 'loop-break
       `(let ((,list-var ,list)
              (,var))
          (while ,list-var
            (setq ,var (car ,list-var))
            (setq ,list-var (cdr ,list-var))
            ,@body)))))

(defun loop-break (throw 'loop-break))


(provide 'loop)
;;; loop.el ends here
