;;; loop.el -- the missing loops of Emacs

;;; Commentary:

;; Emacs lisp is missing loop structures familiar to users of newer
;; languages. This library adds a bigger selection of loop
;; structures. Every loop structure can also be broken out of.

;;; Todo:

;; * Implement
;; * Document
;; * Unit test
;; * Add return from
;; * Explore adding continue

;; (loop-for (x 0 (< x 10) (incf x))
(defmacro loop-for)

(defmacro loop-for-in)

(defmacro loop-while)

(defmacro loop-do-while)

(provide 'loop)
;;; loop.el ends here
