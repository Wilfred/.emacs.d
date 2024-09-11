;;; closql.el --- Store EIEIO objects using EmacSQL  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.closql@jonas.bernoulli.dev>
;; Homepage: https://github.com/emacscollective/closql
;; Keywords: extensions

;; Package-Version: 2.0.0
;; Package-Requires: (
;;     (emacs "26.1")
;;     (compat "30.0.0.0")
;;     (emacsql "4.0.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Store uniform EIEIO objects in an EmacSQL database.  SQLite is used
;; as backend.  This library imposes some restrictions on what kind of
;; objects can be stored; it isn't intended to store arbitrary objects.
;; All objects have to share a common superclass and subclasses cannot
;; add any additional instance slots.

;;; Code:

(require 'compat)
(require 'eieio)
(require 'eieio-base)
(require 'emacsql)
(require 'emacsql-sqlite-common)

(eval-when-compile (require 'subr-x))

(eval-and-compile
  (unless (boundp 'eieio--unbound) ; New name since Emacs 28.1.
    (defvaralias 'eieio--unbound 'eieio-unbound nil)))

(eval-when-compile
  (cl-pushnew 'connection eieio--known-slot-names))

(defmacro closql-with-transaction (db &rest body)
  (declare (indent 1))
  `(emacsql-with-transaction (oref ,db connection)
     ,@body))

;;; Objects

(defclass closql-object ()
  ((closql-class-prefix  :initform nil :allocation :class)
   (closql-class-suffix  :initform nil :allocation :class)
   (closql-table         :initform nil :allocation :class)
   (closql-primary-key   :initform nil :allocation :class)
   (closql-foreign-key   :initform nil :allocation :class)
   (closql-order-by      :initform nil :allocation :class)
   (closql-database      :initform nil :initarg :closql-database))
  :abstract t)

(defun closql--closql-object-p (obj)
  ;; Prevent a recursive load when the class object is autoloaded.
  ;; See c1a9b816ec.  Don't #'quote; doesn't exist in older releases.
  (cl-letf (((symbol-function 'eieio--full-class-object)
             #'eieio--class-object))
    (closql-object--eieio-childp obj)))

;;;; Oref

(define-advice eieio-oref (:around (fn obj slot) closql-object)
  "If OBJ is a `closql-object', delegate to `closql-oref'."
  (if (closql--closql-object-p obj)
      (closql-oref obj slot)
    (funcall fn obj slot)))

(defun closql--oref (obj slot)
  (aref obj (eieio--slot-name-index (eieio--object-class obj) slot)))

(defun closql--oref-default (class slot)
  (let ((class (if (symbolp class)
                   (cl--find-class class)
                 (eieio--object-class class))))
    (aref (eieio--class-class-allocation-values class)
          (eieio--class-slot-name-index class slot))))

(defun closql-oref (obj slot)
  (cl-check-type slot symbol)
  (let ((class (eieio--object-class obj)))
    (if-let ((c (eieio--slot-name-index class slot)))
        (let ((value (aref obj c)))
          (if (eq value eieio--unbound)
              (closql-dref obj slot)
            value))
      (if-let ((c (eieio--class-slot-name-index class slot)))
          (aref (eieio--class-class-allocation-values class) c)
        (slot-missing obj slot 'oref)))))

(cl-defgeneric closql-dref (obj slot)
  (let ((c (eieio--slot-name-index (eieio--object-class obj) slot))
        (db (closql--oref obj 'closql-database))
        (props (closql--slot-properties obj slot))
        class table tables)
    (cond
     ((setq class (alist-get :closql-class props))
      (aset obj c
            (closql--remake-instances class db
              (emacsql
               db `[:select * :from $i1
                    :where (= $i2 $s3)
                    :order-by ,(or (closql--oref-default class 'closql-order-by)
                                   [(asc $i4)])]
               (closql--oref-default class 'closql-table)
               (closql--oref-default class 'closql-foreign-key)
               (closql--oref obj (closql--oref-default obj 'closql-primary-key))
               (closql--oref-default class 'closql-primary-key)))))
     ((setq table (alist-get :closql-table props))
      (let ((columns (closql--table-columns db table)))
        (aset obj c
              (mapcar
               (if (length= columns 2) #'cadr #'cdr)
               (emacsql
                db [:select * :from $i1
                    :where (= $i2 $s3)
                    :order-by [(asc $i4)]]
                table
                (car columns)
                (closql--oref obj (closql--oref-default obj 'closql-primary-key))
                (cadr columns))))))
     ((setq tables (alist-get :closql-tables props))
      (pcase-let ((`(,slot-table ,data-table) tables))
        (aset obj c
              (mapcar
               #'cdr
               (emacsql
                db [:select $i1 :from $i2
                    :join $i3 :on (= $i4 $i5)
                    :where (= $i6 $s7)
                    :order-by [(asc id)]]
                (intern (format "%s:*" data-table))
                data-table slot-table
                (intern (format "%s:id" slot-table))
                (intern (format "%s:id" data-table))
                (intern (format "%s:%s" slot-table
                                (closql--oref-default obj 'closql-table)))
                (closql--oref obj (closql--oref-default obj 'closql-primary-key))
                (closql--oref obj 'id))))))
     ((slot-unbound obj (eieio--object-class obj) slot 'oref)))))

;;;; Oset

(define-advice eieio-oset (:around (fn obj slot value) closql-object)
  "If OBJ is a `closql-object', delegate to `closql-oset'."
  (if (closql--closql-object-p obj)
      (closql-oset obj slot value)
    (funcall fn obj slot value)))

(defun closql--oset (obj slot value)
  (aset obj (eieio--slot-name-index (eieio--object-class obj) slot) value))

(defun closql-oset (obj slot value)
  (cl-check-type slot symbol)
  (let ((class (eieio--object-class obj)))
    (if-let ((c (eieio--slot-name-index class slot)))
        (progn (eieio--validate-slot-value class c value slot)
               (when (and (not (eq slot 'closql-database))
                          (closql--oref obj 'closql-database))
                 (closql-dset obj slot value))
               (aset obj c value))
      (if-let ((c (eieio--class-slot-name-index class slot)))
          (progn (eieio--validate-class-slot-value class c value slot)
                 (aset (eieio--class-class-allocation-values class) c value))
        (slot-missing obj slot 'oset value)))))

(cl-defgeneric closql-dset (obj slot value)
  (let* ((db    (closql--oref obj 'closql-database))
         (key   (oref-default obj closql-primary-key))
         (id    (closql--oref obj key))
         (props (closql--slot-properties obj slot))
         (table (alist-get :closql-table props))
         (tables (alist-get :closql-tables props)))
    (cond
     ((alist-get :closql-class props)
      (error "Not implemented for closql-class slots: oset"))
     ((or table (setq table (car tables)))
      (closql-with-transaction db
        (let ((columns (closql--table-columns db table)))
          ;; Caller might have modified value in place.
          (closql--oset obj slot eieio--unbound)
          (let ((list1 (closql-oref obj slot))
                (list2 value)
                elt1 elt2)
            (cond (tables
                   (setq list1 (mapcar (lambda (e) (list (car e))) list1))
                   (setq list2 (mapcar (if (atom (car list2))
                                           #'list
                                         (lambda (e) (list (car e))))
                                       list2)))
                  ((length= columns 2)
                   (setq list1 (mapcar #'list list1))
                   (setq list2 (mapcar #'list list2))))
            ;; `list2' may not be sorted at all and `list1' has to
            ;; be sorted because Elisp and SQLite sort differently.
            (setq list1 (cl-sort list1 #'string< :key #'car))
            (setq list2 (cl-sort list2 #'string< :key #'car))
            (while (progn (setq elt1 (car list1))
                          (setq elt2 (car list2))
                          (or elt1 elt2))
              (let ((key1 (car elt1))
                    (key2 (car elt2)))
                (cond
                 ((and elt1 (or (not elt2) (string< key1 key2)))
                  (apply #'emacsql db
                         `[:delete-from $i1
                           :where ,(closql--where-equal (cons id elt1) 1)]
                         table
                         (cl-mapcan #'list columns (cons id elt1)))
                  (pop list1))
                 ((string= key1 key2)
                  (unless (equal elt1 elt2)
                    (cl-mapc
                     (lambda (col val1 val2)
                       (unless (equal val1 val2)
                         (emacsql db [:update $i1 :set (= $i2 $s3)
                                      :where (and (= $i4 $s5) (= $i6 $s7))]
                                  table col val2
                                  (car  columns) id
                                  (cadr columns) key2)))
                     (cddr columns)
                     (cdr  elt1)
                     (cdr  elt2)))
                  (pop list1)
                  (pop list2))
                 (t
                  (emacsql db [:insert-into $i1 :values $v2]
                           table (vconcat (cons id elt2)))
                  (pop list2)))))))))
     ((emacsql db [:update $i1 :set (= $i2 $s3) :where (= $i4 $s5)]
               (oref-default obj closql-table)
               slot
               (if (eq value eieio--unbound) 'eieio-unbound value)
               key id)))))

;;;; Slot Properties

(defun closql--slot-properties (object-or-class slot)
  (and-let* ((desc (cl-find slot
                            (closql--object-slots object-or-class)
                            :key #'cl--slot-descriptor-name)))
    (cl--slot-descriptor-props desc)))

(defun closql--object-slots (object-or-class)
  (eieio-class-slots
   (cond
    ((eieio-object-p object-or-class) (eieio--object-class object-or-class))
    ((eieio--class-p object-or-class) object-or-class)
    ((find-class object-or-class 'error)))))

(defconst closql--slot-properties '(:closql-class :closql-table :closql-tables))

(define-advice eieio-defclass-internal
    (:after (cname _superclasses slots _options) closql-object)
  "Handle additional slot properties used by `closql-object' derived classes."
  (when-let* ((class (cl--find-class cname))
              ((child-of-class-p class 'closql-object)))
    (pcase-dolist (`(,name . ,slot) slots)
      (let ((desc (cl-find name
                           (cl-coerce (eieio--class-slots class) 'list)
                           :key (lambda (elt) (aref elt 1)))))
        (dolist (prop closql--slot-properties)
          (when-let
              ((v (plist-get slot prop)))
            (setf (alist-get prop (cl--slot-descriptor-props desc)) v)))))))

(define-advice eieio--slot-override
    (:after (old new _skipnil) closql-object)
  "Handle additional slot properties used by `closql-object' derived classes."
  (dolist (prop closql--slot-properties)
    (when-let
        ((v (alist-get prop (cl--slot-descriptor-props new))))
      (setf (alist-get prop (cl--slot-descriptor-props old)) v))))

;;; Database

(defclass closql-database (eieio-singleton)
  ((name         :initform nil :allocation :class)
   (object-class :initform nil :allocation :class)
   (file         :initform nil :allocation :class)
   (schemata     :initform nil :allocation :class)
   (version      :initform nil :allocation :class)
   (disabled     :initform nil :allocation :class)
   (connection   :initform nil :initarg :connection))
  :abstract t)

(cl-defmethod closql-db ((class (subclass closql-database))
                         &optional livep connection-class)
  (or (and-let* ((db (oref-default class singleton))
                 (conn (and (not (eq db eieio--unbound))
                            (oref db connection))))
        (and (emacsql-live-p conn) db))
      (and (not livep)
           (let* ((file (closql--db-prepare-storage class))
                  (connection-class (or connection-class
                                        (emacsql-sqlite-default-connection)))
                  (conn (make-instance connection-class :file file))
                  (db (make-instance class))) ; ignores slot arguments
             (oset db connection conn)
             (when (and (slot-boundp conn 'handle)
                        (processp (oref conn handle)))
               (set-process-query-on-exit-flag (oref conn handle) nil))
             (emacsql conn [:pragma (= foreign-keys on)])
             (if (not (emacsql-sqlite-list-tables db))
                 (closql--db-create-schema db)
               (let ((code-version (oref-default db version))
                     (data-version (closql--db-get-version db)))
                 (cond
                  ((< code-version data-version)
                   (message
                    "Please update %s package (database schema version %s < %s)"
                    (oref-default db name) code-version data-version)
                   (oset-default class disabled t)
                   (emacsql-close db)
                   (setq db nil))
                  ((closql--db-update-schema db)))))
             db))))

(cl-defmethod closql--db-prepare-storage ((class (subclass closql-database)))
  (when-let ((file (oref-default class file)))
    (when (symbolp file)
      (setq file (symbol-value file)))
    (make-directory (file-name-directory file) t)
    file))

(cl-defmethod closql--db-create-schema ((db closql-database))
  (closql-with-transaction db
    (pcase-dolist (`(,table . ,schema)
                   (symbol-value (oref-default db schemata)))
      (emacsql db [:create-table $i1 $S2] table schema))
    (closql--db-set-version db (oref-default db version))))

(cl-defmethod closql--db-update-schema ((db closql-database))
  (let ((code-version (oref-default db version))
        (data-version (closql--db-get-version db)))
    (when (< data-version code-version)
      (oset-default db disabled t)
      (emacsql-close db)
      (error "Please update %s database (schema version %s < %s)"
             (oref-default db name) data-version code-version))))

(cl-defmethod emacsql-live-p ((db closql-database))
  (and-let* ((conn (oref db connection)))
    (emacsql-live-p conn)))

(cl-defmethod emacsql-enable-debugging ((db closql-database))
  (emacsql-enable-debugging (oref db connection)))

(cl-defmethod emacsql-close ((db closql-database))
  (emacsql-close (oref db connection))
  (oset db connection nil))

(cl-defmethod emacsql ((db closql-database) sql &rest args)
  (mapcar #'closql--extern-unbound
          (apply #'emacsql (oref db connection) sql
                 (mapcar (lambda (arg)
                           (if (stringp arg)
                               (let ((copy (copy-sequence arg)))
                                 (set-text-properties 0 (length copy) nil copy)
                                 copy)
                             arg))
                         args))))

(cl-defmethod closql-insert ((db closql-database) obj &optional replace)
  (closql--oset obj 'closql-database db)
  (let (alist)
    (dolist (slot (eieio-class-slots (eieio--object-class obj)))
      (setq  slot (cl--slot-descriptor-name slot))
      (when (alist-get :closql-table (closql--slot-properties obj slot))
        (push (cons slot (closql-oref obj slot)) alist)
        (closql--oset obj slot eieio--unbound)))
    (closql-with-transaction db
      (emacsql db
               (if replace
                   [:insert-or-replace-into $i1 :values $v2]
                 [:insert-into $i1 :values $v2])
               (oref-default obj closql-table)
               (pcase-let ((`(,class ,_db . ,values)
                            (closql--intern-unbound
                             (closql--coerce obj 'list))))
                 (vconcat (cons (closql--abbrev-class
                                 (if (eieio--class-p class)    ; see 7db24ab
                                     (eieio--class-name class) ; Emacs 26
                                   class))                     ; Emacs 27+
                                values))))
      (pcase-dolist (`(,slot . ,value) alist)
        (closql-dset obj slot value))))
  obj)

(cl-defmethod closql-delete ((obj closql-object))
  (let ((key (oref-default obj closql-primary-key)))
    (emacsql (closql--oref obj 'closql-database)
             [:delete-from $i1 :where (= $i2 $s3)]
             (oref-default obj closql-table)
             key
             (closql--oref obj key))))

(cl-defmethod closql-reload ((obj closql-object))
  (or (closql-get (closql--oref obj 'closql-database)
                  (closql--oref obj (oref-default obj closql-primary-key))
                  (eieio-object-class obj))
      (error "Cannot reload object")))

(cl-defmethod closql-get ((db closql-database) ident &optional class resolve)
  (unless class
    (setq class (oref-default db object-class)))
  (and-let* ((row (car (emacsql db [:select * :from $i1
                                    :where (= $i2 $s3)]
                                (oref-default class closql-table)
                                (oref-default class closql-primary-key)
                                ident))))
    (closql--remake-instance class db row resolve)))

(cl-defmethod closql-query ((db closql-database) &optional select pred class)
  (if select
      (let ((value (closql-select db select pred class)))
        (if (and select (symbolp select))
            (mapcar #'car value)
          value))
    (closql-entries db pred class)))

(cl-defmethod closql-entries ((db closql-database) &optional pred class)
  (unless class
    (setq class (oref-default db object-class)))
  (mapcar (lambda (row)
            (closql--remake-instance class db row))
          (closql-select db '* pred class)))

(cl-defmethod closql-select ((db closql-database) select &optional pred class)
  (unless class
    (setq class (oref-default db object-class)))
  (emacsql db
           `[:select $i1 :from $i2
             ,@(and pred [:where class :in $v3])
             ,@(if-let ((order (oref-default class closql-order-by)))
                   (list :order-by order)
                 '(:order-by [(asc $i4)]))]
           select
           (oref-default class closql-table)
           (and pred (closql-where-class-in pred db))
           (oref-default class closql-primary-key)))

(defun closql--table-columns (db table)
  (mapcar #'cadr (emacsql db [:pragma (funcall table-info $i1)] table)))

(defun closql--db-get-version (db)
  (caar (emacsql db [:pragma user-version])))

(defun closql--db-set-version (db version)
  (cl-assert (integerp version))
  (emacsql db [:pragma (= user-version $s1)] version))

;;; Object/Row Conversion

(cl-defmethod closql--remake-instance ((class (subclass closql-object))
                                       db row &optional resolve)
  (pcase-let*
      ((`(,abbrev . ,values) (closql--extern-unbound row))
       (class-obj (eieio--class-object (closql--expand-abbrev class abbrev)))
       (obj (copy-sequence (eieio--class-default-object-cache class-obj)))
       (values (apply #'vector (cons db values))))
    (dotimes (i (length (eieio--class-slots class-obj)))
      (aset obj (1+ i) (aref values i)))
    (when resolve
      (closql--resolve-slots obj))
    obj))

(defun closql--remake-instances (class db rows)
  (declare (indent defun))
  (mapcar (lambda (row)
            (closql--remake-instance class db row))
          rows))

(cl-defmethod closql--resolve-slots ((obj closql-object))
  (dolist (slot (eieio-class-slots (eieio--object-class obj)))
    (setq  slot (cl--slot-descriptor-name slot))
    (when (and (not (slot-boundp obj slot))
               (let ((props (closql--slot-properties obj slot)))
                 (or (alist-get :closql-class props)
                     (alist-get :closql-table props))))
      (closql--oset obj slot (closql-oref obj slot)))))

(defun closql--intern-unbound (row)
  (mapcar (lambda (elt)
            (if (eq elt eieio--unbound) 'eieio-unbound elt))
          row))

(defun closql--extern-unbound (row)
  (mapcar (lambda (elt)
            (if (eq elt 'eieio-unbound) eieio--unbound elt))
          row))

(defun closql--coerce (object type)
  (cl-coerce (let* ((length (length object))
                    (vector (make-vector length -1)))
               (dotimes (i length)
                 (aset vector i (aref object i)))
               vector)
             type))

(cl-defmethod closql--abbrev-class ((class (subclass closql-object)))
  (let ((name (symbol-name class))
        (prefix (oref-default class closql-class-prefix))
        (suffix (oref-default class closql-class-suffix)))
    (intern (substring name
                       (if prefix    (length prefix)    0)
                       (if suffix (- (length suffix)) nil)))))

(cl-defmethod closql--expand-abbrev ((class (subclass closql-object)) abbrev)
  (intern (concat (oref-default class closql-class-prefix)
                  (symbol-name abbrev)
                  (oref-default class closql-class-suffix))))

(defun closql--where-equal (value offset)
  (vector
   (cons 'and
         (mapcar (lambda (v)
                   (if v
                       (list '=
                             (intern (format "$i%i" (cl-incf offset)))
                             (intern (format "$s%i" (cl-incf offset))))
                     (list 'isnull
                           (intern (format "$i%i" (1- (cl-incf offset 2)))))))
                 value))))

(defun closql-where-class-in (args &optional db)
  (when (symbolp args)
    (setq args (list args)))
  (cond
   ((vectorp args)
    (unless db
      (error "closql-where-class-in: DB cannot be nil if ARGS is a vector"))
    (let ((class (oref-default db object-class))
          (abbrevs nil))
      (mapc (lambda (arg)
              (let ((str (symbol-name arg)))
                (unless (string-match "\\`\\(!\\)?\\([^*]+\\)\\(\\*\\)?\\'" str)
                  (error "closql-where-class-in: invalid type: %s" arg))
                (let* ((exclude (match-beginning 1))
                       (a (intern (match-string 2 str)))
                       (a (cond ((match-beginning 3)
                                 (closql--list-subabbrevs
                                  (closql--expand-abbrev class a)))
                                ((not (class-abstract-p
                                       (closql--expand-abbrev class a)))
                                 (list a)))))
                  (setq abbrevs
                        (if exclude
                            (cl-set-difference abbrevs a)
                          (nconc abbrevs a))))))
            args)
      (vconcat abbrevs)))
   ((vconcat
     (mapcar #'closql--abbrev-class
             (cl-mapcan (lambda (sym)
                          (let ((str (symbol-name sym)))
                            (cond ((string-suffix-p "--eieio-childp" str)
                                   (closql--list-subclasses
                                    (intern (substring str 0 -14)) nil))
                                  ((string-suffix-p "-p" str)
                                   (list (intern (substring str 0 -2))))
                                  ((list sym)))))
                        args))))))

(defun closql--list-subclasses (class &optional result)
  (unless (class-abstract-p class)
    (cl-pushnew class result))
  (dolist (child (eieio--class-children (cl--find-class class)))
    (setq result (closql--list-subclasses child result)))
  result)

(cl-defmethod closql--list-subabbrevs ((class (subclass closql-object))
                                       &optional wildcards)
  (cl-labels
      ((types (class)
         (let ((children (eieio--class-children (cl--find-class class)))
               ;; An abstract base-class may violate its own naming rules.
               (abbrev (ignore-errors (closql--abbrev-class class))))
           (nconc (and (not (class-abstract-p class)) (list abbrev))
                  (and wildcards children
                       (list (if abbrev (intern (format "%s*" abbrev)) '*)))
                  (cl-mapcan #'types children)))))
    (sort (types class) #'string<)))

(cl-defmethod closql--set-object-class ((db closql-database) obj class)
  (let* ((table (oref-default obj closql-table))
         (key   (oref-default obj closql-primary-key))
         (id    (closql--oref obj key)))
    (aset obj 0
          (aref (copy-sequence
                 (eieio--class-default-object-cache
                  (eieio--class-object class)))
                0))
    (emacsql db [:update $i1 :set (= class $s2) :where (= $i3 $s4)]
             table
             (closql--abbrev-class class)
             key id)))

;;; Utilities

(defun closql-format (object string &rest slots)
  "Format a string out of a format STRING and an OBJECT's SLOTS.

STRING is a format-string like for `format'.  OBJECT is an Eieio
object and SLOTS are slots of that object, their values are used
like `format' uses its OBJECTS arguments (which are unrelated to
this function's OBJECT argument, they just have similar names).

While this function does not have much to do with the purpose of
`closql', it is being defined here anyway because Eieio does not
define a similar function under a more appropriate name such as
`eieio-format'."
  (apply #'format string
         (mapcar (lambda (slot) (eieio-oref object slot)) slots)))

;;; _
(provide 'closql)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; closql.el ends here
