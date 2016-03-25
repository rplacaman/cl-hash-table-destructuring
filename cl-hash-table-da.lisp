(in-package #:cl-hash-table-da)

(deftype non-keyword-symbol ()
  '(and symbol (not keyword)))

(deftype function-designator ()
  '(or function non-keyword-symbol))

(deftype item-entry ()
  '(or non-keyword-symbol (cons non-keyword-symbol (cons t null))))

(defmacro defhelper (function-name lambda-list &body forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,function-name ,lambda-list
       ,@forms)))

(defhelper make-names (entries)
  (mapcar (lambda (entry)
            (if (consp entry) (car entry) entry))
          entries))

(defhelper make-key-forms (entries keyfn-name)
  (mapcar (lambda (entry)
            (check-type entry item-entry)
            (if (consp entry) (second entry) `(funcall ,keyfn-name ',entry)))
          entries))

(defhelper make-expansions (forms hash-table-name)
  (mapcar (lambda (form)
            `(gethash ,form ,hash-table-name))
          forms))

(defparameter *keyfn* #'identity)

(defmacro with-hash-table-values-fn ((&rest entries) (hash-table-form keyfn) &body body)
  (let* ((hash-table-name (gensym (string :hash-table)))
         (keyfn-name (gensym (string :keyfn)))
         (init-forms (make-expansions (make-key-forms entries keyfn-name) hash-table-name))
         (names (make-names entries)))
    `(let ((,keyfn-name ,keyfn))
       (check-type ,keyfn-name function-designator)
       (let ((,hash-table-name (the hash-table ,hash-table-form)))
         (declare (ignorable ,hash-table-name))
         (check-type ,hash-table-name hash-table)
         (let ,(mapcar #'list names init-forms)
           ,@body)))))

(defmacro with-hash-table-values ((&rest entries) hash-table-form &body body)
  `(with-hash-table-values-fn ,entries (,hash-table-form *keyfn*)
     ,@body))


(defmacro with-hash-table-items-fn ((&rest entries) (hash-table-form keyfn) &body body)
  (let* ((hash-table-name (gensym (string :hash-table)))
         (keyfn-name (gensym (string :keyfn)))
         (init-forms (make-key-forms entries keyfn-name))
         (names (make-names entries))
         (gsyms (mapcar (lambda (name) (gensym (string name))) names)))
    `(let ((,keyfn-name ,keyfn))
       (check-type ,keyfn-name function-designator)
       (let ((,hash-table-name (the hash-table ,hash-table-form)))
         (declare (ignorable ,hash-table-name))
         (check-type ,hash-table-name hash-table)
         (let ,(mapcar #'list gsyms init-forms)
           (declare (ignorable ,@gsyms))
           (symbol-macrolet ,(mapcar #'list names (make-expansions gsyms hash-table-name))
             ,@body))))))

(defmacro with-hash-table-items ((&rest entries) hash-table-form &body body)
  `(with-hash-table-items-fn ,entries (,hash-table-form *keyfn*)
     ,@body))
