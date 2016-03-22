(in-package #:cl-hash-table-utils)

(defmacro defhelper (function-name lambda-list &body forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,function-name ,lambda-list
       ,@forms)))

(defhelper make-names (entries)
  (mapcar (compose #'first #'ensure-cons) entries))

(defhelper make-key-forms (entries key-transformer)
  (mapcar (lambda (entry)
            (check-type entry (or symbol (cons symbol (cons t null))))
            (destructuring-bind (variable-name &optional (key variable-name p))
                (ensure-cons entry)
              (if p key `(funcall ,key-transformer ',key))))
          entries))

(defhelper make-expansions (hash-table-name forms)
  (mapcar (lambda (form)
            `(gethash ,form ,hash-table-name))
          forms))


(defparameter *keyfn* #'identity)

(defmacro with-hash-table-values% ((entry &rest entries) (hash-table-form &key (key '*keyfn*)) &body body)
  (let ((entries (cons entry entries)))
    (let* ((hash-table-name (make-gensym :hash-table-))
           (keyfn-name (make-gensym :keyfn-))
           (init-forms (make-expansions hash-table-name (make-key-forms entries keyfn-name)))
           (names (make-names entries)))
      `(let ((,keyfn-name ,key)
             (,hash-table-name (the hash-table ,hash-table-form)))
         (check-type ,hash-table-name hash-table)
         (let ,(mapcar #'list names init-forms)
           ,@body)))))

(defmacro with-hash-table-values ((entry &rest entries) hash-table-form &body body)
  `(with-hash-table-values% ,(cons entry entries) (,hash-table-form)
     ,@body))


(defmacro with-hash-table-items% ((entry &rest entries) (hash-table-form &key (key '*keyfn*)) &body body)
  (let ((entries (cons entry entries)))
    (let* ((hash-table-name (make-gensym :hash-table-))
           (keyfn-name (make-gensym :keyfn-))
           (init-forms (make-key-forms entries keyfn-name))
           (names (make-names entries))
           (gsyms (make-gensym-list (length entries))))
      `(let ((,keyfn-name ,key)
             (,hash-table-name (the hash-table ,hash-table-form)))
         (check-type ,hash-table-name hash-table)
         (let ,(mapcar #'list gsyms init-forms)
           (declare (ignorable ,@gsyms))
           (symbol-macrolet ,(mapcar #'list names (make-expansions hash-table-name gsyms))
             ,@body))))))

(defmacro with-hash-table-items ((entry &rest entries) hash-table-form &body body)
  `(with-hash-table-items% ,(cons entry entries) (,hash-table-form)
     ,@body))
