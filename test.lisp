(in-package #:cl-user)

(defpackage #:cl-hash-table-da-test
  (:use #:cl
        #:prove
        #:cl-hash-table-da))

(in-package #:cl-hash-table-da-test)

(defmacro ht (&body body)
  `(let ((ht (make-hash-table :test #'equal)))
     ,@body))

(plan 2)

(subtest "with-hash-table-items"

  (is-values
   (ht
     (with-hash-table-items (x y z) ht
       (setf x 1
             y 2
             z 3)
       (values x
               y
               z)))
   '(1 2 3))

  (is-values
   (ht
     (with-hash-table-items ((x :x) y (z "z")) ht
       (setf x 1
             y 2
             z 3))
     (values (gethash :x  ht)
             (gethash 'y  ht)
             (gethash "z" ht)))
   '(1 2 3))

  (is-values
   (ht
     (let ((*keyfn* (lambda (sym) (intern (string sym) :keyword))))
       (with-hash-table-items (x y z) ht
         (setf x 1
               y 2
               z 3)))
     (values (gethash :x ht)
             (gethash :y ht)
             (gethash :z ht)))
   '(1 2 3))

  (is-values
   (ht
     (let ((*keyfn* (lambda (sym)
                      (string-downcase (string sym)))))
       (with-hash-table-items ((foo "Foo") (bar "Bar") baz) ht
         (setf foo 1
               bar 2
               baz 3)))
     (values (gethash "Foo" ht)
             (gethash "Bar" ht)
             (gethash "baz" ht)))
   '(1 2 3))

  (is-values
   (ht
     (with-hash-table-items ((foo nil)) ht
       (setf foo nil))
     (gethash nil ht))
   '(NIL T))

  (is-error
   (macroexpand
    `(with-hash-table-items (a (b :b :x) c) ht
       (values a b c)))
   'type-error
   "item-entry check type failed when macroexpand")
  
  (is-error
   (ht
     (let ((*keyfn* "foo"))
       (with-hash-table-items (a) ht
         a)))
   'type-error
   "*keyfn* check type failed"))


(subtest "with-hash-table-values"

  (is-values
   (ht
     (with-hash-table-items (a b c) ht
       (setf a 1
             b 2
             c 3))
     (with-hash-table-values (a b c) ht
       (setf a :a
             b :b
             c :c))
     (with-hash-table-values (a b c) ht
       (values a
               b
               c)))
   '(1 2 3)))

(finalize)
