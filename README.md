# CL-HASH-TABLE-DESTRUCTURING
Hash table destructuring utils. Design similar to the `with-slots` macro.
```lisp
(let ((ht (make-hash-table)))
  (with-hash-table-items (x (y :y) z) ht
    (setf x 1
          y 2
          z 3))
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             ht)
    result))

;; ((Z . 3) (:Y . 2) (X . 1))

(let ((ht (make-hash-table :test #'equal)))
  (with-hash-table-items-fn (x (y :y) z) (ht #'string)
    (setf x 1
          y 2
          z 3))
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             ht)
    result))

;; (("Z" . 3) (:Y . 2) ("X" . 1))
```

### Macro WITH-HASH-TABLE-ITEMS, WITH-HASH-TABLE-ITEMS-FN
#### Syntax

```
with-hash-table-items (entry*) hash-table-form declaration* form*
with-hash-table-items-fn (entry*) (hash-table-form keyfn) declaration* form*

entry ::= variable-name | (variable-name key-form)
```

#### Examples
```lisp
(let ((ht (make-hash-table)))
  (with-hash-table-items (x (y :y) z) ht
    (setf x 1
          y 2
          z 3))
  (values (gethash 'x ht)
          (gethash :y ht)
          (gethash 'z ht)))

;; 1
;; 2
;; 3
```

```lisp
(flet ((make-keyword (sym) (intern (string sym) :keyword)))
  (let ((ht (make-hash-table)))
    (let ((*keyfn* #'make-keyword))
      (with-hash-table-items (foo bar baz) ht
        (setf foo :foo
              bar :bar
              baz :baz))
      (with-hash-table-items (foo bar baz) ht
        (values foo
                bar
                baz)))))

;; :FOO
;; :BAR
;; :BAZ
```

```lisp
(flet ((make-keyword (sym) (intern (string sym) :keyword)))
  (let ((ht (make-hash-table)))
    (with-hash-table-items-fn (foo bar baz) (ht #'make-keyword)
      (setf foo :foo
            bar :bar
            baz :baz))
    (with-hash-table-items-fn (foo bar baz) (ht #'make-keyword)
      (values foo
              bar
              baz))))

;; :FOO
;; :BAR
;; :BAZ
```

### Macro WITH-HASH-TABLE-VALUES, WITH-HASH-TABLE-VALUES-FN
#### Syntax

```
with-hash-table-values (entry*) hash-table-form declaration* form*
with-hash-table-values-fn (entry*) (hash-table-form keyfn) declaration* form*

entry ::= variable-name | (variable-name key-form)
```

#### Examples

```lisp
(let ((ht (make-hash-table)))
  (with-hash-table-items (x y z) ht
    (setf x 1
          y 2
          z 3))
  (let (result)
    (with-hash-table-values (x y z) ht
      (setf result (list x y z))
      (setf x :x
            y :y
            z :z))
    (with-hash-table-values (x y z) ht
      (values result
              (list x y z)))))

;; (1 2 3)
;; (1 2 3)
```

# License
```
Copyright © 2016 Andrey V. Tikhonov <andrey.tikhonov.mailbox@gmail.com>
This work is free. You can redistribute it and/or modify it under the
terms of the Do What The Fuck You Want To Public License, Version 2
as published by Sam Hocevar. See http://www.wtfpl.net/ for more details.
```
