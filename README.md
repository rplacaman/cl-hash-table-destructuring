# CL-HASH-TABLE-DA
Common Lisp hash table destructruing assignment macroses
```lisp
(let ((ht (make-hash-table)))
  (with-hash-table-items (foo (bar 'baz)) ht
    (setf foo 1
          bar 2))
  (with-hash-table-items (foo bar baz) ht
    (values foo bar baz)))

;; 1
;; NIL
;; 2
```
