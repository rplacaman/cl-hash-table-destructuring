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
## License
```
Copyright © 2016 Andrey V. Tikhonov <andrey.tikhonov.mailbox@gmail.com>
This work is free. You can redistribute it and/or modify it under the
terms of the Do What The Fuck You Want To Public License, Version 2,
as published by Sam Hocevar. See http://www.wtfpl.net/ for more details.
```
