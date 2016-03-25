(in-package #:cl-user)

(defpackage #:cl-hash-table-destructuring
  (:nicknames #:htd)
  (:use #:cl)
  (:export #:with-hash-table-values-fn
           #:with-hash-table-values
           #:with-hash-table-items-fn
           #:with-hash-table-items
           #:*keyfn*))
