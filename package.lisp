(in-package #:cl-user)

(defpackage #:cl-hash-table-da
  (:use #:cl)
  (:export #:with-hash-table-values%
           #:with-hash-table-values
           #:with-hash-table-items%
           #:with-hash-table-items
           #:*keyfn*))
