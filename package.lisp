(in-package #:cl-user)

(defpackage #:cl-hash-table-utils
  (:use #:cl)
  (:import-from #:alexandria
                #:compose
                #:ensure-cons
                #:make-gensym
                #:make-gensym-list)
  (:export #:with-hash-table-values%
           #:with-hash-table-values
           #:with-hash-table-items%
           #:with-hash-table-items
           #:*keyfn*))
