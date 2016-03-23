(in-package #:cl-user)

(defpackage #:cl-hash-table-utils-asd
  (:use #:cl #:asdf))

(in-package #:cl-hash-table-utils-asd)

(defsystem #:cl-hash-table-utils
  :description ""
  :version "0.0.1"
  :author "Andrey V. Tikhonov <andrey.tikhonov.mailbox@gmail.com>"
  :licence "WTFPL"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "cl-hash-table-utils"))
  :in-order-to ((test-op (test-op #:cl-hash-table-utils-test))))

(defsystem #:cl-hash-table-utils-test
  :description ""
  :version "0.0.1"
  :author "Andrey V. Tikhonov <andrey.tikhonov.mailbox@gmail.com>"
  :licence "WTFPL"
  :depends-on (#:cl-hash-table-utils
               #:prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
