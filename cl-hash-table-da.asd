(in-package #:cl-user)

(defpackage #:cl-hash-table-da-asd
  (:use #:cl #:asdf))

(in-package #:cl-hash-table-da-asd)

(defsystem #:cl-hash-table-da
  :description "Hash table destructuring assignment macroses"
  :version "0.0.1"
  :author "Andrey V. Tikhonov <andrey.tikhonov.mailbox@gmail.com>"
  :licence "WTFPL"
  :serial t
  :components ((:file "package")
               (:file "cl-hash-table-da"))
  :in-order-to ((test-op (test-op #:cl-hash-table-da-test))))

(defsystem #:cl-hash-table-da-test
  :version "0.0.1"
  :author "Andrey V. Tikhonov <andrey.tikhonov.mailbox@gmail.com>"
  :licence "WTFPL"
  :depends-on (#:cl-hash-table-da
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:test-file "test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
