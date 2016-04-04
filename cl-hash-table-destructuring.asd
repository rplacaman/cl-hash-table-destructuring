(in-package #:cl-user)

(defpackage #:cl-hash-table-destructuring-asd
  (:use #:cl #:asdf))

(in-package #:cl-hash-table-destructuring-asd)

(defsystem #:cl-hash-table-destructuring
  :description "Hash table destructuring utils"
  :version "0.0.1"
  :author "Andrey V. Tikhonov <andrey.tikhonov.mailbox@gmail.com>"
  :licence "WTFPL"
  :components ((:module "src"
                :components ((:file "cl-hash-table-destructuring"))))
  :in-order-to ((test-op (test-op #:cl-hash-table-destructuring-test))))

(defsystem #:cl-hash-table-destructuring-test
  :version "0.0.1"
  :author "Andrey V. Tikhonov <andrey.tikhonov.mailbox@gmail.com>"
  :licence "WTFPL"
  :depends-on (#:cl-hash-table-destructuring
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module "t"
                :components ((:test-file "cl-hash-table-destructuring"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
