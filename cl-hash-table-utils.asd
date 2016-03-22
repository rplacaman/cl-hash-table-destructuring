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
               (:file "cl-hash-table-utils")))
