(in-package :cl-user)

(defpackage cl-anonfun-asd
  (:use :cl :asdf))

(in-package :cl-anonfun-asd)

(defsystem cl-anonfun
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :description "Anonymous function helpers for Common Lisp"
  :serial t
  :components ((:file "package")
               (:file "anonfun")))
