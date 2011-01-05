(in-package :cl-user)

(defpackage cl-anonfun-asd
  (:use :cl :asdf))

(in-package :cl-anonfun-asd)

(defsystem cl-anonfun
  :version "0.1"
  :serial t
  :components ((:file "package")
               (:file "anonfun")))
