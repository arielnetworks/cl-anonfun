(in-package :cl-user)

(defpackage cl-anonfun
  (:use :cl)
  (:nicknames :anonfun)
  (:export :fn
           :fnn
           :enable-fn-syntax))
