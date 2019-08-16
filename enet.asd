(in-package #:cl-user)

(asdf:defsystem enet
  :version "0.0.0"
  :license "MIT"
  :author "Noel Benzinger <noelbenzinger@gmail.com>"
  :maintainer "Noel Benzinger <noelbenzinger@gmail.com>"
  :description "Common lisp wrapper around Enet using autowrap"
  :serial t
  :depends-on (:cl-autowrap)
  :components
  ((:module
    autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "enet.h")))
   (:file "package")
   (:file "library")
   (:file "autowrap")))
