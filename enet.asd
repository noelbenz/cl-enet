(in-package #:cl-user)

(asdf:defsystem enet
  :version "0.0.0"
  :license "MIT"
  :author "Noel Benzinger <noelbenzinger@gmail.com>"
  :maintainer "Noel Benzinger <noelbenzinger@gmail.com>"
  :description "Common lisp wrapper around Enet using autowrap"
  :serial t
  :depends-on
  (:alexandria
   :cl-autowrap
   :cl-plus-c
   :trivial-garbage
   :cl-ppcre
   :bordeaux-threads
   :chanl)
  :components
  ((:module
    autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "enet.h")
     (:static-file "enet.arm-pc-linux-gnu.spec")
     (:static-file "enet.i386-unknown-freebsd.spec")
     (:static-file "enet.i386-unknown-openbsd.spec")
     (:static-file "enet.i686-apple-darwin9.spec")
     (:static-file "enet.i686-pc-linux-gnu.spec")
     (:static-file "enet.i686-pc-windows-msvc.spec")
     (:static-file "enet.x86_64-apple-darwin9.spec")
     (:static-file "enet.x86_64-pc-linux-gnu.spec")
     (:static-file "enet.x86_64-pc-windows-msvc.spec")
     (:static-file "enet.x86_64-unknown-freebsd.spec")
     (:static-file "enet.x86_64-unknown-openbsd.spec")))
   (:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "enet")
   (:file "example")))
