(in-package #:enet)

(cffi:define-foreign-library libenet
  (:darwin (:or "libenet.dylib" "libenet"))
  (:unix (:or "libenet.so" "libenet"))
  (:windows (:or "enet.dll"))
  (t "libenet"))

(cffi:use-foreign-library libenet)

