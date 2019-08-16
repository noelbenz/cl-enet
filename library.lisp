(in-package #:enet)

(cffi:define-foreign-library libenet
  (:darwin (:or "libenet.dylib" "libenet"))
  (:unix (:or "libenet.so" "libenet"))
  (:windows (:or "libenet.dll" "libenet"))
  (t "libenet"))

(cffi:use-foreign-library libenet)

