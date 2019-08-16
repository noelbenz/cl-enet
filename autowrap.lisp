(in-package #:enet-ffi)

(setf autowrap:*c2ffi-program* "/usr/local/bin/c2ffi")

(autowrap:c-include
 '(enet autowrap-spec "enet.h")
 :spec-path '(enet autowrap-spec))

