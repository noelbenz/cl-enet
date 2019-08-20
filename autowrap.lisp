(in-package #:enet-ffi)

(setf autowrap:*c2ffi-program* "/usr/local/bin/c2ffi")

(autowrap:c-include
 '(enet autowrap-spec "enet.h")
 :spec-path '(enet autowrap-spec)
 :exclude-sources ("/usr/include/")
 :include-sources ("_types.h"
                   "_size_t.h"
                   "_fd_def.h")
 :exclude-definitions ("__darwin_va_list")
 :no-accessors t
 :no-functions t)

