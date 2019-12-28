(in-package #:enet-ffi)

(autowrap:c-include
 '(enet autowrap-spec "enet.h")
 :spec-path '(enet autowrap-spec)
 ;; This sysincludes is for Windows. Enet only uses a handful of
 ;; definitions from winsock2.h and stdlib.h (which probably are not
 ;; even needed for writing bindings) so we include just those
 ;; definitions in the spec directory. Running c2ffi against actual
 ;; windows headers was posing too many issues since it was pulling in
 ;; almost all of window's SDK (~8.5Mb spec files). With this
 ;; sysincludes, #include <winsock2.h> and <stdlib.h> will resolve to
 ;; our slim versions of the files.
 :sysincludes (list (namestring
                     (asdf:component-pathname
                      (asdf:find-component (asdf:find-system 'enet)
                                           'autowrap-spec))))
 :exclude-sources (#+darwin
                   "/usr/include/")
 :include-sources (#+darwin
                   "_types.h"
                   #+darwin
                   "_size_t.h"
                   #+darwin
                   "_fd_def.h")
 :exclude-definitions (#+darwin
                       "__darwin_va_list")
 :no-accessors t
 :no-functions t)

