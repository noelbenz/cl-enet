(in-package #:enet)

(define-condition enet-error (error)
  ((function-name :initarg :function-name :reader function-name)
   (error-code :initarg :error-code :reader error-code)))
(export 'enet-error)
(export 'function-name)
(export 'error-code)

(defmacro %handle-error-call (fn &rest args)
  (with-gensyms (error-code)
    `(let ((,error-code (apply (function ,fn) ,args)))
       (when (/= ,error-code 0)
         (error 'enet-error
                :function-name ,(symbol-name fn)
                :error-code ,error-code)))))

(defun initialize ()
  (%handle-error-call enet-ffi:enet-initialize))
(export 'initialize)

(defun deinitialize ()
  (%handle-error-call enet-ffi:enet-deinitialize))
(export 'deinitialize)

(defun create-host (address &key
                              (peer-count 32)
                              (channel-limit 1)
                              (incoming-bandwidth 0)
                              (outgoing-bandwidth 0))
  (let ((host (enet-ffi:enet-host-create address
                                         peer-count
                                         channel-limit
                                         incoming-bandwidth
                                         outgoing-bandwidth)))
    (let ((ptr (autowrap:ptr host)))
      (if (cffi:null-pointer-p ptr)
          (error 'enet-error :function-name 'enet-ffi:enet-host-create)
          (progn (trivial-garbage:finalize
                  host 
                  (lambda () (autowrap:free ptr)))
                 host)))))
(export 'create-host)

(defun create-server (host port &key
                                  (peer-count 32)
                                  (channel-limit 1)
                                  (incoming-bandwidth 0)
                                  (outgoing-bandwidth 0))
  (autowrap:with-alloc (address 'enet-ffi:e-net-address)
    (if (stringp host)
        (error 'error) ;; TODO
        (setf (enet-ffi:e-net-address.host address) host))
    (setf (enet-ffi:e-net-address.port address) port)
    (create-host address
                 :peer-count peer-count
                 :channel-limit channel-limit
                 :incoming-bandwidth incoming-bandwidth
                 :outgoing-bandwidth outgoing-bandwidth)))
(export 'create-server)

(defun create-client (&key (peer-count 1)
                           (channel-limit 1)
                           (incoming-bandwidth 0)
                           (outgoing-bandwidth 0))
  (create-host (cffi:null-pointer)
               :peer-count peer-count
               :channel-limit channel-limit
               :incoming-bandwidth incoming-bandwidth
               :outgoing-bandwidth outgoing-bandwidth))
(export 'create-client)





