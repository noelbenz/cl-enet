(in-package #:enet)

(define-condition enet-error (error)
  ((function-name :initarg :function-name :reader function-name)
   (error-code :initarg :error-code :reader error-code)))
(export 'enet-error)
(export 'function-name)
(export 'error-code)

(defmacro %handle-error-c-fun (fn &rest args)
  (with-gensyms (error-code)
    `(let ((,error-code (c-fun ,fn ,@args)))
       (when (/= ,error-code 0)
         (error 'enet-error
                :function-name ,(symbol-name fn)
                :error-code ,error-code)))))

(defun initialize ()
  (%handle-error-c-fun enet-ffi:enet-initialize))
(export 'initialize)

(defun deinitialize ()
  (%handle-error-call enet-ffi:enet-deinitialize))
(export 'deinitialize)

(defun create-host (address &key
                              (peer-count 32)
                              (channel-limit 1)
                              (incoming-bandwidth 0)
                              (outgoing-bandwidth 0))
  (let ((host (c-fun enet-ffi:enet-host-create
                     address
                     peer-count
                     channel-limit
                     incoming-bandwidth
                     outgoing-bandwidth)))
    (let ((ptr (ptr host)))
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
  (c-with ((address enet-ffi:e-net-address))
    (if (stringp host)
        (error 'error) ;; TODO
        (setf (address :host) host))
    (setf (address :port) port)
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

(defun host-service (host timeout)
  (c-let ((event enet-ffi:e-net-event))
    (let ((ret (c-fun enet-ffi:enet-host-service host event timeout)))
      (cond ((> ret 0) (autowrap:autocollect (ptr)
                           event
                         (when (= (event :type) enet-ffi:+enet-event-type-receive+)
                           (c-fun enet-ffi:enet-packet-destroy (event :packet)))
                         (autowrap:free ptr)))
            ((= ret 0) nil)
            ((< ret 0) (error 'enet-error
                              :function-name 'enet-ffi:enet-host-service
                              :error-code ret))))))
(export 'host-service)

(defun %find-handler (handlers type)
  (or (find-if (lambda (handler) (eq (first handler) type)) handlers)
      (list type () ())))

(defmacro %generate-args (event-sym args)
  (loop for (member name) on args by #'cddr
     collect
       (list name (case member
                    (:event event-sym)
                    (t (list event-sym member))))))

(defmacro handle-event (event &body handlers)
  (let ((connect-handler (%find-handler handlers :connect))
        (disconnect-handler (%find-handler handlers :disconnect))
        (receive-handler (%find-handler handlers :receive)))
   (with-gensyms (event-sym)
     `(c-let ((,event-sym enet-ffi:e-net-event :from ,event))
        (case (,event-sym :type)
          (enet-ffi:+enet-event-type-connect+
           (let (%generate-args ,event-sym ,(second connect-handler))
             ,@(cddr connect-handler)))
          (enet-ffi:+enet-event-type-disconnect+
           (let (%generate-args ,event-sym ,(second disconnect-handler))
             ,@(cddr disconnect-handler)))
          (enet-ffi:+enet-event-type-receive+
           (let (%generate-args ,event-sym ,(second receive-handler))
             ,@(cddr receive-handler)))
          (enet-ffi:+enet-event-type-none+ nil))))))
(export 'handle-event)

