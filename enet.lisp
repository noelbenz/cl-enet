(in-package #:enet)

(define-condition enet-error (error)
  ((function-name :initarg :function-name :reader function-name)
   (error-code :initarg :error-code :reader error-code)))
(export 'enet-error)
(export 'function-name)
(export 'error-code)

(defmacro handle-error-c-fun (fn &rest args)
  (with-gensyms (error-code)
    `(let ((,error-code (c-fun ,fn ,@args)))
       (when (/= ,error-code 0)
         (error 'enet-error
                :function-name ,(symbol-name fn)
                :error-code ,error-code)))))

(defun initialize ()
  (handle-error-c-fun enet-ffi:enet-initialize))
(export 'initialize)

(defun deinitialize ()
  (c-fun enet-ffi:enet-deinitialize))
(export 'deinitialize)

(defmacro with-init (&body body)
  `(unwind-protect
        (progn (initialize) ,@body)
     (deinitialize)))
(export 'with-init)

(defparameter +ip-matcher+ (ppcre:create-scanner "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$"))

(defun fill-address (address host port)
  (c-val ((address enet-ffi:e-net-address))
    (setf (address :port) port)
    (cond
      ((integerp host)
       (setf (address :host) host))
      ((listp host)
       (setf (address :host) (+ (ash (nth 0 host) 24)
                                (ash (nth 1 host) 16)
                                (ash (nth 2 host) 8)
                                (nth 3 host))))
      ((stringp host)
       (if (ppcre:scan +ip-matcher+ host)
           (handle-error-c-fun enet-ffi:enet-address-set-host-ip address host)
           (handle-error-c-fun enet-ffi:enet-address-set-host address host)))
      (t (error 'type-error :datum host)))
    address))

(defmacro with-address ((address host port) &body body)
  `(c-with ((,address enet-ffi:e-net-address))
     (fill-address ,address ,host ,port)
     ,@body))

(defun create-host (address &key
                              (peer-count 32)
                              (channel-limit 1)
                              (incoming-bandwidth 0)
                              (outgoing-bandwidth 0))
  (let* ((host (c-fun enet-ffi:enet-host-create
                     address
                     peer-count
                     channel-limit
                     incoming-bandwidth
                     outgoing-bandwidth)))
    (if (cffi:null-pointer-p (ptr host))
        (error 'enet-error :function-name 'enet-ffi:enet-host-create)
        host)))
(export 'create-host)

(defun destroy-host (host)
  (c-fun enet-ffi:enet-host-destroy host))
(export 'destroy-host)

(defun create-server (&key (host enet-ffi:+enet-host-any+)
                           (port 1234) 
                           (peer-count 32)
                           (channel-limit 1)
                           (incoming-bandwidth 0)
                           (outgoing-bandwidth 0))
  (with-address (address host port)
    (create-host address
                 :peer-count peer-count
                 :channel-limit channel-limit
                 :incoming-bandwidth incoming-bandwidth
                 :outgoing-bandwidth outgoing-bandwidth)))
(export 'create-server)

(defun destroy-server (server)
  (destroy-host server))
(export 'destroy-server)

(defmacro with-server ((server
                        &key
                        (host enet-ffi:+enet-host-any+)
                        (port 1234) 
                        (peer-count 32)
                        (channel-limit 1)
                        (incoming-bandwidth 0)
                        (outgoing-bandwidth 0))
                       &body body)
  `(let ((,server (create-server
                   :host ,host
                   :port ,port
                   :peer-count ,peer-count
                   :channel-limit ,channel-limit
                   :incoming-bandwidth ,incoming-bandwidth
                   :outgoing-bandwidth ,outgoing-bandwidth)))
     (unwind-protect
          (progn ,@body)
       (destroy-server ,server))))
(export 'with-server)

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

(defun destroy-client (client)
  (destroy-host client))
(export 'destroy-client)

(defmacro with-client ((client
                        &key
                        (peer-count 32)
                        (channel-limit 1)
                        (incoming-bandwidth 0)
                        (outgoing-bandwidth 0))
                       &body body)
  `(let ((,client (create-client
                   :peer-count ,peer-count
                   :channel-limit ,channel-limit
                   :incoming-bandwidth ,incoming-bandwidth
                   :outgoing-bandwidth ,outgoing-bandwidth)))
     (unwind-protect
          (progn ,@body)
       (destroy-client ,client))))
(export 'with-client)

(defun connect (client &key (host enet-ffi:+enet-host-any+)
                            (port 1234)
                            (channel-count 1)
                            (data 0))
  (with-address (address host port)
    (let* ((peer (c-fun enet-ffi:enet-host-connect client address channel-count data)))
      (if (cffi:null-pointer-p (ptr peer))
          (error 'enet-error :function-name 'enet-ffi:enet-host-connect)
          peer))))
(export 'connect)

(defun disconnect (peer &optional (data 0))
  (c-fun enet-ffi:enet-peer-disconnect peer data))
(export 'disconnect)


(defun reset-peer (peer)
  (c-fun enet-ffi:enet-peer-reset peer))
(export 'reset-peer)

(defmacro with-connect ((peer
                         client
                         &key
                         (host enet-ffi:+enet-host-any+)
                         (port 1234)
                         (channel-count 1)
                         (data 0))
                        &body body)
  `(let ((,peer (connect ,client
                         :host ,host
                         :port ,port
                         :channel-count ,channel-count
                         :data ,data)))
     (unwind-protect
          (progn ,@body)
       (reset-peer ,peer))))
(export 'with-connect)

(defclass event ()
  ((channel-id :initarg :channel-id :reader channel-id)
   (data :initarg :data :reader data)
   (packet :initarg :packet :reader packet)
   (peer :initarg :peer :reader peer)
   (event-type :initarg :event-type :reader event-type)))
(export 'event)
(export 'channel-id)
(export 'data)
(export 'packet)
(export 'peer)
(export 'event-type)

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "channel-id: ~A data: ~A packet: ~A peer: ~A event-type: ~A"
            (channel-id event)
            (data event)
            (packet event)
            (peer event)
            (event-type event))))

(defun host-service (host timeout)
  (c-with ((enet-event enet-ffi:e-net-event))
    (let ((ret (c-fun enet-ffi:enet-host-service host enet-event timeout)))
      (cond ((> ret 0)
             (let ((event (make-instance 'event
                                         :channel-id (enet-event :channel-id)
                                         :data (enet-event :data)
                                         :packet (enet-event :packet)
                                         :peer (enet-event :peer)
                                         :event-type (enet-event :type))))
               event))
            ((= ret 0) nil)
            ((< ret 0) (error 'enet-error
                              :function-name 'enet-ffi:enet-host-service
                              :error-code ret))))))
(export 'host-service)

(defun destroy-event (event)
  "Destroy an event returned by host-service.

While an event is a lisp object, it may contain a packet which we do need to free."
  (when (not (cffi:null-pointer-p (ptr (packet event))))
    (c-fun enet-ffi:enet-packet-destroy (packet event))))
(export 'destroy-event)

(defmacro with-host-service ((event host timeout) &body body)
  `(let ((,event (host-service ,host ,timeout)))
     (when ,event
       (unwind-protect
            (progn ,@body)
         (destroy-event ,event)))))
(export 'with-host-service)

(defun find-handler (handlers type)
  (or (find-if (lambda (handler) (eq (first handler) type)) handlers)
      (list type () ())))

(defun generate-args (event-sym args)
  (loop for (member name) on args by #'cddr
     collect
       (list name (ecase member
                    (:event event-sym)
                    (:channel-id `(channel-id ,event-sym))
                    (:data `(data ,event-sym))
                    (:peer `(peer ,event-sym))
                    (:packet `(packet ,event-sym))
                    (:event-type `(event-type ,event-sym))))))

(defmacro handle-event (event &body handlers)
  (let ((connect-handler (find-handler handlers :connect))
        (disconnect-handler (find-handler handlers :disconnect))
        (receive-handler (find-handler handlers :receive)))
    `(when (not (null ,event))
       (ecase (event-type ,event)
         (,enet-ffi:+enet-event-type-connect+
          (let ,(generate-args event (second connect-handler))
            ,@(cddr connect-handler)))
         (,enet-ffi:+enet-event-type-disconnect+
          (let ,(generate-args event (second disconnect-handler))
            ,@(cddr disconnect-handler)))
         (,enet-ffi:+enet-event-type-receive+
          (let ,(generate-args event (second receive-handler))
            ,@(cddr receive-handler)))
         (,enet-ffi:+enet-event-type-none+ nil)))))
(export 'handle-event)

(defun create-packet (bytes &key reliable unsequenced unreliable-fragment)
  (let ((flags (logior enet-ffi:+enet-packet-flag-no-allocate+
                       (if reliable enet-ffi:+enet-packet-flag-reliable+ 0)
                       (if unsequenced enet-ffi:+enet-packet-flag-unsequenced+ 0)
                       (if unreliable-fragment enet-ffi:+enet-packet-flag-unreliable-fragment+ 0))))
    (c-let ((c-bytes :unsigned-char :count (length bytes)))
      (etypecase bytes
        (vector (loop for b across bytes for i from 0 do (setf (c-bytes i) (elt bytes i))))
        (list (loop for b in bytes for i from 0 do (setf (c-bytes i) (elt bytes i)))))
      (c-fun enet-ffi:enet-packet-create (c-bytes &) (length bytes) flags))))
(export 'create-packet)

(defun destroy-packet (packet)
  (c-fun enet-ffi:enet-packet-destroy packet))
(export 'destroy-packet)

(defun packet-data (packet)
  (multiple-value-bind (str data-ptr-ptr)
      (autowrap:inhibit-string-conversion (c-ref packet enet-ffi:e-net-packet :data))
    (declare (ignore str))
    (let* ((data-ptr (cffi:mem-ref data-ptr-ptr :pointer))
           (data-length (c-ref packet enet-ffi:e-net-packet :data-length))
           (data (make-array data-length :element-type '(unsigned-byte 8))))
      (loop for i from 0 below data-length do
           (setf (elt data i) (cffi:mem-ref data-ptr :unsigned-char i)))
      data)))
(export 'packet-data)

(defun send-packet (peer packet &optional (channel-id 0))
  (c-fun enet-ffi:enet-peer-send peer channel-id packet))
(export 'send-packet)

(defun send (peer bytes &key (channel-id 0) reliable unsequenced unreliable-fragment)
  (send-packet peer
               (create-packet bytes :reliable reliable
                              :unsequenced unsequenced
                              :unreliable-fragment unreliable-fragment)
               channel-id))
(export 'send)
