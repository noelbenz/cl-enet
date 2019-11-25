(in-package :enet-examples)

(defvar *side* "None")
(defun log-info (fmtstr &rest args)
  (format t "~A: ~A~%" *side* (apply #'format (cons nil (cons fmtstr args)))))

(defun send-32bit-unsigned-number (peer num)
  (let* ((byte-list (list (mod num 256)
                          (mod (floor num 256) 256)
                          (mod (floor num (expt 256 2)) 256)
                          (mod (floor num (expt 256 3)) 256)))
         (packet (enet:create-packet
                  (make-array 4
                              :element-type '(unsigned-byte 8)
                              :initial-contents byte-list)
                  :reliable t)))
    (enet:send-packet peer packet)))

(defun read-32bit-unsigned-number (packet)
  (let ((bytes (enet:packet-data packet)))
    (+ (elt bytes 0)
       (* (elt bytes 1) 256)
       (* (elt bytes 2) (expt 256 2))
       (* (elt bytes 3) (expt 256 3)))))

;; Server

(defparameter +port+ 19654)
(defvar *server-thread* nil)
(defvar *peers* ())
(defvar *server-channel* (make-instance 'chanl:unbounded-channel))
(defvar *stop-server* nil)

(defun server-tick (server)
  (enet:with-host-service (event server 1000)
    (enet:handle-event event
      (:connect (:peer peer)
                (log-info "Peer connected.")
                (push peer *peers*))
      (:receive (:packet packet :peer peer)
                (let* ((received-number (read-32bit-unsigned-number packet))
                       (delta (random 5))
                       (new-number (+ received-number delta)))
                  (log-info "Received number ~A, adding ~A, sending number ~A"
                            received-number delta new-number)
                  (send-32bit-unsigned-number peer new-number)))
      (:disconnect ()
                   (log-info "Peer disconnected."))))
  (setf *stop-server* (first (list (chanl:recv *server-channel* :blockp nil))))
  (when *stop-server*
    (log-info "Received stop signal.")))

(defun start-server ()
  (unless (and *server-thread* (bt:thread-alive-p *server-thread*))
    (setf *server-thread*
          (let ((o *standard-output*))
            (bt:make-thread
             (lambda ()
               (let ((*side* "Server"))
                 (setf *standard-output* o)
                 (enet:with-server (server :port +port+)
                   (let ((*stop-server* nil))
                     (restart-case
                         (loop while (not *stop-server*) do
                              (restart-case (server-tick server)
                                (return-to-loop () t))
                              (sleep 1))
                       (stop-server () (setf *stop-server* t)))
                     (setf *peers* ())))
                 (log-info "Server shutting down."))))))))
(export 'start-server)

(defun stop-server ()
  (when (and *server-thread* (bt:thread-alive-p *server-thread*))
    (format t "Sending stop signal to server thread...~%")
    (chanl:send *server-channel* t :blockp t)))
(export 'stop-server)

;; Client

(defvar *client-channel* (make-instance 'chanl:unbounded-channel))
(defvar *client-thread* nil)
(defvar *stop-client* nil)

(defun client-tick (client peer)
  (enet:with-host-service (event client 1000)
    (enet:handle-event event
      (:connect ()
                (log-info "Successfully connected. Sending number 1")
                (send-32bit-unsigned-number peer 1))
      (:receive (:packet packet)
                (let* ((received-number (read-32bit-unsigned-number packet))
                       (delta (random 5))
                       (new-number (+ received-number delta)))
                  (log-info "Received number ~A, adding ~A, sending number ~A"
                            received-number delta new-number)
                  (send-32bit-unsigned-number peer new-number)))
      (:disconnect ()
                   (log-info "Disconnected."))))
  (setf *stop-client* (first (list (chanl:recv *client-channel* :blockp nil))))
  (when *stop-client*
    (log-info "Received stop signal.")))

(defun start-client ()
  (unless (and *client-thread* (bt:thread-alive-p *client-thread*))
    (setf *client-thread*
          (let ((o *standard-output*))
            (bt:make-thread
             (lambda ()
               (let ((*side* "Client"))
                 (setf *standard-output* o)
                 (enet:with-client (client)
                   (log-info "Attempting to connect.")
                   (enet:with-connect (peer client :host "127.0.0.1" :port +port+)
                     (let ((*stop-client* nil))
                       (restart-case
                           (loop while (not *stop-client*) do
                                (restart-case (client-tick client peer)
                                  (return-to-loop () t))
                                (sleep 1))
                         (stop-client () (setf *stop-client* t))))))
                 (log-info "Client shutting down."))))))))
(export 'start-client)

(defun stop-client ()
  (when (and *client-thread* (bt:thread-alive-p *client-thread*))
    (format t "Sending stop signal to client thread...~%")
    (chanl:send *client-channel* t :blockp t)))
(export 'stop-client)
