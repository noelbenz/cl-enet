(in-package :enet-examples)

(defvar *side* "None")
(defun log-info (fmtstr &rest args)
  (format t "~A: ~A~%" *side* (apply #'format (cons nil (cons fmtstr args))))
  (force-output *standard-output*))

(defparameter +port+ 19654)
(defvar *server-thread* nil)
(defvar *peers* ())
(defvar *server-channel* (make-instance 'chanl:unbounded-channel))
(defvar *stop-server* nil)

(defun server-tick (server)
  (enet:with-host-service (event server 1000)
    (enet:handle-event event
      (:connect (:peer peer)
                (log-info "Peer connected.~%")
                (push peer *peers*))))
  (setf *stop-server* (first (list (chanl:recv *server-channel* :blockp nil))))
  (when *stop-server*
    (log-info "Received stop signal.~%")))

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
                     (loop while (not *stop-server*) do (server-tick server))
                     (setf *peers* ()))))))))))
(export 'start-server)

(defun stop-server ()
  (when (and *server-thread* (bt:thread-alive-p *server-thread*))
    (format t "Sending stop signal to server thread...~%")
    (chanl:send *server-channel* t :blockp t)))
(export 'stop-server)

(defvar *client-channel* (make-instance 'chanl:unbounded-channel))
(defvar *client-thread* nil)
(defvar *stop-client* nil)

(defun client-tick (client)
  (enet:with-host-service (event client 1000)
                          (enet:handle-event event
                            (:connect (:event event)
                                      (log-info "Successfully connected. Event: ~A~%" event))))
  (setf *stop-client* (first (list (chanl:recv *client-channel* :blockp nil))))
  (when *stop-client*
    (log-info "Received stop signal.~%")))

(defun start-client ()
  (unless (and *client-thread* (bt:thread-alive-p *client-thread*))
    (setf *client-thread*
          (let ((o *standard-output*))
            (bt:make-thread
             (lambda ()
               (let ((*side* "Client"))
                 (setf *standard-output* o)
                 (enet:with-client (client)
                   (log-info "Attempting to connect.~%")
                   (enet:with-connect (peer client :host "127.0.0.1" :port +port+)
                     (let ((*stop-client* nil))
                       (loop while (not *stop-client*) do (client-tick client))))))))))))
(export 'start-client)

(defun stop-client ()
  (when (and *client-thread* (bt:thread-alive-p *client-thread*))
    (format t "Sending stop signal to client thread...~%")
    (chanl:send *client-channel* t :blockp t)))
(export 'stop-client)