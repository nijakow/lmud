
(tos:defclass telnet:<telnet-port> (io:<wrapped-port-stream>))

(tos:defmethod (telnet:<telnet-port> construct) (port)
   (tos:xsend io:<wrapped-port-stream> self 'construct port))

(tos:defmethod (telnet:<telnet-port> telnet::basic-read-byte) ()
   (tos:xsend io:<wrapped-port-stream> self 'read-byte))

(tos:defmethod (telnet:<telnet-port> telnet::basic-write-byte) (byte)
   (tos:xsend io:<wrapped-port-stream> self 'write-byte byte))


(tos:defmethod (telnet:<telnet-port> telnet::send-will) (option)
   (.telnet::basic-write-byte self 255)
   (.telnet::basic-write-byte self 251)
   (.telnet::basic-write-byte self option)
   (.flush self))

(tos:defmethod (telnet:<telnet-port> telnet::send-wont) (option)
   (.telnet::basic-write-byte self 255)
   (.telnet::basic-write-byte self 252)
   (.telnet::basic-write-byte self option)
   (.flush self))

(tos:defmethod (telnet:<telnet-port> telnet::send-do) (option)
   (.telnet::basic-write-byte self 255)
   (.telnet::basic-write-byte self 253)
   (.telnet::basic-write-byte self option)
   (.flush self))

(tos:defmethod (telnet:<telnet-port> telnet::send-dont) (option)
   (.telnet::basic-write-byte self 255)
   (.telnet::basic-write-byte self 254)
   (.telnet::basic-write-byte self option)
   (.flush self))


(tos:defmethod (telnet:<telnet-port> telnet::begin-receive-sub-negotiation) ()
   ;; Only read in the data - don't do anything with it
   (let ((command (.telnet::basic-read-byte self))
         (option  (.telnet::basic-read-byte self)))
      nil))

(tos:defmethod (telnet:<telnet-port> telnet::begin-receive-will) ()
   (let ((option (.telnet::basic-read-byte self)))
      (.telnet::send-wont self option)))

(tos:defmethod (telnet:<telnet-port> telnet::begin-receive-wont) ()
   (let ((option (.telnet::basic-read-byte self)))
      (.telnet::send-will self option)))

(tos:defmethod (telnet:<telnet-port> telnet::begin-receive-do) ()
   (let ((option (.telnet::basic-read-byte self)))
      (.telnet::send-will self option)))

(tos:defmethod (telnet:<telnet-port> telnet::begin-receive-dont) ()
   (let ((option (.telnet::basic-read-byte self)))
      (.telnet::send-wont self option)))

(tos:defmethod (telnet:<telnet-port> telnet::begin-receive-command) ()
   (let ((command (.telnet::basic-read-byte self)))
      (case command
         ((250) (.telnet::begin-receive-sub-negotiation self))
         ((251) (.telnet::begin-receive-will            self))
         ((252) (.telnet::begin-receive-wont            self))
         ((253) (.telnet::begin-receive-do              self))
         ((254) (.telnet::begin-receive-dont            self))
         ((255) nil)
         (t (error "Unknown telnet command: ~S" command)))))

(tos:defmethod (telnet:<telnet-port> read-byte) ()
   (let ((byte (.telnet::basic-read-byte self)))
      (if (= byte 255)
          (progn (.telnet::begin-receive-command self)
                 (.read-byte self)) ; This is recursive -- turn this into a loop?
          byte)))

(tos:defmethod (telnet:<telnet-port> write-byte) (byte)
   (if (= byte 255)
       (progn (.telnet::basic-write-byte self 255)
              (.telnet::basic-write-byte self 255))
       (.telnet::basic-write-byte self byte)))

(defun telnet:make-telnet-port (port)
   (let ((tp (tos:make-instance telnet:<telnet-port>)))
      (.construct tp port)
      tp))


(defun telnet:disable-echo (&optional (stream (io:default-stream)))
   (.telnet::send-will stream 1))

(defun telnet:enable-echo (&optional (stream (io:default-stream)))
   (.telnet::send-wont stream 1))
