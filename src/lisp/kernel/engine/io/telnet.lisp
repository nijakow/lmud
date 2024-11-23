
(tos:defclass telnet:<telnet-port> (io:<wrapped-port-stream>))

(tos:defmethod (telnet:<telnet-port> construct) (port)
   (tos:xsend io:<wrapped-port-stream> self 'construct port))

(tos:defmethod (telnet:<telnet-port> telnet::basic-read-byte) ()
   (tos:xsend io:<wrapped-port-stream> self 'read-byte))

(tos:defmethod (telnet:<telnet-port> telnet::basic-write-byte) (byte)
   (tos:xsend io:<wrapped-port-stream> self 'write-byte byte))

(tos:defmethod (telnet:<telnet-port> read-byte) ()
   (.telnet::basic-read-byte self))

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
   (.write-byte stream 255)
   (.write-byte stream 251)
   (.write-byte stream 1)
   (.flush      stream))
