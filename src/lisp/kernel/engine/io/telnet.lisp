
(defun telnet:disable-echo (&optional (stream (io:default-stream)))
  (.write-byte stream 255)
  (.write-byte stream 251)
  (.write-byte stream 1)
  (.flush      stream))
