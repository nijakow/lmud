
(defun telnet::echoable-char-p (char)
   (and (characterp char)
        (not (char= char #\Tab))
        (not (char= char #\Up))
        (not (char= char #\Down))
        (not (char= char #\Right))
        (not (char= char #\Left))
        (not (char= char #\Home))
        (not (char= char #\End))))

(tos:defclass telnet:<telnet-port> (io:<wrapped-port-stream>)
   (with (echoing?  t)
         (pushbacks nil)))

(tos:defmethod (telnet:<telnet-port> construct) (port)
   (tos:xsend io:<wrapped-port-stream> self 'construct port))

(tos:defmethod (telnet:<telnet-port> telnet::basic-read-byte) ()
   (tos:xsend io:<wrapped-port-stream> self 'read-byte))

(tos:defmethod (telnet:<telnet-port> telnet::basic-unread-byte) (byte)
   (tos:xsend io:<wrapped-port-stream> self 'unread-byte byte))

(tos:defmethod (telnet:<telnet-port> telnet::basic-write-byte) (byte)
   (tos:xsend io:<wrapped-port-stream> self 'write-byte byte))

(tos:defmethod (telnet:<telnet-port> telnet::basic-read-char) ()
   (tos:xsend io:<wrapped-port-stream> self 'read-char))

(tos:defmethod (telnet:<telnet-port> telnet::basic-write-char) (char)
   (tos:xsend io:<wrapped-port-stream> self 'write-char char))


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
   ))

(tos:defmethod (telnet:<telnet-port> telnet::begin-receive-wont) ()
   (let ((option (.telnet::basic-read-byte self)))
   ))

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

(tos:defmethod (telnet:<telnet-port> telnet::handle-interrupt) ()
   (lmud.int:signal "Control-C interrupt!"))

(tos:defmethod (telnet:<telnet-port> disable-echo) () (setf .echoing? nil))
(tos:defmethod (telnet:<telnet-port> enable-echo)  () (setf .echoing? t))

(tos:defmethod (telnet:<telnet-port> enable-character-mode) ()
   (.telnet::send-will self 1)
   (.telnet::send-will self 3))

(tos:defmethod (telnet:<telnet-port> read-byte) ()
   (let ((byte (.telnet::basic-read-byte self)))
      (case byte
         ((0)  (.read-byte self)) ; Ignore NULLs
         ((3)  (.telnet::handle-interrupt self)
               (.read-byte self))
         ((13) 10) ; Ignore CRs
         ((255) (.telnet::begin-receive-command self)
                (.read-byte self)) ; This is recursive -- turn this into a loop?
         (t byte))))

(tos:defmethod (telnet:<telnet-port> write-byte) (byte)
   (if (= byte 255)
       (progn (.telnet::basic-write-byte self 255)
              (.telnet::basic-write-byte self 255))
       (.telnet::basic-write-byte self byte)))

(tos:defmethod (telnet:<telnet-port> telnet::read-character-escape-sequence) ()
   (let ((command (.telnet::basic-read-byte self)))
      (case command
         ((65) #\Up)
         ((66) #\Down)
         ((67) #\Right)
         ((68) #\Left)
         ((70) #\End)
         ((72) #\Home)
         (t (.telnet::basic-unread-byte self command)
            (.telnet::basic-unread-byte self 91)
            #\Escape))))

(tos:defmethod (telnet:<telnet-port> telnet::begin-escape-command) ()
   (let ((command (.telnet::basic-read-byte self)))
      (case command
         ((91) (.telnet::read-character-escape-sequence self))
         (t (.telnet::basic-unread-byte self command)
            #\Escape))))

(tos:defmethod (telnet:<telnet-port> telnet::read-char-wrapped) ()
   (let ((char (.telnet::basic-read-char self)))
      (cond ((char= char #\Escape) (.telnet::begin-escape-command self))
            ((char= char #\Delete) #\Backspace)
            (t char))))

(tos:defmethod (telnet:<telnet-port> read-char) ()
   (if .pushbacks
       (progn (.io:advance-text-position self 1)
              (pop .pushbacks))
       (let ((char (.telnet::read-char-wrapped self)))
          (when (and .echoing? (telnet::echoable-char-p char))
             (.write-char self char))
          char)))

(tos:defmethod (telnet:<telnet-port> write-char) (char)
   (cond
      ((char= char #\Newline) (.telnet::basic-write-char self #\Return)
                              (.telnet::basic-write-char self #\Newline))
      (t (.telnet::basic-write-char self char))))

(tos:defmethod (telnet:<telnet-port> unread-char) (char)
   (.io:advance-text-position self -1)
   (push char .pushbacks))

(tos:defmethod (telnet:<telnet-port> eof-p) ()
   (and (null .pushbacks)
        (tos:xsend io:<wrapped-port-stream> self 'eof-p)))

(tos:defmethod (telnet:<telnet-port> cursor-position) ()
   (vt100:get-cursor-position self))

(defun telnet:make-telnet-port (port)
   (let ((tp (tos:make-instance telnet:<telnet-port>)))
      (.construct tp port)
      (.enable-character-mode tp)
      tp))
