
(tos:defclass io:<string-stream> (io:<basic-stream>)
   (with (data     "")
         (position 0)))

(tos:defmethod (io:<string-stream> construct) (string)
   (setf .data string)
   self)

(tos:defmethod (io:<string-stream> read-char) ()
   (unless (.eof-p self)
      (.io:advance-text-position self 1)
      (let ((char (aref .data .position)))
         (incf .position)
         char)))

(tos:defmethod (io:<string-stream> unread-char) (char)
   ;; TODO: Compare the pushback character to the last character in the string
   (.io:advance-text-position self -1)
   (decf .position)
   char)

(tos:defmethod (io:<string-stream> eof-p) ()
   (>= .position (length .data)))

(defun io:make-string-stream (string)
   (.construct (tos:make-instance io:<string-stream>) string))
