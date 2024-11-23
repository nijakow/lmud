
(defun vt100:clear-screen (&optional (stream (io:default-stream)))
   (princ "\e[2J\e[1;1H" stream))

(defun vt100:move-cursor (x y &optional (stream (io:default-stream)))
   (format stream "\e[~a;~aH" y x))

;; (defun vt100:get-cursor-position (&optional (stream (io:default-stream)))
;;    (princ "\e[6n" stream)
;;    (while (not (char= (read-char stream) #\[)))
;;    (format stream "~%~%The character is: ~s~%~%" (read-char stream))
;;    (format stream "~%~%The character is: ~s~%~%" (read-char stream))
;;    (format stream "~%~%The character is: ~s~%~%" (read-char stream)))
