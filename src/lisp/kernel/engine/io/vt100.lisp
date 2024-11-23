
(defun vt100:clear-screen (&optional (stream (io:default-stream)))
   (princ "\e[2J\e[1;1H" stream))

(defun vt100:move-cursor (x y &optional (stream (io:default-stream)))
   (format stream "\e[~a;~aH" y x))

(defun vt100:get-cursor-position (&optional (stream (io:default-stream)))
   (flet ((subtract-one-if-number (n) (if n (- n 1) n)))
      (.disable-echo stream)
      (princ "\e[6n" stream)
      (io.reader:read-until-char stream #\[)
      (let ((first-number  (io.reader:read-until-char stream #\;))
            (second-number (io.reader:read-until-char stream #\R)))
         (.enable-echo stream)
         (values (subtract-one-if-number (io:parse-number second-number))
                 (subtract-one-if-number (io:parse-number first-number))))))
