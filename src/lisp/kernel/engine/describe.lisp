
(defun describe-location (location)
   (tell (:p)
         (:p ((:color :cyan) (:lisp (short-name location)))))
   (.describe location))
