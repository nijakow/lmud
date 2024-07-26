
(defparameter cr (string #\Return #\Newline))

(defun tell (&ignore-rest)
   (lmud.int:funcall-forward #'tell-on (lmud.int:current-port)))

(defun tell-on (stream &rest messages)
   (dolist (message messages)
      (princ message stream)))
