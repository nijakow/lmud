
(defun describe-items (items indentation)
   (dolist (item items)
      (tell (:p (:lisp (util:timeschar #\Space indentation))
                " - "
                (:object item)))
      (describe-items (children item) (+ indentation 3))))

(defun describe-location (&optional (location (game:current-location)))
   (tell (:p)
         (:p ((:color :cyan) (:lisp (short-name location)))))
   (.describe location)
   (describe-items (children location) 0))
