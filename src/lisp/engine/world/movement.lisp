
(defun go-direction (direction &optional (object (game:current-player)))
   (if (null (environment object))
      (tell (:p "You are nowhere and can't go anywhere."))
      (let ((location (direction (environment object) direction)))
         (if location
             (move object location)
             (tell (:p "You can't go that way."))))))
