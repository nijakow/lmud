
(game:defcommand :hello ("hello")
   (tell (:p "Hello!")))

(game:defcommand :hello ("look")
   (let ((environment (environment (lmud.int:player))))
      (.describe environment)))

(game:defcommand :pick-up ("pick" "up" item)
   (tell (:p "You pick up " item)))
