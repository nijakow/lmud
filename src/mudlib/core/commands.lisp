
(game:defcommand :hello ("hello")
   (tell (:p "Hello!")))

(game:defcommand :pick-up ("pick" "up" item)
   (tell (:p "You pick up " item)))
