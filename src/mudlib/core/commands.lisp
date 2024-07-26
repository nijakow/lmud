
(game:defcommand :hello ("hello")
   (tell "Hello!" cr))

(game:defcommand :pick-up ("pick" "up" item)
   (tell "You pick up " item cr))
