
(game:defcommand :clear ("clear")
   ;; Full clear of the screen
   (vt100:clear-screen))

(game:defcommand :help-commands ("help" topic)
   (cond ((string= (car topic) "commands")
          (tell (:p ((:color :green) "Commands"))
                (:p "  " ((:color :cyan) "help ") " - View this help")
                (:p "  " ((:color :cyan) "clear") " - Clear the screen")
                (:p "  " ((:color :cyan) "look ") " - Look around you")))
         (t (tell (:p (:italics "No help available for topic: ") (:bold (:lisp (car topic))))))))

(game:defcommand :help ("help")
   (tell (:p ((:color :green) "Help"))
         (:p "  " ((:color :cyan) "help") " " ((:color :green) "commands") " - View available commands")
         (:p "  " ((:color :cyan) "help") " " ((:color :green) "<topic> ") " - View help for a specific topic")))

(game:defcommand :look ("look")
   (let ((environment (environment (lmud.int:player))))
      (describe-location environment)))
