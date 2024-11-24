
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
   (describe-location))

(game:defcommand :abbrev-look ("l") (describe-location))

(game:defcommand :go ("go" where)
   (flet ((translate-direction (direction)
            (cond ((or (string= direction "north")
                       (string= direction "n"))
                   :north)
                  ((or (string= direction "south")
                       (string= direction "s"))
                   :south)
                  ((or (string= direction "east")
                       (string= direction "e"))
                   :east)
                  ((or (string= direction "west")
                       (string= direction "w"))
                   :west)
                  ((or (string= direction "northeast")
                       (string= direction "ne"))
                   :northeast)
                  ((or (string= direction "northwest")
                       (string= direction "nw"))
                   :northwest)
                  ((or (string= direction "southeast")
                       (string= direction "se"))
                   :southeast)
                  ((or (string= direction "southwest")
                       (string= direction "sw"))
                   :southwest)
                  ((or (string= direction "up")
                       (string= direction "u"))
                   :up)
                  ((or (string= direction "down")
                       (string= direction "d"))
                   :down))))
      (let ((translated-direction (translate-direction (car where))))
         (if translated-direction
             (go-direction translated-direction)
             (tell (:p "Invalid direction."))))))

(game:defcommand :north ("north") (go-direction :north))
(game:defcommand :south ("south") (go-direction :south))
(game:defcommand :east  ("east")  (go-direction :east))
(game:defcommand :west  ("west")  (go-direction :west))
(game:defcommand :northeast ("northeast") (go-direction :northeast))
(game:defcommand :northwest ("northwest") (go-direction :northwest))
(game:defcommand :southeast ("southeast") (go-direction :southeast))
(game:defcommand :southwest ("southwest") (go-direction :southwest))
(game:defcommand :up   ("up")   (go-direction :up))
(game:defcommand :down ("down") (go-direction :down))

(game:defcommand :abbrev-north ("n") (go-direction :north))
(game:defcommand :abbrev-south ("s") (go-direction :south))
(game:defcommand :abbrev-east  ("e") (go-direction :east))
(game:defcommand :abbrev-west  ("w") (go-direction :west))
(game:defcommand :abbrev-northeast ("ne") (go-direction :northeast))
(game:defcommand :abbrev-northwest ("nw") (go-direction :northwest))
(game:defcommand :abbrev-southeast ("se") (go-direction :southeast))
(game:defcommand :abbrev-southwest ("sw") (go-direction :southwest))
(game:defcommand :abbrev-up   ("u") (go-direction :up))
(game:defcommand :abbrev-down ("d") (go-direction :down))
