
(load-modules "core/util"
              "core/command"
              "core/shell"
              "core/game")

(lmud.int:open-v4 "0.0.0.0" 4242 #'game::start-from-telnet "telnet")
(lmud.int:open-v6 "::1"     4244 #'game::start-from-telnet "telnet")