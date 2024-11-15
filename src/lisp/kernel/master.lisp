
(load-kernel-modules
   "util"
   "engine/io/tell"
   "engine/objects/objects"
   "engine/objects/rooms"
   "core/command"
   "core/commands"
   "shell/repl"
   "shell/shell"
   "game")

(lmud.int:open-v4 "0.0.0.0" 4242 #'game::start-from-telnet "telnet")
(lmud.int:open-v6 "::1"     4244 #'game::start-from-telnet "telnet")
