
(load-kernel-modules
   "util"
   "engine/io/telnet"
   "engine/io/vt100"
   "engine/io/tell"
   "engine/objects/name"
   "engine/objects/objects"
   "engine/objects/rooms"
   "engine/describe"
   "shell/cmd/command"
   "shell/cmd/commands"
   "shell/repl"
   "shell/shell"
   "game")

(lmud.int:open-v4 "0.0.0.0" 4242 #'game::start-from-telnet "telnet")
(lmud.int:open-v6 "::1"     4244 #'game::start-from-telnet "telnet")