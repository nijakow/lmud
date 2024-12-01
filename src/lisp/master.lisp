
(load-modules
   "kernel/util"
   "kernel/io/streams/async"
   "kernel/io/streams/string"
   "kernel/io/readline"
   "kernel/io/telnet"
   "kernel/io/vt100"
   "kernel/io/tell"
   "kernel/repl/genius"
   "kernel/repl/repl"
   "engine/objects/name"
   "engine/objects/objects"
   "engine/objects/rooms"
   "engine/describe"
   "engine/movement"
   "engine/shell/cmd/command"
   "engine/shell/cmd/commands"
   "engine/shell/shell"
   "engine/game")

(lmud.int:open-v4 "0.0.0.0" 4242 #'game::start-from-telnet "telnet")
(lmud.int:open-v6 "::1"     4244 #'game::start-from-telnet "telnet")
