
(load-modules
   "kernel/modules/sourcer/sourcer"  ; Load the sourcer first
   "kernel/util"
   "kernel/io/streams/async"
   "kernel/io/streams/string"
   "kernel/io/readline"
   "kernel/io/telnet"
   "kernel/io/vt100"
   "kernel/io/tell"
   "kernel/modules/repl/genius"
   "kernel/modules/repl/repl"
   "engine/world/objects/name"
   "engine/world/objects/objects"
   "engine/world/objects/rooms"
   "engine/world/describe"
   "engine/world/movement"
   "engine/shell/cmd/command"
   "engine/shell/cmd/commands"
   "engine/shell/shell"
   "engine/game")

(lmud.int:open-v4 "0.0.0.0" 4242 #'game::start-from-telnet "telnet")
(lmud.int:open-v6 "::1"     4244 #'game::start-from-telnet "telnet")
