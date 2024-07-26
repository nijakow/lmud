
(defun game:banner ()
   (fresh-line)
   (terpri)
   (io:uformat t "Welcome to the MUD!~%")
   (io:uformat t "Type 'quit' to exit.~%")
   (terpri))

(defun game:start ()
   (game:banner)
   (shell:loop nil))

(defun game:start-from-telnet (port)
   (lmud.int:set-current-port port)
   (%signal-handler (e)
         (game:start)
      (io:uformat port "~&An error occurred!~%" e))
   (io:uformat port "~&Goodbye! :)~%")
   (close port))
