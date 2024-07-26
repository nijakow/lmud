
(defun game:banner ()
   (fresh-line)
   (terpri)
   (format t "Welcome to the MUD!~%")
   (format t "Type 'quit' to exit.~%")
   (terpri))

(defun game:start ()
   (game:banner)
   (shell:loop nil))

(defun game:start-from-telnet (port)
   (lmud.int:set-current-port port)
   (%signal-handler (e)
         (game:start)
      (format port "~&An error occurred!~%" e))
   (format port "~&Goodbye! :)~%")
   (close port))
