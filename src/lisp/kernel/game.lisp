
(defun game:current-player ()
   (lmud.int:player))

(defun game:current-location ()
   (environment (game:current-player)))

(defun game:make-player ()
   (let ((player (make <object>)))
      (move player ~the-void)
      player))

(defun game:banner ()
   (fresh-line)
   (terpri)
   (format t "Welcome to the MUD!~%")
   (format t "Type 'quit' to exit.~%")
   (terpri))

(defun game:start ()
   (game:banner)
   (shell:loop))

(defun game:start-from-telnet (port)
   (setq port (telnet:make-telnet-port port))
   (lmud.int:set-current-port port)
   (lmud.int:set-player (game:make-player))
   (%signal-handler (e)
         (game:start)
      (format port "~&An error occurred!~%" e))
   (format port "~&Goodbye! :)~%")
   (close port))
