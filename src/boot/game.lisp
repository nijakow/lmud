
(defun game:punctuationp (char)
   (member char '(#\. #\, #\! #\? #\; #\:)))

(defun game:split-input-chars (chars)
   (setq chars (conversions:->list chars))
   (let ((current-word '())
         (words        '()))
      (until (endp chars)
         (let ((char (pop chars)))
            (cond ((lmud.char:whitespacep char)
                   (when current-word
                      (push (conversions:->string (nreverse current-word)) words)
                      (setq current-word '())))
                  (t (if (game:punctuationp char)
                         (progn (when current-word
                                   (push (conversions:->string (nreverse current-word)) words)
                                   (setq current-word '()))
                                (push (string char) words))
                         (push char current-word))))))
      (when current-word
         (push (conversions:->string (nreverse current-word)) words))
      (nreverse words)))

(defun game:loop (player-object)
   (while t
      (princ "> ")
      (let ((command (game:split-input-chars (read-line))))
         (cond ((null command) nil)
               ((string= (first command) "quit" :key #'char-upcase) (return))
               (t (princ "I don't understand that command.")
                  (terpri))))))

(defun game:banner ()
   (fresh-line)
   (terpri)
   (io:uformat t "Welcome to the MUD!~%")
   (io:uformat t "Type 'quit' to exit.~%")
   (terpri))

(defun game:start ()
   (game:banner)
   (game:loop nil))

(defun game:start-from-telnet (port)
   (lmud.int:set-current-port port)
   (%signal-handler (e)
         (game:start)
      (io:uformat port "~&An error occurred!~%" e))
   (io:uformat port "~&Goodbye! :)~%")
   (close port))
