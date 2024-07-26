
(defun shell:huh? ()
   (let ((phrase (random:pick "I don't understand."
                              "Huh?"
                              "What?"
                              "Pardon?"
                              "No worky."
                              "Say what?")))
      (tell phrase cr)))

(defun shell:loop (player-object)
   (while t
      (princ "> ")
      (let* ((input   (game:split-input-chars (read-line)))
             (command (first input)))
         (cond ((null input) nil)
               ((string= command "quit" :key #'char-upcase) (return))
               ((string= command "repl" :key #'char-upcase)
                (%signal-handler (e)
                     (lmud.bootstrap::repl (lmud.int:current-port))
                  (format t "~&An error occurred!~%")))
               (t (multiple-value-bind (command bindings)
                     (game:find-command input)
                  (if command
                      (game:run-command command bindings)
                      (shell:huh?))))))))
