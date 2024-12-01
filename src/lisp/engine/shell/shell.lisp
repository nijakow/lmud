
(defun shell:huh? ()
   (let ((phrase (random:pick "I don't understand."
                              "Huh?"
                              "What?"
                              "Pardon?"
                              "No worky."
                              "Say what?")))
      (tell (:p phrase))))

(defun shell:loop (&optional (player-object (game:current-player)))
   (while t
      (fresh-line)
      (let* ((input   (game:split-input-chars (readline:read-line :prompt "> ")))
             (command (first input)))
         (cond ((null input) nil)
               ((string= command "quit" :key #'char-upcase) (return))
               ((string= command "repl" :key #'char-upcase)
                (%signal-handler (e)
                     (repl:repl)
                  (format t "~&An error occurred!~%")))
               (t (multiple-value-bind (command bindings)
                     (game:find-command input)
                  (if command
                      (game:run-command command bindings)
                      (shell:huh?))))))))
