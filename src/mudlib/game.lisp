
(defvar game:*commands* '())


(defmacro random:pick (&body body)
   (let ((count (length body))
         (index 0))
      `(case (random ,count)
          ,@(domap (form body)
               (prog1 `((,index) ,form)
                  (incf index))))))


(defmacro game:defcommand (id pattern &body body)
   (flet ((extract-args (pattern)
            (remove-if-not #'symbolp pattern))
          (create-alist-let (bindings-var vars body)
            `(let (,@(mapcar (lambda (var)
                               `(,var (cdr (assoc ',var ,bindings-var))))
                            vars))
               ,@body)))
      (let ((vars (extract-args pattern))
            (bindings-var (gensym)))
         `(progn
             (push (cons (cons ',id ',pattern)
                         (lambda (,bindings-var)
                           ,(create-alist-let bindings-var vars body)))
                   game:*commands*)))))


(game:defcommand :hello ("hello")
   (io:uformat t "Hello!~%"))

(game:defcommand :pick-up ("pick" "up" item)
   (io:uformat t "You pick up ~a.~%" item))


(defun game:compare-pattern-elements (input-element pattern-element)
   (string= input-element pattern-element :key #'char-upcase))

(defun game:match-basic-pattern (input pattern bindings-pass)
   (cond ((endp pattern)
          (if (endp input)
              (values t bindings-pass)
              (values nil nil)))
         ((endp input)
          (values nil nil))
         ((game:compare-pattern-elements (car input) (car pattern))
          (game:match-basic-pattern (cdr input) (cdr pattern) bindings-pass))
         ((symbolp (car pattern))
          (if (endp (cdr pattern))
              (values t (cons (cons (car pattern) input) bindings-pass))
              (multiple-value-bind (match-p bindings)
                  (game:match-basic-pattern (cdr input) (cdr pattern) bindings-pass)
                (if match-p
                    (let ((binding (assoc (car pattern) bindings)))
                       (if binding
                           (setf (cdr binding) (cons (car input) (cdr binding)))
                           (push (cons (car pattern) (list (car input))) bindings))
                       (values t bindings))
                    (values nil nil)))))
         (t (values nil nil))))

(defun game:find-command (input)
   (dolist (command game:*commands*)
      (multiple-value-bind (match-p bindings)
            (game:match-basic-pattern input (cdar command) nil)
         (if match-p
            (return (values command bindings))))))


(defun game:run-command (command bindings)
   (funcall (cadr command) bindings))

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

(defun game:huh? ()
   (io:uformat t "~a~%"
      (random:pick "I don't understand."
                   "Huh?"
                   "What?"
                   "Pardon?"
                   "No worky."
                   "Say what?")))

(defun game:loop (player-object)
   (while t
      (princ "> ")
      (let* ((input   (game:split-input-chars (read-line)))
             (command (first input)))
         (cond ((null input) nil)
               ((string= command "quit" :key #'char-upcase) (return))
               ((string= command "repl" :key #'char-upcase)
                (%signal-handler (e)
                     (lmud.bootstrap::repl (lmud.int:current-port))
                  (io:uformat t "~&An error occurred!~%")))
               (t (multiple-value-bind (command bindings)
                     (game:find-command input)
                  (if command
                      (game:run-command command bindings)
                      (game:huh?))))))))

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
