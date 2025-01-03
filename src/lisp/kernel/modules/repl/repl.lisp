
(defun repl::banner ()
   (format t "~&~%Welcome to the LMud REPL!~%"))

(defun repl::safely-evaluate (expression)
   (%signal-handler (e)
         (eval expression)
      (format t "~&Error: ~a~%" e)
      (values)))

(defun repl::read ()
   (read (io:make-string-stream (genius:read-line))))

(defun repl::repl ()
   ;; (repl::banner)
   (while t
      (let ((expr (repl::read)))
         (when (or (eq expr :q)
                   (and (consp expr)
                        (eq (car expr) 'quit)))
            (return))
         (let ((results (multiple-value-list (repl::safely-evaluate expr))))
            (dolist (e results)
               (format t "~&  ~s~%" e))))))
