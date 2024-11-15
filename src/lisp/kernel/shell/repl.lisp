
(defun repl::banner ()
   (format t "~&~%Welcome to the LMud REPL!~%"))

(defun repl::safely-evaluate (expression)
   (%signal-handler (e)
         (eval expression)
      (format t "~&Error: ~a~%" e)
      (values)))

(defun repl::repl ()
   (repl::banner)
   (while t
      (princ ". ")
      (let ((expr (read)))
         (when (eq expr :q)
            (return))
         (let ((results (multiple-value-list (repl::safely-evaluate expr))))
            (dolist (e results)
               (format t "  ~a~%" e))))))
