
(tos:defclass repl::<custom-repl-reporter> (io.reader:<reporter>))

(tos:defmethod (repl::<custom-repl-reporter> report-data) (data &key from to)
   (lmud.dummy:%princ "From: ")
   (lmud.dummy:%prin1 from)
   (lmud.dummy:%princ " To: ")
   (lmud.dummy:%prin1 to)
   (lmud.dummy:%princ " Data: ")
   (lmud.dummy:%prin1 data)
   (lmud.dummy:%princ " / ")
   (lmud.dummy:%prin1 (car .openings))
   (lmud.dummy:%terpri))

(defun repl::make-repl-reporter ()
   (tos:make-instance repl::<custom-repl-reporter>))


(defun repl::banner ()
   (format t "~&~%Welcome to the LMud REPL!~%"))

(defun repl::safely-evaluate (expression)
   (%signal-handler (e)
         (eval expression)
      (format t "~&Error: ~a~%" e)
      (values)))

(defun repl::read ()
   (read (io:default-stream) :reporter (repl::make-repl-reporter)))

(defun repl::repl ()
   (repl::banner)
   (while t
      (princ ". ")
      (let ((expr (repl::read)))
         (when (or (eq expr :q)
                   (and (consp expr)
                        (eq (car expr) 'quit)))
            (return))
         (let ((results (multiple-value-list (repl::safely-evaluate expr))))
            (dolist (e results)
               (format t "~&  ~s~%" e))))))
