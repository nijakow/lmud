
(set-symbol-function 'map1
   (lambda (function list)
      (if list
          (cons (funcall function (car list))
                (map1 function (cdr list))))))

(set-symbol-function 'macroexpand
   (lambda (expression)
      (if (consp expression)
          (let ((head (car expression))
                (args (cdr expression)))
            (if (eq head 'quote)
                expression
                (if (if (symbolp head)
                        (if (symbol-macro head) t))
                    (macroexpand (apply (symbol-macro head) args))
                    (map1 #'macroexpand expression))))
           expression)))

(set-symbol-function 'compile
   (lambda (expression)
      (%compile (macroexpand expression))))

(set-symbol-function 'eval
   (lambda (expression)
      (funcall (compile expression))))

(set-symbol-macro 'defun
   (lambda (name args &rest body)
      (list 'set-symbol-function (list 'quote name)
            (cons 'lambda (cons args body)))))

(set-symbol-macro 'defmacro
   (lambda (name args &rest body)
      (list 'set-symbol-macro (list 'quote name)
            (cons 'lambda (cons args body)))))

(eval '(progn

   (defun repl ()
      (while t
         (%princ "> ")
         (let ((expr (%read)))
            (%princ "  ")
            (%prin1 (eval expr)))
         (%terpri)))

   (repl)

))
