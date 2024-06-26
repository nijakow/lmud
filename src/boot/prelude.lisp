
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
      (lmud.int::%compile (macroexpand expression))))

(set-symbol-function 'eval
   (lambda (expression)
      (funcall (compile expression))))

(set-symbol-macro 'defun
   (lambda (name args &rest body)
      (list 'set-symbol-function (list 'quote name)
            (list 'lambda args (list* 'block name body)))))

(set-symbol-macro 'defmacro
   (lambda (name args &rest body)
      (list 'set-symbol-macro (list 'quote name)
            (list 'lambda args (list* 'block name body)))))

(map1 #'eval '(
   (defmacro return (&rest args)
      (list* 'return-from 'nil args))

   (defun caar (e) (car (car e)))
   (defun cadr (e) (car (cdr e)))
   (defun cdar (e) (cdr (car e)))
   (defun cddr (e) (cdr (cdr e)))
   (defun caaar (e) (car (caar e)))
   (defun caadr (e) (car (cadr e)))
   (defun cadar (e) (car (cdar e)))
   (defun caddr (e) (car (cddr e)))
   (defun cdaar (e) (cdr (caar e)))
   (defun cdadr (e) (cdr (cadr e)))
   (defun cddar (e) (cdr (cdar e)))
   (defun cdddr (e) (cdr (cddr e)))

   (defun first   (e)  (car e))
   (defun second  (e)  (car (cdr e)))
   (defun third   (e)  (car (cdr (cdr e))))
   (defun fourth  (e)  (car (cdr (cdr (cdr e)))))
   (defun fifth   (e)  (car (cdr (cdr (cdr (cdr e))))))
   (defun sixth   (e)  (car (cdr (cdr (cdr (cdr (cdr e)))))))
   (defun seventh (e)  (car (cdr (cdr (cdr (cdr (cdr (cdr e))))))))
   (defun eighth  (e)  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr e)))))))))
   (defun ninth   (e)  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr e))))))))))
   (defun tenth   (e)  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr e)))))))))))
   (defun rest    (e)  (cdr e))

   (defun null (e) (eq e nil))
   (defun not  (e) (if e nil t))

   (defun mapcar (function list &rest lists)
      (if (not (consp list))
          nil
          (if (null lists)
              (cons (funcall function (car list))
                    (mapcar  function (cdr list)))
              (cons (apply function (car list) (mapcar #'car lists))
                    (apply #'mapcar function (cdr list) (mapcar #'cdr lists))))))

   (defmacro when (test &rest body)
      (list 'if test (cons 'progn body) nil))
   
   (defmacro unless (test &rest body)
      (list 'if test nil (cons 'progn body)))

   (defmacro until (test &rest body)
      (list* 'while (list 'not test) body))

   (defmacro cond (&rest clauses)
      (if clauses
          (list 'if
                (caar clauses)
                (cons 'progn (cdar clauses))
                (cons 'cond (cdr clauses)))
          nil))
   
   (defmacro and (&rest args)
      (if args
          (if (cdr args)
              (list 'if (car args) (cons 'and (cdr args)) nil)
              (car args))
          t))
   
   (defmacro or (&rest args)
      (if args
          (if (cdr args)
              (let ((temp (gensym)))
                 (list 'let (list (list temp (car args)))
                    (list 'if temp temp (cons 'or (cdr args)))))
              (car args))
          nil))

   (defmacro dotimes (info &rest body)
      (let ((var  (car  info))
            (n    (cadr info))
            (nv   (gensym)))
         (list 'let (list (list var 0)
                          (list nv  n))
            (list 'until (list '= var nv)
               (list* 'progn body)
               (list 'setq var (list '+ var 1))))))

   (defmacro dolist (info &rest body)
      (let ((var  (gensym))
            (list (cadr info)))
         (list 'let (list (list var list))
            (list 'while (list 'consp var)
               (list* 'let (list (list (car info) (list 'car var))) body)
               (list 'setq var (list 'cdr var))))))
   
   (defmacro domap (info &rest body)
      (let ((var  (car  info))
            (list (cadr info)))
         (list 'mapcar (list* 'lambda (list var) body) list)))
   
   (defun member (item list)
      (cond ((null list) nil)
            ((eq item (car list)) list)
            (t (member item (cdr list)))))
   
   (defun assoc (item alist)
      (dolist (pair alist)
         (if (eq item (car pair))
             (return pair)))
      nil)
   
   (defun get (symbol property)
      (let ((plist (symbol-plist symbol)))
         (while (consp plist)
            (if (eq property (car plist))
                (return (cadr plist))
                (setq plist (cddr plist))))))
   
   (defun put (symbol property value)
      (let ((plist (symbol-plist symbol)))
         (while (consp plist)
            (if (eq property (car plist))
                (progn (rplaca (cdr plist) value)
                       (return))
                (setq plist (cddr plist))))
         (set-symbol-plist symbol (list* property value (symbol-plist symbol)))))

   (defun char=  (a b) (= (char-code a) (char-code b)))
   (defun char<  (a b) (< (char-code a) (char-code b)))
   (defun char>  (a b) (> (char-code a) (char-code b)))
   (defun char<= (a b) (<= (char-code a) (char-code b)))
   (defun char>= (a b) (>= (char-code a) (char-code b)))

   (defun sequence->list (sequence)
      (let ((list '())
            (len  (length sequence)))
         (dotimes (i len)
            (setq list (cons (aref sequence (- len i 1)) list)))
         list))
   
   (defun list->vector (list)
      (apply #'vector list))
   
   (defun list->string (list)
      (apply #'string list))
   
   (defun string->list (string)
      (sequence->list string))
   
   (defun sequence->string (sequence)
      (list->string (sequence->list sequence)))
   
   (defun string->vector (string)
      (list->vector (string->list string)))

   (defun lmud.bootstrap::repl ()
      (while t
         (lmud.dummy::%princ "⍝ ")
         (let ((expr (lmud.dummy::%read)))
            (dolist (e (multiple-value-list (eval expr)))
               (lmud.dummy::%princ "  ")
               (lmud.dummy::%prin1 e)
               (lmud.dummy::%terpri)))))

   (lmud.bootstrap::repl)

))
