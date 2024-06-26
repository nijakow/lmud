
(set-symbol-function 'lmud.util:map1
   (lambda (function list)
      (if list
          (cons (funcall function (car list))
                (lmud.util:map1 function (cdr list))))))

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
                    (lmud.util:map1 #'macroexpand expression))))
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

(lmud.util:map1 #'eval '(
   (defmacro return (&rest args)
      (list* 'return-from 'nil args))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Predicates and Primitives
   ;;;

   (defun null (e) (eq e nil))
   (defun not  (e) (if e nil t))
   (defun endp (e) (null e))
   (defun atom (e) (not (consp e)))

   (defun identity (e) e)


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Car, Cdr and Sequences
   ;;;

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

   (defun last (list)
      (if (endp (cdr list))
          (car list)
          (last (cdr list))))
   
   (defun butlast (list)
      (if (endp (cdr list))
          nil
          (cons (car list) (butlast (cdr list)))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Block Control Structures
   ;;;

   (defmacro prog1 (expr &rest body)
      (let ((temp (gensym)))
         (list 'let (list (list temp expr))
            (cons 'progn body)
            temp)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Branching Control Structures
   ;;;

   (defmacro when (test &rest body)
      (list 'if test (cons 'progn body) nil))
   
   (defmacro unless (test &rest body)
      (list 'if test nil (cons 'progn body)))

   (defmacro cond (&rest clauses)
      (if clauses
          (list 'if
                (caar clauses)
                (cons 'progn (cdar clauses))
                (cons 'cond (cdr clauses)))
          nil))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Logical Control Structures
   ;;;

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


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Iteration and Mapping
   ;;;

   (defmacro until (test &rest body)
      (list* 'while (list 'not test) body))

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


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    List Operations
   ;;;

   (defun member (item list)
      (cond ((null list) nil)
            ((eq item (car list)) list)
            (t (member item (cdr list)))))
   
   (defun union (list1 list2)
      (cond ((null list1) list2)
            ((member (car list1) list2) (union (cdr list1) list2))
            (t (cons (car list1) (union (cdr list1) list2)))))
   
   (defun intersection (list1 list2)
      (cond ((null list1) nil)
            ((member (car list1) list2) (cons (car list1) (intersection (cdr list1) list2)))
            (t (intersection (cdr list1) list2))))
   
   (defun reduce (function sequence &key (from-end nil)
                                         (initial-value (if from-end
                                                            (prog1 (last sequence) (setq sequence (butlast sequence)))
                                                            (prog1 (car sequence) (setq sequence (cdr sequence))))))
      (if from-end
          (if (endp sequence)
              initial-value
               (funcall function
                        (car sequence)
                        (reduce function (cdr sequence) :initial-value initial-value :from-end t)))
          (let ((result initial-value))
             (dolist (item sequence)
                (setq result (funcall function result item)))
             result)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    List Iteration, Filtering and Mapping
   ;;;

   (defun mapcar (function list &rest lists)
      (if (endp list)
          nil
          (if (null lists)
              (cons (funcall function (car list))
                    (mapcar  function (cdr list)))
              (cons (apply function (car list) (mapcar #'car lists))
                    (apply #'mapcar function (cdr list) (mapcar #'cdr lists))))))
   
   (defun remove-if (predicate list)
      (cond ((endp list) nil)
            ((funcall predicate (car list))
             (remove-if predicate (cdr list)))
            (t (cons (car list) (remove-if predicate (cdr list))))))
   
   (defun remove-if-not (predicate list)
      (cond ((endp list) nil)
            ((funcall predicate (car list))
             (cons (car list) (remove-if-not predicate (cdr list))))
            (t (remove-if-not predicate (cdr list)))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Association Lists and Property Lists
   ;;;

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


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Characters
   ;;;

   (defun char=  (a b) (= (char-code a) (char-code b)))
   (defun char<  (a b) (< (char-code a) (char-code b)))
   (defun char>  (a b) (> (char-code a) (char-code b)))
   (defun char<= (a b) (<= (char-code a) (char-code b)))
   (defun char>= (a b) (>= (char-code a) (char-code b)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Non-standard conversions
   ;;;

   (defun conversions:sequence->list (sequence)
      (let ((list '())
            (len  (length sequence)))
         (dotimes (i len)
            (setq list (cons (aref sequence (- len i 1)) list)))
         list))
   
   (defun conversions:list->vector (list)
      (apply #'vector list))
   
   (defun conversions:list->string (list)
      (apply #'string list))
   
   (defun conversions:string->list (string)
      (conversions:sequence->list string))
   
   (defun conversions:sequence->string (sequence)
      (conversions:list->string (conversions:sequence->list sequence)))
   
   (defun conversions:string->vector (string)
      (conversions:list->vector (conversions:string->list string)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    REPL
   ;;;

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
