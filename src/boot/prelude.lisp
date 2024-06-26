
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
      (if (if (consp name) (if (eq (car name) 'setf) t))
          ;; Setf defun
          (progn (setq name (car (cdr name)))
                 (list 'lmud.int:set-setf-expander-function (list 'quote name)
                       (list 'lambda args (list* 'block name body))))
          ;; Normal defun
          (list 'set-symbol-function (list 'quote name)
                (list 'lambda args (list* 'block name body))))))

(set-symbol-macro 'defmacro
   (lambda (name args &rest body)
      (list 'set-symbol-macro (list 'quote name)
            (list 'lambda args (list* 'block name body)))))

(lmud.util:map1 #'eval '(

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Base Macros
   ;;;

   (defmacro return (&rest args)
      (list* 'return-from 'nil args))
   
   (defmacro let* (bindings &rest body)
      (if (endp bindings)
          (cons 'progn body)
          (list 'let (list (car bindings))
             (cons 'let* (cons (cdr bindings) body)))))
   
   (defmacro defvar (name value)
      (list 'set-symbol-value (list 'quote name) value))
   
   (defmacro defparameter (name value)
      (list 'set-symbol-value (list 'quote name) value))
   
   (defmacro defconstant (name value)
      (list 'set-symbol-value (list 'quote name) value))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Error Handling
   ;;;

   (defun lmud.util:simple-error (&optional (message nil))
      (lmud.dummy::%terpri)
      (lmud.dummy::%princ "Error: ")
      (lmud.dummy::%princ message)
      (lmud.dummy::%terpri)
      (lmud.int::%quit))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Predicates and Primitives
   ;;;

   (defun null  (e) (eq e nil))
   (defun not   (e) (if e nil t))
   (defun endp  (e) (null e))
   (defun listp (e) (if (null e) t (consp e)))
   (defun atom  (e) (not (consp e)))

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
   
   (defmacro dosequence (info &rest body)
      (let ((var  (car  info))
            (seq  (gensym))
            (i    (gensym)))
         (list 'let (list (list seq (cadr info)))
            (list 'dotimes (list i (list 'length seq))
               (list* 'let (list (list var (list 'aref seq i))) body)))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    List Operations
   ;;;

   (defun copy-list (list)
      (if (consp list)
          (cons (car list) (copy-list (cdr list)))
          (cdr list)))

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

   (defun lmud.util:append2 (l1 l2)
      (if (endp l1)
          l2
          (cons (car l1) (lmud.util:append2 (cdr l1) l2))))
   
   (defun append (&rest lists)
      (reduce #'lmud.util:append2 lists))


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
   
   (defun conversions:->list (e)
      (cond ((listp e) e)
            ((vectorp e) (conversions:sequence->list e))
            ((stringp e) (conversions:string->list e))
            (t (lmud.util:simple-error "Cannot convert to list!"))))

   (defun conversions:->vector (e)
      (cond ((vectorp e) e)
            ((listp e)   (conversions:list->vector e))
            ((stringp e) (conversions:string->vector e))
            (t (lmud.util:simple-error "Cannot convert to vector!"))))
   
   (defun conversions:->string (e)
      (cond ((stringp e) e)
            ((listp e)   (conversions:list->string e))
            ((vectorp e) (conversions:sequence->string e))
            (t (lmud.util:simple-error "Cannot convert to string!"))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Setf
   ;;;

   (defun lmud.int:get-setf-expander-function (symbol)
      (get symbol 'lmud.int:setf-expander-function))
   
   (defun lmud.int:set-setf-expander-function (symbol function)
      (put symbol 'lmud.int:setf-expander-function function))
   
   (defmacro setf (place value)
      (cond ((consp place)
             (let ((head (car place))
                   (args (cdr place)))
                (if (symbolp head)
                    (if (lmud.int:get-setf-expander-function head)
                        (apply (lmud.int:get-setf-expander-function head) value args)
                        (lmud.util:simple-error "Undefined setf expander!"))
                    (lmud.util:simple-error "Setf expects a symbol!"))))
            (t (list 'setq place value))))

   (defun (setf car) (value cons) (list 'rplaca cons value))
   (defun (setf cdr) (value cons) (list 'rplacd cons value))

   (defmacro push (item place)
      (list 'setf place (list 'cons item place)))
   
   (defmacro pop (place)
      (let ((temp (gensym)))
         (list 'let (list (list temp place))
            (list 'setf place (list 'cdr temp))
            (list 'car temp))))
   
   (defmacro incf (place)
      (list 'setf place (list '+ place 1)))
   
   (defmacro decf (place)
      (list 'setf place (list '- place 1)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    The Type- and Object System
   ;;;

   (defun (setf lmud.int:%custom-meta) (value object)
      (list 'lmud.int:%custom-set-meta object value))
   
   (defun (setf lmud.int:%custom-at) (value object index)
      (list 'lmud.int:%custom-set object index value))

   (defparameter tos.int:<class> (lmud.int:%make-custom nil nil nil nil nil))

   (setf (lmud.int:%custom-meta tos.int:<class>) tos.int:<class>)

   (defun tos.int:default-constructor (class)
      (lmud.int:%make-custom class))

   (defun tos.int:make-class (&key (constructor nil) (superclasses nil) (slots nil))
      (let ((instance (lmud.int:%make-custom tos.int:<class> constructor nil superclasses slots)))
         (tos.int:rebuild-class-layout instance)
         (when constructor
            (setf (lmud.int:%custom-at instance 0) constructor))
         instance))
   
   (defun tos.int:class-instance-slot-index-by-name (class name)
      (let ((allslots (lmud.int:%custom-at class 1)))
         (dotimes (i (length allslots))
            (let ((slot (aref allslots i)))
               (if (eq name (lmud.int:%custom-at slot 0))
                   (return i)))))
      nil)
   
   (defun tos.int:class-of (object)
      (lmud.int:%custom-meta object))

   (defun tos.int:slot-value-by-name (object slot-name)
      (let ((slot-index (tos.int:class-instance-slot-index-by-name (tos.int:class-of object) slot-name)))
         (when (null slot-index)
            (lmud.util:simple-error "Slot not found!"))
         (lmud.int:%custom-at object slot-index)))
   
   (defun tos.int:rebuild-class-layout (class)
      (let* ((layout (conversions:->vector (lmud.int:%custom-at class 3)))
             (slots (lmud.int:%custom-at class 3))
             (constructor-source
               (let ((class-var (gensym)))
                  (list 'lambda (list* class-var '&key
                                    (domap (slot slots)
                                       (list (lmud.int:%custom-at slot 0)
                                             (lmud.int:%custom-at slot 1))))
                     (list* 'lmud.int:%make-custom class-var
                        (domap (slot slots) (lmud.int:%custom-at slot 0)))))))
         (setf (lmud.int:%custom-at class 1) layout)
         (setf (lmud.int:%custom-at class 0) (eval constructor-source)))
      class)
   
   (defun tos.int:class-add-slots (class slots)
      (setf (lmud.int:%custom-at class 3)
            (append (lmud.int:%custom-at class 3) (copy-list slots)))
      (tos.int:rebuild-class-layout class))
   
   (defun tos.int:class-add-slot (class slot)
      (tos.int:class-add-slots class (list slot)))

   (defparameter tos.int:<slot>
      (tos.int:make-class :constructor
         (lambda (class &key name default-value)
            (lmud.int:%make-custom class name default-value))))

   (defun tos.int:make-instance (class &rest params)
      (apply (lmud.int:%custom-at class 0) class params))
   
   (tos.int:class-add-slots tos.int:<class>
      (list (tos.int:make-instance tos.int:<slot> :name 'constructor)
            (tos.int:make-instance tos.int:<slot> :name 'layout)
            (tos.int:make-instance tos.int:<slot> :name 'superclasses)
            (tos.int:make-instance tos.int:<slot> :name 'slots)))
   
   (tos.int:class-add-slots tos.int:<slot>
      (list (tos.int:make-instance tos.int:<slot> :name 'name)
            (tos.int:make-instance tos.int:<slot> :name 'default-value)))
   
   (defparameter <point>
      (tos.int:make-class :slots
         (list (tos.int:make-instance tos.int:<slot> :name 'x :default-value 42)
               (tos.int:make-instance tos.int:<slot> :name 'y :default-value 22))))
   (defparameter *my-point* (tos.int:make-instance <point>))


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
