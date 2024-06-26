
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
      (let ((expanded (macroexpand expression)))
         (lmud.int::%compile expanded))))

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
   
   (defun reverse (list)
      (let ((result nil))
         (dolist (item list)
            (setq result (cons item result)))
         result))

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

   (defun conversions:vectoric-p (e)
      (or (vectorp e) (stringp e) (lmud.int:bytesp e)))

   (defun conversions:->bool (e)
      (if e t nil))

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
   
   (defun conversions:list->bytes (list)
      (apply #'lmud.int:bytes list))
   
   (defun conversions:string->list (string)
      (conversions:sequence->list string))
   
   (defun conversions:sequence->string (sequence)
      (conversions:list->string (conversions:sequence->list sequence)))
   
   (defun conversions:string->vector (string)
      (conversions:list->vector (conversions:string->list string)))
   
   (defun conversions:bytes->vector (bytes)
      (conversions:list->vector (conversions:sequence->list bytes)))
   
   (defun conversions:sequence->bytes (sequence)
      (conversions:list->bytes (conversions:sequence->list sequence)))

   (defun conversions:->list (e)
      (cond ((listp e) e)
            ((conversions:vectoric-p e) (conversions:sequence->list e))
            (t (lmud.util:simple-error "Cannot convert to list!"))))

   (defun conversions:->vector (e)
      (cond ((vectorp e)         e)
            ((listp e)           (conversions:list->vector e))
            ((stringp e)         (conversions:string->vector e))
            ((lmud.int:bytesp e) (conversions:bytes->vector e))
            (t (lmud.util:simple-error "Cannot convert to vector!"))))
   
   (defun conversions:->string (e)
      (cond ((stringp e) e)
            ((listp e)   (conversions:list->string e))
            ((vectorp e) (conversions:sequence->string e))
            (t (lmud.util:simple-error "Cannot convert to string!"))))
   
   (defun conversions:->bytes (e)
      (cond ((lmud.int:bytesp e)        e)
            ((listp e)                  (conversions:list->bytes    e))
            ((conversions:vectoric-p e) (conversions:sequence->bytes e))
            (t (lmud.util:simple-error "Cannot convert to bytes!"))))


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

   (defun (setf aref) (value vector index)
      (list 'lmud.int:%aset vector index value))

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
   ;;;    Sorting
   ;;;

   (defun lmud.util:vector-bubble-sort (vector predicate &key (key #'identity))
      (let ((n (length vector)))
         (while t
            (let ((swapped nil))
               (dotimes (j (- n 1))
                  (let ((a (aref vector j))
                        (b (aref vector (+ j 1))))
                     (when (funcall predicate (funcall key b) (funcall key a))
                        (setf (aref vector j) b)
                        (setf (aref vector (+ j 1)) a)
                        (setq swapped t))))
               (unless swapped (return vector))))
         vector))

   (defun sort (sequence predicate &key (key #'identity))
      (let ((result (lmud.util:vector-bubble-sort (conversions:->vector sequence) predicate :key key))
            (result-list nil))
         (conversions:->list result)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Binding and Destructuring
   ;;;

   (defmacro destructuring-bind (lambda-list expression &body forms)
      (let ((let-expressions '())
            (expr-sym (gensym)))
         (labels ((generate-let (expr func)
                     (cond ((null expr) nil)
                           ((consp expr)
                            (generate-let (car expr) #'(lambda (e) (list 'car (funcall func e))))
                            (generate-let (cdr expr) #'(lambda (e) (list 'cdr (funcall func e)))))
                           ((symbolp expr) (push (list expr (funcall func expr-sym)) let-expressions)))))
               (generate-let lambda-list #'identity)
               (list 'let (list (list expr-sym expression))
                  (list* 'let (reverse let-expressions)
                     forms)))))

   (defmacro multiple-value-bind (lambda-list expression &body forms)
      (list* 'destructuring-bind lambda-list
             (list 'multiple-value-list expression)
             forms))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    The Type- and Object System
   ;;;

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;; TOS internals
   ;;;

   (defun (setf lmud.int:%custom-meta) (value object)
      (list 'lmud.int:%custom-set-meta object value))
   
   (defun (setf lmud.int:%custom-at) (value object index)
      (list 'lmud.int:%custom-set object index value))

   (defun tos.int:%class-constructor       (class) (lmud.int:%custom-at class 0))
   (defun tos.int:%class-layout            (class) (lmud.int:%custom-at class 1))
   (defun tos.int:%class-inheritance-chain (class) (lmud.int:%custom-at class 2))
   (defun tos.int:%class-superclasses      (class) (lmud.int:%custom-at class 3))
   (defun tos.int:%class-slots             (class) (lmud.int:%custom-at class 4))

   (defun (setf tos.int:%class-constructor)       (value class) (list 'lmud.int:%custom-set class 0 value))
   (defun (setf tos.int:%class-layout)            (value class) (list 'lmud.int:%custom-set class 1 value))
   (defun (setf tos.int:%class-inheritance-chain) (value class) (list 'lmud.int:%custom-set class 2 value))
   (defun (setf tos.int:%class-superclasses)      (value class) (list 'lmud.int:%custom-set class 3 value))
   (defun (setf tos.int:%class-slots)             (value class) (list 'lmud.int:%custom-set class 4 value))

   (defun tos.int:%slot-name          (slot) (lmud.int:%custom-at slot 0))
   (defun tos.int:%slot-default-value (slot) (lmud.int:%custom-at slot 1))

   (defun (setf tos.int:%slot-name)          (value slot) (list 'lmud.int:%custom-set slot 0 value))
   (defun (setf tos.int:%slot-default-value) (value slot) (list 'lmud.int:%custom-set slot 1 value))


   (defparameter tos.int:<class> (lmud.int:%make-custom nil nil nil nil nil nil))

   (setf (lmud.int:%custom-meta tos.int:<class>) tos.int:<class>)

   (defun tos.int:make-class (&key (constructor nil) (superclasses nil) (slots nil))
      (let ((instance (lmud.int:%make-custom tos.int:<class> constructor nil nil superclasses slots)))
         (tos.int:rebuild-class-layout instance)
         (when constructor
            (setf (tos.int:%class-constructor instance) constructor))
         instance))
   
   (defun tos.int:class-instance-slot-index-by-name (class name)
      (let ((allslots (tos.int:%class-layout class)))
         (dotimes (i (length allslots))
            (let ((slot (aref allslots i)))
               (if (eq name (tos.int:%slot-name slot))
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
      (let* ((slots  (tos.int:%class-slots class))
             (layout (conversions:->vector slots))
             (inheritance-chain (apply #'append
                                       (domap (superclass (tos.int:%class-superclasses class))
                                          (cons superclass (tos.int:%class-inheritance-chain superclass)))))
             (constructor-source
               (let ((class-var (gensym)))
                  (list 'lambda (list* class-var '&key
                                    (domap (slot slots)
                                       (list (tos.int:%slot-name          slot)
                                             (tos.int:%slot-default-value slot))))
                     (list* 'lmud.int:%make-custom class-var
                        (domap (slot slots) (tos.int:%slot-name slot)))))))
         (setf (tos.int:%class-constructor       class) (eval constructor-source))
         (setf (tos.int:%class-layout            class) layout)
         (setf (tos.int:%class-inheritance-chain class) inheritance-chain))
      class)
   
   (defun tos.int:class-add-slots (class slots)
      (setf (tos.int:%class-slots class)
            (append (tos.int:%class-slots class)
                    (copy-list slots)))
      (tos.int:rebuild-class-layout class))
   
   (defun tos.int:class-add-slot (class slot)
      (tos.int:class-add-slots class (list slot)))

   (defparameter tos.int:<slot>
      (tos.int:make-class :constructor
         (lambda (class &key name default-value)
            (lmud.int:%make-custom class name default-value))))

   (defun tos.int:make-instance (class &rest params)
      (apply (tos.int:%class-constructor class) class params))
   
   (tos.int:class-add-slots tos.int:<class>
      (list (tos.int:make-instance tos.int:<slot> :name 'constructor)
            (tos.int:make-instance tos.int:<slot> :name 'layout)
            (tos.int:make-instance tos.int:<slot> :name 'inheritance-chain)
            (tos.int:make-instance tos.int:<slot> :name 'superclasses)
            (tos.int:make-instance tos.int:<slot> :name 'slots)))
   
   (tos.int:class-add-slots tos.int:<slot>
      (list (tos.int:make-instance tos.int:<slot> :name 'name)
            (tos.int:make-instance tos.int:<slot> :name 'default-value)))
   
   (defun tos.int:construct-class (name superclasses slot-descriptions)
      (let ((slots (domap (slot-description slot-descriptions)
                                    (apply (lambda (name &key initform)
                                             (tos.int:make-instance tos.int:<slot> :name name :default-value initform))
                                           slot-description))))
         (tos.int:make-class :superclasses superclasses
                             :slots        slots)))

   (defun tos.int:defclass-execute (name superclasses slot-descriptions)
      (set-symbol-value name (tos.int:construct-class name superclasses slot-descriptions)))

   (defun tos.int:specific-< (class1 class2)
      (conversions:->bool
         (member class1 (tos.int:%class-inheritance-chain class2))))

   (defmacro tos.int:defclass (name superclasses slot-descriptions)
      (list 'tos.int:defclass-execute (list 'quote name)
                                      (list* 'list superclasses)
                                      (list 'quote slot-descriptions)))
   
   (tos.int:defclass tos.int:<t> () ())

   (tos.int:defclass tos.int:<generic-function> ()
      ((dispatch-table)))
   
   (defun tos.int:extract-typed-args (arglist)
      (cond ((endp arglist) (values nil nil))
            ((member (car arglist) '(&optional &key &rest &body)) (values nil arglist))
            (t (multiple-value-bind (typed untyped)
                     (tos.int:extract-typed-args (cdr arglist))
                  (values (cons (if (consp (car arglist))
                                    (car arglist)
                                    (list (car arglist) tos.int:<t>))
                                typed)
                           untyped)))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;; These are the public TOS macros
   ;;;

   (defmacro tos:defclass (name superclasses slot-descriptions)
      (list 'tos.int:defclass name superclasses slot-descriptions))
   
   (defun tos:class-of (object)
      (tos.int:class-of object))
   
   (tos:defclass <point> ()
      ((x :initform 0)
       (y :initform 0)))
   
   (defvar *my-point* (tos.int:make-instance <point>))

   (tos:defclass <a> () ())
   (tos:defclass <b> () ())
   (tos:defclass <c> () ())
   (tos:defclass <d> () ())

   (tos:defclass <e> (<a> <b>) ())
   (tos:defclass <f> (<b> <c>) ())
   (tos:defclass <g> (<e> <f>) ())


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
