
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  We return a lambda that will be passed to the scheduler
;;;;  as the kickstart function for the master process.
;;;;

(lambda ()
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
      (cond ((endp list) nil)
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

   (defun lmud.util:insertion-step (entry list predicate &key (key #'identity))
      (let ((value (funcall key entry)))
         (cond ((endp list) (list entry))
               ((funcall predicate value (funcall key (car list)))
                (cons entry list))
               (t (cons (car list) (lmud.util:insertion-step entry (cdr list) predicate :key key))))))

   (defun lmud.util:insertion-sort (list predicate &key (key #'identity))
      (let ((lst '()))
         (dolist (entry list)
            (setq lst (lmud.util:insertion-step entry lst predicate :key key)))
         lst))

   (defun sort (sequence predicate &key (key #'identity))
      (lmud.util:insertion-sort sequence predicate :key key))


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


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    String Operations
   ;;;

   (defun string= (a b &optional (key #'identity))
      (and (stringp a)
           (stringp b)
           (or (eq a b)
               (= (length a) (length b))
               (progn (dosequence (i a)
                  (unless (char= (funcall key (aref a i))
                                 (funcall key (aref b i)))
                     (return nil)))
                  t))))
   
   (defun string-upcase (string)
      (conversions:->string (mapcar #'char-upcase (conversions:string->list string))))

   (defun substring (string start end)
      (let ((result '()))
         (dotimes (i (- end start))
            (push (aref string (+ start i)) result))
         (conversions:->string (reverse result))))

   (defun lmud.util:string-subsequence-= (string subsequence &key (start 0) (key #'identity))
      (let ((sublen (length subsequence)))
         (and (>= (length string) (+ start sublen))
              (progn (dotimes (i sublen)
                        (unless (char= (funcall key (aref string (+ start i)))
                                       (funcall key (aref subsequence i)))
                           (return nil)))
                     t))))
   
   (defun lmud.util:string-find-subsequence (string subsequence &key (key #'identity))
      (let ((len    (length string))
            (sublen (length subsequence)))
         (when (>= len sublen)
            (dotimes (i (- len sublen))
               (when (lmud.util:string-subsequence-= string subsequence :start i :key key)
                  (return i))))
         nil))
   
   (defun lmud.util:string-partition (string separator &key (key #'identity))
      (let ((index (lmud.util:string-find-subsequence string separator :key key)))
         (if index
             (values (substring string 0 index)
                     (substring string (+ index (length separator)) (length string)))
             (values string nil))))


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

   (defun tos.int:%class-constructor        (class) (lmud.int:%custom-at class 0))
   (defun tos.int:%class-layout             (class) (lmud.int:%custom-at class 1))
   (defun tos.int:%class-inheritance-chain  (class) (lmud.int:%custom-at class 2))
   (defun tos.int:%class-superclasses       (class) (lmud.int:%custom-at class 3))
   (defun tos.int:%class-slots              (class) (lmud.int:%custom-at class 4))
   (defun tos.int:%class-funcall-dispatcher (class) (lmud.int:%custom-at class 5))

   (defun (setf tos.int:%class-constructor)        (value class) (list 'lmud.int:%custom-set class 0 value))
   (defun (setf tos.int:%class-layout)             (value class) (list 'lmud.int:%custom-set class 1 value))
   (defun (setf tos.int:%class-inheritance-chain)  (value class) (list 'lmud.int:%custom-set class 2 value))
   (defun (setf tos.int:%class-superclasses)       (value class) (list 'lmud.int:%custom-set class 3 value))
   (defun (setf tos.int:%class-slots)              (value class) (list 'lmud.int:%custom-set class 4 value))
   (defun (setf tos.int:%class-funcall-dispatcher) (value class) (list 'lmud.int:%custom-set class 5 value))

   (defun tos.int:%slot-name          (slot) (lmud.int:%custom-at slot 0))
   (defun tos.int:%slot-default-value (slot) (lmud.int:%custom-at slot 1))

   (defun (setf tos.int:%slot-name)          (value slot) (list 'lmud.int:%custom-set slot 0 value))
   (defun (setf tos.int:%slot-default-value) (value slot) (list 'lmud.int:%custom-set slot 1 value))


   (defparameter tos.int:<class> (lmud.int:%make-custom nil nil nil nil nil nil nil))

   (setf (lmud.int:%custom-meta tos.int:<class>) tos.int:<class>)

   (defun tos.int:make-class (&key (constructor nil) (superclasses nil) (slots nil) (funcall-dispatcher nil))
      (let ((instance (lmud.int:%make-custom tos.int:<class> constructor nil nil superclasses slots funcall-dispatcher)))
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
   
   (defun tos.int:soft-class-of (object)
      (cond ((lmud.int:%customp object) (lmud.int:%custom-meta object))
            ((characterp        object) lmud.classes:<character>)
            ((lmud.int:portp    object) lmud.classes:<port>)
            (t                          lmud.classes:<t>)))

   (defun tos.int:classy-object-p (object)
      (not (null (tos.int:soft-class-of object))))

   (defun tos.int:class-of (object)
      (or (tos.int:soft-class-of object)
          (lmud.util:simple-error "Not a classy object!")))

   (defun tos.int:slot-value-by-name (object slot-name)
      (let ((slot-index (tos.int:class-instance-slot-index-by-name (tos.int:class-of object) slot-name)))
         (when (null slot-index)
            (lmud.util:simple-error "Slot not found!"))
         (lmud.int:%custom-at object slot-index)))
   
   (defun tos.int:set-slot-value-by-name (object slot-name value)
      (let ((slot-index (tos.int:class-instance-slot-index-by-name (tos.int:class-of object) slot-name)))
         (when (null slot-index)
            (lmud.util:simple-error "Slot not found!"))
         (setf (lmud.int:%custom-at object slot-index) value)))
   
   (defun tos.int:funcall-dispatcher-of (object)
      (let ((class (tos.int:class-of object)))
         (or (tos.int:%class-funcall-dispatcher class)
             (dolist (superclass (tos.int:%class-inheritance-chain class))
                (when (tos.int:%class-funcall-dispatcher superclass)
                   (return (tos.int:%class-funcall-dispatcher superclass)))))))
   
   (defun tos.int:funcall-dispatcher (object &ignore-rest)
      (lmud.int:funcall-forward (tos.int:funcall-dispatcher-of object)))
   
   (lmud.int:%set-custom-dispatcher-function #'tos.int:funcall-dispatcher)

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

   (defun tos.int:make-instance (class &ignore-rest)
      (lmud.int:funcall-forward (tos.int:%class-constructor class)))
   
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
      (set-symbol-value name
         (tos.int:construct-class name
                                  (or superclasses
                                      (and lmud.classes:<t> (list lmud.classes:<t>)))
                                  slot-descriptions)))

   (defun tos.int:specific-< (class1 class2)
      (conversions:->bool
         (member class1 (tos.int:%class-inheritance-chain class2))))

   (defun tos.int:specific-> (class1 class2)
      (tos.int:specific-< class2 class1))
   
   (defun tos.int:strict-subclassp (class parent-class)
      (tos.int:specific-< parent-class class))

   (defun tos.int:subclassp (class parent-class)
      (or (eq class parent-class)
          (tos.int:strict-subclassp class parent-class)))
   
   (defun tos.int:instancep (object class)
      (and (tos.int:classy-object-p object)
           (tos.int:subclassp (tos.int:class-of object) class)))

   (defmacro tos.int:defclass (name superclasses slot-descriptions)
      (list 'tos.int:defclass-execute (list 'quote name)
                                      (list* 'list superclasses)
                                      (list 'quote slot-descriptions)))
   
   (tos.int:defclass lmud.classes:<t> () ())

   (tos.int:defclass tos.classes:<generic-function> ()
      ((dispatch-table)
       (dispatch-function)))
   
   (defun tos.int:%generic-function-dispatch-table (gf)
      (tos.int:slot-value-by-name gf 'dispatch-table))
   
   (defun (setf tos.int:%generic-function-dispatch-table) (value gf)
      (list 'tos.int:set-slot-value-by-name gf ''dispatch-table value))
   
   (defun tos.int:%generic-function-dispatch-function (gf)
      (tos.int:slot-value-by-name gf 'dispatch-function))
   
   (defun (setf tos.int:%generic-function-dispatch-function) (value gf)
      (list 'tos.int:set-slot-value-by-name gf ''dispatch-function value))

   (defun tos.int:typed-arglist-equal-arity-specific-> (t1 t2)
      (cond ((endp t1) nil)
            ((endp t2) (lmud.util:simple-error "Arity mismatch!"))
            (t (let ((a (cdar t1))
                     (b (cdar t2)))
                  (or (tos.int:specific-> a b)
                      (and (not (tos.int:specific-> b a))
                           (tos.int:typed-arglist-equal-arity-specific-> (cdr t1) (cdr t2))))))))

   (defun tos.int:typed-arglist-specific-> (t1 t2)
      (let ((l1 (length t1))
            (l2 (length t2)))
         (cond ((> l1 l2) t)
               ((< l1 l2) nil)
               (t (tos.int:typed-arglist-equal-arity-specific-> t1 t2)))))
   
   (defun tos.int:generic-function-resort-methods (gf)
      (setf (tos.int:%generic-function-dispatch-table gf)
            (sort (tos.int:%generic-function-dispatch-table gf)
                  #'tos.int:typed-arglist-specific->
                  :key #'car))
      (let ((dispatcher-source
               (list 'lambda '(&ignore-rest)
                     (list* 'cond
                        (domap (clause (tos.int:%generic-function-dispatch-table gf))
                           (list (list* 'and
                                    (list '>= '(lmud.int:%given-argument-count) (length (car clause)))
                                    (let ((arg-index 0))
                                       (domap (arg (car clause))
                                          (prog1 (list 'tos.int:instancep (list 'lmud.int:%given-argument-ref arg-index) (cdr arg))
                                             (incf arg-index)))))
                                 (list 'lmud.int:funcall-forward (cdr clause))))))))
         (setf (tos.int:%generic-function-dispatch-function gf)
               (eval dispatcher-source)))
      gf)

   (defun tos.int:generic-function-add-method (gf fixed-args lambda)
      ;; TODO: Check for duplicate methods, replace them
      (push (cons fixed-args lambda) (tos.int:%generic-function-dispatch-table gf))
      (tos.int:generic-function-resort-methods gf))
   
   (defun tos.int:generic-function-invoke (gf &ignore-rest)
      (lmud.int:funcall-forward-rest (tos.int:%generic-function-dispatch-function gf)))
   (setf (tos.int:%class-funcall-dispatcher tos.classes:<generic-function>) #'tos.int:generic-function-invoke)

   (defun tos.int:extract-typed-args (arglist)
      (cond ((endp arglist) (values nil nil))
            ((member (car arglist) '(&optional &key &rest &body &ignore-rest)) (values nil arglist))
            (t (multiple-value-bind (typed untyped)
                     (tos.int:extract-typed-args (cdr arglist))
                  (values (cons (if (consp (car arglist))
                                    (car arglist)
                                    (list (car arglist) lmud.classes:<t>))
                                typed)
                           untyped)))))
   
   (defmacro tos.int:defmethod-on-generic-function (gf args &body body)
      (multiple-value-bind (typed-args untyped-args)
            (tos.int:extract-typed-args args)
         (let ((real-arglist (append (mapcar #'car typed-args) untyped-args)))
            (list 'tos.int:generic-function-add-method
                     gf
                     (list* 'list
                            (domap (argdef typed-args)
                               (list 'cons (list 'quote (car argdef)) (list* 'progn (cdr argdef)))))
                     (list* 'lambda real-arglist body)))))

   (defun tos.int:ensure-generic-function (symbol)
      (let ((value (symbol-function symbol)))
         (if (tos.int:instancep value tos.classes:<generic-function>)
             value
             (progn (when (not (null value))
                       (lmud.util:simple-error "Symbol already bound to a non-generic-function!"))
                    (let ((gf (tos.int:make-instance tos.classes:<generic-function>)))
                       (set-symbol-function symbol gf)
                       gf)))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;; These are the public TOS macros
   ;;;

   (defun tos:class-of (object)
      (tos.int:class-of object))
   
   (defun tos:instancep (object class)
      (tos.int:instancep object class))
   
   (defmacro tos:defclass (name superclasses slot-descriptions)
      (list 'tos.int:defclass name superclasses slot-descriptions))
   
   (defmacro tos:defgeneric (name args &body body)
      (list 'tos.int:ensure-generic-function (list 'quote name)))

   (defmacro tos:defmethod (name args &body body)
      (list* 'tos.int:defmethod-on-generic-function (list 'tos.int:ensure-generic-function (list 'quote name)) args body))
   
   (defun tos:make-instance (class &ignore-rest)
      (lmud.int:funcall-forward #'tos.int:make-instance))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;; The TOS standard classes
   ;;;

   (tos:defclass lmud.classes:<character> () ())
   (tos:defclass lmud.classes:<port>      () ())


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Streams and I/O
   ;;;

   (tos:defclass io.stream:<stream> () ())

   (tos:defmethod io:write-byte-to-stream ((stream lmud.classes:<port>) protocol byte)
      (lmud.int:port-write-byte stream byte))
   
   (tos:defmethod io:read-byte-from-stream ((stream lmud.classes:<port>) protocol)
      (lmud.int:port-read-byte stream))
   
   (tos:defmethod io:unread-char-from-stream ((stream lmud.classes:<port>) protocol char)
      (lmud.int:port-unread-char stream char))
   
   (defun io:write-utf8-char (char stream protocol)
      (let ((code (char-code char)))
         (cond ((< code #x80)    (io:write-byte-to-stream stream protocol code))
               ((< code #x800)   (io:write-byte-to-stream stream protocol (logior #xC0 (ash code -6)))
                                 (io:write-byte-to-stream stream protocol (logior #x80 (logand code #x3F))))
               ((< code #x10000) (io:write-byte-to-stream stream protocol (logior #xE0 (ash code -12)))
                                 (io:write-byte-to-stream stream protocol (logior #x80 (logand (ash code -6) #x3F)))
                                 (io:write-byte-to-stream stream protocol (logior #x80 (logand code #x3F))))
               (t                (io:write-byte-to-stream stream protocol (logior #xF0 (ash code -18)))
                                 (io:write-byte-to-stream stream protocol (logior #x80 (logand (ash code -12) #x3F)))
                                 (io:write-byte-to-stream stream protocol (logior #x80 (logand (ash code -6) #x3F)))
                                 (io:write-byte-to-stream stream protocol (logior #x80 (logand code #x3F)))))))
   
   (defun io:read-utf8-char (stream protocol)
      (code-char
         (let ((byte (io:read-byte-from-stream stream protocol)))
            (cond ((< byte #x80) byte)
                  ((< byte #xE0) (logior (ash (logand byte #x1F) 6)
                                         (logand (io:read-byte-from-stream stream protocol) #x3F)))
                  ((< byte #xF0) (logior (ash (logand byte #xF) 12)
                                         (ash (logand (io:read-byte-from-stream stream protocol) #x3F) 6)
                                         (logand (io:read-byte-from-stream stream protocol) #x3F)))
                  ((< byte #xF8) (logior (ash (logand byte #x7) 18)
                                         (ash (logand (io:read-byte-from-stream stream protocol) #x3F) 12)
                                         (ash (logand (io:read-byte-from-stream stream protocol) #x3F) 6)
                                         (logand (io:read-byte-from-stream stream protocol) #x3F)))
                  (t (lmud.util:simple-error "Invalid UTF-8 encoding!"))))))

   (tos:defmethod io:read-char-from-stream (stream protocol)
      (io:read-utf8-char stream protocol))
   
   (tos:defmethod io:write-char-to-stream (stream protocol char)
      (io:write-utf8-char char stream protocol))
   
   (defun io:eof-p (stream) nil) ; TODO

   (defun write-byte (byte stream)
      (io:write-byte-to-stream stream nil byte))
   
   (defun read-byte (stream)
      (io:read-byte-from-stream stream nil))
   
   (defun write-char (char stream)
      (io:write-char-to-stream stream nil char))
   
   (defun read-char (stream)
      (io:read-char-from-stream stream nil))
   
   (defun unread-char (char stream)
      (io:unread-char-from-stream stream nil char))
   
   (defun peek-char (stream)
      (let ((char (read-char stream)))
         (unread-char char stream)
         char))

   (defun io.reader:whitespacep (char)
      (or (char= char #\Space)
          (char= char #\Tab)
          (char= char #\Newline)
          (char= char #\Return)))
   
   (defun io.reader:breaking-char-p (char)
      (or (char= char (code-char 40)) ; '('
          (char= char (code-char 41)) ; ')'
          (io.reader:whitespacep char)))

   (defun io.reader:check (stream char)
      (let ((parsed-char (read-char stream)))
         (if (char= parsed-char char)
             parsed-char
             (progn (unread-char parsed-char stream)
                    (values nil nil)))))
   
   (defun io.reader:checkpred (stream predicate)
      (let ((parsed-char (read-char stream)))
         (if (funcall predicate parsed-char)
             parsed-char
             (progn (unread-char parsed-char stream)
                    nil))))
   
   (defun io.reader:checkstr (stream sequence)
      (dotimes (i (length sequence))
         (unless (io.reader:check stream (aref sequence i))
            (dotimes (j i)
               (unread-char (aref sequence (- i j 1)) stream))
            (return-from io.reader:checkstr nil)))
      t)

   (defun io.reader:skip (stream predicate)
      (while (io.reader:checkpred stream predicate)))
   
   (defun io.reader:read-until2 (stream predicate)
      (let ((result '()))
         (while (not (io:eof-p stream))
            (let ((char (read-char stream)))
               (when (funcall predicate char)
                  (unread-char char stream)
                  (return (conversions:->string (reverse result))))
               (push char result)))
         (conversions:->string (reverse result))))
   
   (defun io.reader:read-until (stream predicate)
      (lmud.dummy::%princln "Reading until...")
      (prog1 (io.reader:read-until2 stream predicate)
         (lmud.dummy::%princln "Done reading until!")))
   
   (defun io.reader:skip-whitespace (stream)
      (io.reader:skip stream #'io.reader:whitespacep))

   (defun io.reader:read-until-breaking-char (stream)
      (io.reader:read-until stream #'io.reader:breaking-char-p))
   
   (defun io.reader:generate-letcond (clauses)
      (if (endp clauses)
          nil
          (let ((clause (car clauses))
                (var    (gensym)))
             (list 'let (list (cons var (cdar clause)))
                (list 'if var
                   (if var
                       (list* 'let (list (list (caar clause) var))
                          (cdr clause))
                       (cons 'progn (cdr clause)))
                   (io.reader:generate-letcond (cdr clauses)))))))
   
   (defmacro io.reader:letcond (&body clauses)
      (io.reader:generate-letcond clauses))
   
   (defmacro io.reader:iflet (var-expr &body body)
      (list 'let (list var-expr)
         (list* 'if (car var-expr)
            body)))
   
   (defun io.reader:eof-error (stream)
      (lmud.util:simple-error "Unexpected end of file!"))

   (defun io.reader:read-list (stream)
      (io.reader:skip-whitespace stream)
      (cond ((io:eof-p stream) (io.reader:eof-error stream))
            ((io.reader:checkstr stream ")") nil)
            ((io.reader:checkstr stream ". ")
               (prog1 (io.reader:read stream)
                      (unless (io.reader:checkstr stream ")")
                         (lmud.util:simple-error "Expected ')' after '.'!"))))
            (t (cons (io.reader:read stream)
                     (io.reader:read-list stream)))))

   (defun io.reader:char->digit (char &optional (base 10))
      (let ((value (cond ((and (char>= char #\0) (char<= char #\9)) (- (char-code char) (char-code #\0)))
                         ((and (char>= char #\A) (char<= char #\Z)) (+ 10 (- (char-code char) (char-code #\A))))
                         ((and (char>= char #\a) (char<= char #\z)) (+ 10 (- (char-code char) (char-code #\a))))
                         (t nil))))
         (and value (< value base) value))) 

   (defun io.reader:read-integer (stream &optional (base 10))
      (io.reader:skip-whitespace stream)

      (let ((negative nil)
            (value 0)
            (digits-read 0))

         (setq negative (io.reader:check stream #\-))

         (while t
            (let* ((char  (read-char stream))
                   (digit (io.reader:char->digit char base)))
               (if digit
                   (setq value       (+ (* value base) digit)
                         digits-read (+ digits-read 1))
                   (progn (unread-char char stream)
                          (return (and (> digits-read 0)
                                       (if negative (- value) value)))))))))

   (defun io.reader:read-atom (stream)
      (or (io.reader:read-integer stream)
          (let ((text (io.reader:read-until-breaking-char stream)))
             (multiple-value-bind (part-1 part-2)
                   (multiple-value-bind (a b)
                         (lmud.util:string-partition text "::")
                      (if b
                          (values a b)
                          (lmud.util:string-partition text ":")))
                (if part-2
                    (intern (string-upcase part-2) (find-package (string-upcase part-1)))
                    (intern (string-upcase text)))))))
   
   (defun io.reader:read-keyword (stream)
      (let ((text (io.reader:read-until-breaking-char stream)))
         (intern (string-upcase text) (find-package "KEYWORD"))))

   (defun io.reader:read (stream)
      (io.reader:skip-whitespace stream)
      (cond ((io:eof-p stream) nil)
            ((io.reader:checkstr stream ";")
             (io.reader:skip stream (lambda (char) (not (char= char #\Newline))))
             (io.reader:read stream))
            ((io.reader:checkstr stream "(")
             (io.reader:read-list stream))
            ((io.reader:checkstr stream ":")
             (io.reader:read-keyword))
            ((io.reader:checkstr stream "'")
             (list 'quote (io.reader:read stream)))
            ((io.reader:checkstr stream "#'")
             (list 'function (io.reader:read stream)))
            ((io.reader:checkstr stream "#b")
             (io.reader:read-integer stream 2))
            ((io.reader:checkstr stream "#o")
             (io.reader:read-integer stream 8))
            ((io.reader:checkstr stream "#x")
             (io.reader:read-integer stream 16))
            (t (io.reader:read-atom stream))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    REPL
   ;;;

   (defun lmud.dummy::%princln (e)
      (lmud.dummy::%princ e)
      (lmud.dummy::%terpri))

   (lmud.int:on-connect
      (lambda (port)
         (lmud.dummy::%princln "New connection just came in!")
         (while t
            (dosequence (i "⍝ ")
               (write-char i port))
            (lmud.dummy::%prin1 (io.reader:read port))
            (lmud.dummy::%terpri))))

   (defun lmud.bootstrap::repl ()
      (while t
         (lmud.dummy::%princ "⍝ ")
         (let ((expr (lmud.dummy::%read)))
            (dolist (e (multiple-value-list (eval expr)))
               (lmud.dummy::%princ "  ")
               (lmud.dummy::%prin1 e)
               (lmud.dummy::%terpri)))))
   ))

   (lmud.bootstrap::repl)
)
