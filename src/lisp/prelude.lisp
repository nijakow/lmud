
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
                (if (eq head 'lmud.qq:quasiquote)
                    (macroexpand (lmud.qq:expand-quasiquote (cadr expression)))
                    (if (if (symbolp head)
                            (if (symbol-macro head) t))
                        (macroexpand (apply (symbol-macro head) args))
                        (lmud.util:map1 #'macroexpand expression)))))
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
                (list 'lambda args
                   (list 'declare (list 'function-name name))
                   (list* 'block name body))))))

(set-symbol-macro 'defmacro
   (lambda (name args &rest body)
      (list 'set-symbol-macro (list 'quote name)
            (list 'lambda args
               (list 'declare (list 'macro-name name))
               (list* 'block name body)))))


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

   (defmacro defalias (original alias)
      (list 'defmacro original '(&rest args)
         (list 'cons (list 'quote alias) 'args)))

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
   ;;;    Globals
   ;;;

   (defparameter lmud:*default-package* (find-package "LISP"))
   (defparameter lmud:*keyword-package* (find-package "KEYWORD"))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Error Handling
   ;;;

   (defun lmud.util:simple-error (&optional (message nil))
      (lmud.int:signal message))
   
   (defun lmud:todo! ()
      (lmud.util:simple-error "TODO!"))


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
   
   (defmacro prog1e (expr &rest body)
      (list 'progn
         (cons 'progn body)
         expr))


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
   
   (defun nreverse (list) (reverse list))

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

   (defun remove (item list)
      (cond ((endp list) nil)
            ((eq item (car list))
             (remove item (cdr list)))
            (t (cons (car list) (remove item (cdr list))))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Case analysis
   ;;;

   (defmacro case (key &rest clauses)
      (let ((temp (gensym)))
         (list 'let (list (list temp key))
            (cons 'cond
                  (domap (clause clauses)
                     (if (listp (car clause))
                         (cons (list 'member temp (list 'quote (car clause)))
                               (cdr clause))
                         (cons 't (cdr clause))))))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Association Lists and Property Lists
   ;;;

   (defun assoc (item alist &key (key #'car) (test #'eq))
      (dolist (pair alist)
         (if (funcall test item (funcall key pair))
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
   ;;;    Math
   ;;;

   (defun numberp (e)
      (or (integerp e)
          (lmud.int:ratiop e)))

   (defun 1+ (n) (+ n 1))
   (defun 1- (n) (- n 1))

   (defun zerop  (n) (= n 0))
   (defun plusp  (n) (> n 0))
   (defun minusp (n) (< n 0))

   (defun evenp (n) (= (mod n 2) 0))
   (defun oddp  (n) (not (evenp n)))

   (defun gcd (a b)
      (if (= b 0)
          a
          (gcd b (mod a b))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Characters
   ;;;

   (defun char=  (a b) (= (char-code a) (char-code b)))
   (defun char<  (a b) (< (char-code a) (char-code b)))
   (defun char>  (a b) (> (char-code a) (char-code b)))
   (defun char<= (a b) (<= (char-code a) (char-code b)))
   (defun char>= (a b) (>= (char-code a) (char-code b)))

   (defun lmud.char:char=-ignore-case (a b)
      (or (char= a b)
          (char= (char-upcase a) (char-upcase b))))

   (defun lmud.char:printable-char-p (char)
      (let ((code (char-code char)))
         (or (and (>= code #x20) (<= code #x7E))
             (>= code #xA0))))

   (defun lmud.char:whitespacep (char)
      (or (char= char #\Space)
          (char= char #\Tab)
          (char= char #\Newline)
          (char= char #\Return)
          (char= char #\Backspace)))
   
   (defun lmud.char:newlinep (char)
      (char= char #\Newline))

   (defparameter lmud.char:*special-character-names*
      (list (cons #\Space     "Space")
            (cons #\Tab       "Tab")
            (cons #\Newline   "Newline")
            (cons #\Return    "Return")
            (cons #\Backspace "Backspace")
            (cons #\Escape    "Escape")))

   (defun lmud.char:special-character-by-name (name)
      (dolist (pair lmud.char:*special-character-names*)
         (when (string= name (cdr pair) :compare #'lmud.char:char=-ignore-case)
            (return (car pair))))
      (lmud.util:simple-error "Unknown character name!"))
   
   (defun lmud.char:character-by-name (name)
      (if (= (length name) 1)
          (aref name 0)
          (lmud.char:special-character-by-name name)))
   
   (defun lmud.char:character-name-or-nil (char)
      (dolist (pair lmud.char:*special-character-names*)
         (when (char= char (car pair))
            (return (cdr pair))))
      nil)
   
   (defun lmud.char:character-name (char)
      (or (lmud.char:character-name-or-nil char)
          (if (lmud.char:printable-char-p char)
              (string char)
              "UNPRINTABLE_UNNAMED_CHAR")))


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
   ;;;    String Operations
   ;;;

   (defun string:concatenate (&rest strings)
      (conversions:->string
             (apply #'append (mapcar #'conversions:string->list strings))))

   (defun string:partition (string separator)
      (lmud.util:string-partition string separator))
   
   (defun string:split (string separator)
      (let ((index (lmud.util:string-find-subsequence string separator)))
         (if index
             (cons (substring string 0 index)
                   (string:split (substring string (+ index (length separator)) (length string)) separator))
             (list string))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Functions
   ;;;

   (defun functionp (e)
      (or (lmud.int:bytecode-function-p e)
          (lmud.int:machine-function-p  e)
          (lmud.int:closurep            e)))

   (defun fboundp (e)
      (unless (symbolp e)
         (lmud.util:simple-error "Fboundp expects a symbol!"))
      (functionp (symbol-function e)))


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
   
   (defun (setf get) (value symbol property)
      (list 'put symbol property value))

   (defmacro push (item place)
      (list 'setf place (list 'cons item place)))
   
   (defmacro pop (place)
      (let ((temp (gensym)))
         (list 'let (list (list temp place))
            (list 'setf place (list 'cdr temp))
            (list 'car temp))))
   
   (defmacro incf (place &optional (amount 1))
      (list 'setf place (list '+ place amount)))
   
   (defmacro decf (place &optional (amount 1))
      (list 'setf place (list '- place amount)))


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

   (defun string= (a b &key (key #'identity) (compare #'char=))
      (and (stringp a)
           (stringp b)
           (or (eq a b)
               (and (= (length a) (length b))
                    (progn (dotimes (i (length a))
                              (unless (funcall compare (funcall key (aref a i))
                                                       (funcall key (aref b i)))
                                 (return nil)))
                           t)))))
   
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
   ;;;    The Quasiquote Expander
   ;;;

   (defun lmud.qq:tag-clause-p (symbol expr)
      (and (consp expr)
           (eq (car expr) symbol)
           (consp (cdr expr))
           (null (cddr expr))))
   
   (defun lmud.qq:quasiquote-clause-p (expr)
      (lmud.qq:tag-clause-p 'lmud.qq:quasiquote expr))

   (defun lmud.qq:unquote-clause-p (expr)
      (lmud.qq:tag-clause-p 'lmud.qq:unquote expr))
   
   (defun lmud.qq:unquote-splice-clause-p (expr)
      (lmud.qq:tag-clause-p 'lmud.qq:unquote-splice expr))

   (defun lmud.qq:quote (expr)
      (list 'quote expr))

   (defun lmud.qq:expand-quasiquote-list (expr)
      (if (consp expr)
          (if (lmud.qq:unquote-splice-clause-p (car expr))
              (list (list 'append (cadar expr)
                                  (cons 'list* (lmud.qq:expand-quasiquote-list (cdr expr)))))
              (cons (lmud.qq:expand-quasiquote      (car expr))
                    (lmud.qq:expand-quasiquote-list (cdr expr))))
          (list expr)))

   (defun lmud.qq:expand-quasiquote (expr)
      (cond ((lmud.qq:quasiquote-clause-p expr) (lmud.qq:quote expr))
            ((lmud.qq:unquote-clause-p expr)    (cadr expr))
            ((consp expr) (cons 'list* (lmud.qq:expand-quasiquote-list expr)))
            (t (lmud.qq:quote expr))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    A new Macroexpander
   ;;;

   (defun lmud.util:macroexpand-list (expression environment)
      (if (endp expression)
          nil
          (cons (macroexpand (car expression) environment)
                (lmud.util:macroexpand-list (cdr expression) environment))))

   (defun macroexpand (expression &optional environment)
      (flet ((find-macro-function (object environment)
               (when (symbolp object)
                  (let ((handle (assoc object environment)))
                     (when handle
                        (return (cdr handle)))
                     (symbol-macro object)))))
         (cond ((consp expression)
                (let ((head (car expression))
                      (args (cdr expression)))
                   (cond ((eq head 'quote) expression)
                         ((eq head 'lmud.qq:quasiquote)
                          (macroexpand (lmud.qq:expand-quasiquote (cadr expression)) environment))
                         ((eq head 'macrolet)
                          (let ((macros (car args))
                                (body   (cdr args)))
                             (let ((new-env (append (domap (macro macros)
                                                      (cons (car macro)
                                                            (eval (cons 'lambda (cdr macro)))))
                                                    environment)))
                                (cons 'progn
                                   (domap (e body) (macroexpand e new-env))))))
                         (t (let ((func (find-macro-function head environment)))
                               (if func
                                   (macroexpand (apply func args) environment)
                                   (lmud.util:macroexpand-list expression environment)))))))
               (t expression))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    The Struct System
   ;;;

   (defun struct:structp (e)
      (and (lmud.int:%customp e)
           (consp (lmud.int:%custom-meta e))
           (eq (car (lmud.int:%custom-meta e)) 'struct:struct)))
   
   (defun struct:struct-with-tag-p (e tag)
      (and (struct:structp e)
           (eq (cdr (lmud.int:%custom-meta e)) tag)))
   
   (defun struct:ensure-struct (e tag)
      (if (struct:struct-with-tag-p e tag)
          e
          (lmud.util:simple-error "Not a struct!")))

   (defmacro defstruct (name &rest slots)
      (unless (symbolp name)
         (lmud.util:simple-error "The name of a struct must be a symbol!"))
      (flet ((intern-func-pre  (text)
               (intern (string:concatenate text (symbol-name name))
                       (symbol-package name)))
            (intern-func-post (text)
               (intern (string:concatenate (symbol-name name) text)
                       (symbol-package name)))
            (intern-func-post2 (t1 t2)
               (intern (string:concatenate (symbol-name name) t1 t2)
                       (symbol-package name))))
         (let ((type-tag name))
            (list* 'progn
               (list 'defun (intern-func-pre "MAKE-") (cons '&key slots)
                  (list* 'lmud.int:%make-custom (list 'quote (cons 'struct:struct type-tag))
                                                slots))
               (list 'defun (intern-func-post "-P") '(object)
                  (list 'struct:struct-with-tag-p 'object (list 'quote type-tag)))
               (let ((slot-count 0))
                  (domap (slot slots)
                     (let ((slot-accessor-symbol (intern-func-post2 "-" (symbol-name slot))))
                        (prog1 (list 'progn
                                  (list 'defun slot-accessor-symbol '(object)
                                     (list 'struct:ensure-struct 'object (list 'quote type-tag))
                                     (list 'lmud.int:%custom-at 'object slot-count))
                                  (list 'defun (list 'setf slot-accessor-symbol) '(value object)
                                     (list 'list ''progn
                                        (list 'list ''struct:ensure-struct 'object (list 'list ''quote (list 'quote type-tag)))
                                        (list 'list ''lmud.int:%custom-set 'object slot-count 'value)))
                           (incf slot-count))))))))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    The Type- and Object System
   ;;;

   (defun tos.int:%class-supers  (class) (lmud.int:%custom-at class 0))
   (defun tos.int:%class-methods (class) (lmud.int:%custom-at class 1))
   (defun tos.int:%class-vars    (class) (lmud.int:%custom-at class 2))
   (defun tos.int:%class-name    (class) (lmud.int:%custom-at class 3))

   (defun tos.int:%class-supers!  (class supers)  (lmud.int:%custom-set class 0 supers))
   (defun tos.int:%class-methods! (class methods) (lmud.int:%custom-set class 1 methods))
   (defun tos.int:%class-vars!    (class vars)    (lmud.int:%custom-set class 2 vars))

   (defun tos.int:%class-push-method! (class name method)
      (let ((methods (tos.int:%class-methods class)))
         (tos.int:%class-methods! class (cons (cons name method) methods))))
   
   (defun tos.int:%class-push-var! (class name value)
      (let ((vars (tos.int:%class-vars class)))
         (tos.int:%class-vars! class (cons (cons name value) vars))))

   (defun tos.int:pre-make-class (supers &key name)
      (lmud.int:%make-custom 'class supers nil nil name))
   
   (defun tos.int:%object-class (object) (lmud.int:%custom-meta object))
   (defun tos.int:%object-name  (object) (lmud.int:%custom-at object 0))
   (defun tos.int:%object-vars  (object) (lmud.int:%custom-at object 1))

   (defun tos.int:%object-class! (object class) (lmud.int:%custom-set object 0 class))
   (defun tos.int:%object-name!  (object name)  (lmud.int:%custom-set object 0 name))
   (defun tos.int:%object-vars!  (object vars)  (lmud.int:%custom-set object 1 vars))

   (defun tos.int:pre-make-instance (class &key name)
      (lmud.int:%make-custom class name nil))

   (defun tos.int:classp (object)
      (and (lmud.int:%customp object)
           (eq (lmud.int:%custom-meta object) 'class)))
      
   (defun tos.int:oopp (object)
      (and (lmud.int:%customp object)
           (tos.int:classp (lmud.int:%custom-meta object))))
   
   (defparameter tos.classes:<t>         (tos.int:pre-make-class '() :name 'tos.classes:<t>))
   (defparameter tos.classes:<class>     (tos.int:pre-make-class (list tos.classes:<t>) :name 'tos.classes:<class>))
   (defparameter tos.classes:<symbol>    (tos.int:pre-make-class (list tos.classes:<t>) :name 'tos.classes:<symbol>))
   (defparameter tos.classes:<cons>      (tos.int:pre-make-class (list tos.classes:<t>) :name 'tos.classes:<cons>))
   (defparameter tos.classes:<number>    (tos.int:pre-make-class (list tos.classes:<t>) :name 'tos.classes:<number>))
   (defparameter tos.classes:<integer>   (tos.int:pre-make-class (list tos.classes:<number>) :name 'tos.classes:<integer>))
   (defparameter tos.classes:<rational>  (tos.int:pre-make-class (list tos.classes:<number>) :name 'tos.classes:<rational>))
   (defparameter tos.classes:<ratio>     (tos.int:pre-make-class (list tos.classes:<rational>) :name 'tos.classes:<ratio>))
   (defparameter tos.classes:<character> (tos.int:pre-make-class (list tos.classes:<t>) :name 'tos.classes:<character>))
   (defparameter tos.classes:<function>  (tos.int:pre-make-class (list tos.classes:<t>) :name 'tos.classes:<function>))
   (defparameter tos.classes:<string>    (tos.int:pre-make-class (list tos.classes:<t>) :name 'tos.classes:<string>))
   (defparameter io:<basic-stream>       (tos.int:pre-make-class (list tos.classes:<t>) :name 'io:<basic-stream>))
   (defparameter io:<port-stream>        (tos.int:pre-make-class (list io:<basic-stream>) :name 'io:<port-stream>))
   (defparameter tos.classes:<port>      (tos.int:pre-make-class (list io:<port-stream>) :name 'tos.classes:<port>))
   (defparameter tos.classes:<process>   (tos.int:pre-make-class (list tos.classes:<t>) :name 'tos.classes:<process>))

   (defun tos.int:class-of (object)
      (cond ((tos.int:oopp      object) (lmud.int:%custom-meta object))
            ((tos.int:classp    object) tos.classes:<class>)
            ((symbolp           object) tos.classes:<symbol>)
            ((consp             object) tos.classes:<cons>)
            ((integerp          object) tos.classes:<integer>)
            ((lmud.int:ratiop   object) tos.classes:<ratio>)
            ((numberp           object) tos.classes:<number>)
            ((characterp        object) tos.classes:<character>)
            ((functionp         object) tos.classes:<function>)
            ((stringp           object) tos.classes:<string>)
            ((lmud.int:portp    object) tos.classes:<port>)
            ((lmud.int:processp object) tos.classes:<process>)
            (t                          tos.classes:<t>)))
   
   (defun tos.int:subclassp (class1 class2)
      (or (eq class1 class2)
          (dolist (super (tos.int:%class-supers class1))
             (when (tos.int:subclassp super class2)
                (return t)))))
   
   (defun tos.int:instancep (object class)
      (tos.int:subclassp (tos.int:class-of object) class))
   
   (defun tos.int:lookup-method-in-class (class message)
      (dolist (method (tos.int:%class-methods class))
         (when (eq (car method) message)
            (return (cdr method))))
      (dolist (super (tos.int:%class-supers class))
         (let ((result (tos.int:lookup-method-in-class super message)))
            (when result
               (return result))))
      nil)
   
   (defun tos.int:lookup-variable-slot-in-class (class variable)
      (dolist (var (tos.int:%class-vars class))
         (when (eq (car var) variable)
            (return var)))
      (dolist (super (tos.int:%class-supers class))
         (let ((result (tos.int:lookup-variable-slot-in-class super variable)))
            (when result
               (return result))))
      nil)
   
   (defun tos.int:error-method (object)
      (lmud.util:simple-error "No method found!"))

   (defun tos.int:lookup-method-in-object (object message)
      (or (tos.int:lookup-method-in-class (tos.int:class-of object) message)
          #'tos.int:error-method))
   
   (defun tos.int:lookup-method-in-class-or-error (class message)
      (or (tos.int:lookup-method-in-class class message)
          (lmud.util:simple-error "No such method!")))

   (defun tos.int:lookup-variable-slot-in-object (object variable)
      (dolist (var (tos.int:%object-vars object))
         (when (eq (car var) variable)
            (return var)))
      nil)
   
   (defun tos.int:lookup-variable-slot (object variable)
      (or (and (tos.int:oopp object)
               (tos.int:lookup-variable-slot-in-object object variable))
          (tos.int:lookup-variable-slot-in-class (tos.int:class-of object) variable)))

   (defun tos.int:get-variable-value (object variable)
      (let ((slot (tos.int:lookup-variable-slot object variable)))
         (if slot
             (cdr slot)
             (lmud.util:simple-error "No such variable!"))))
   
   (defun tos.int:set-variable-value (object variable value)
      (unless (tos.int:oopp object)
         (lmud.util:simple-error "Cannot set variable in an immutable object type!"))
      (let ((slot (tos.int:lookup-variable-slot-in-object object variable)))
         (if slot
             (rplacd slot value)
             (tos.int:%object-vars! object (cons (cons variable value) (tos.int:%object-vars object))))))

   (defun tos.int:ensure-class (e)
      (cond ((tos.int:classp e) e)
            ((tos.int:oopp   e)  (tos.int:class-of e))
            ((symbolp        e)
             (let ((symbol-value (symbol-value e)))
                (if (tos.int:classp symbol-value)
                    symbol-value
                    (let ((the-class (tos.int:pre-make-class (list tos.classes:<t>) :name e)))
                       (set-symbol-value e the-class)
                       the-class))))
            (t (lmud.util:simple-error "Invalid class!"))))

   (defmacro tos:define-class-hook (name args &body body)
      (list 'setf (list 'get (list 'quote name) ''tos.int:defclass-hook-function)
         (list* 'lambda args body)))

   (defmacro tos:defclass (name supers &body body)
      (when (null supers)
         (setq supers (list tos.classes:<t>)))
      (let ((class (gensym)))
         (list* 'let (list (list class (list 'tos.int:ensure-class (if (symbolp name) (list 'quote name) name))))
            ;; TODO: Clear class methods and vars
            (list 'tos.int:%class-supers! class (cons 'list supers))
            (apply #'append
               (domap (clause body)
                  (cond ((listp clause)
                         ;; TODO: Add support for class hooks
                         (case (car clause)
                            ((:with with)
                             (domap (var (cdr clause))
                                (cond ((symbolp var) (list 'tos.int:%class-push-var! class (list 'quote var) nil))
                                      ((consp   var) (list 'tos.int:%class-push-var! class (list 'quote (car var)) (cons 'progn (cdr var))))
                                      (t (lmud.util:simple-error "Invalid tos:defclass variable definition!")))))
                            (t (let ((hook-function (or (get (car clause) 'tos.int:defclass-hook-function)
                                                        (lambda (&rest args)
                                                           (lmud.util:simple-error "Invalid tos:defclass clause!")))))
                                 (apply hook-function class (cdr clause))))))
                        (t (lmud.util:simple-error "Invalid tos:defclass clause!"))))))))

   (defun tos.int:ensure-singleton-class (e)
      (cond ((tos.int:oopp e) e)
            (t (lmud.util:simple-error "Expected an object!"))))
   
   (defmacro tos:defobject (name supers &body body)
      (list* 'tos:defclass (list 'tos.int:ensure-singleton-class (if (symbolp name) (list 'quote name) name))
             supers
             body))

   (defmacro tos:defmethod (info params &body body)
      (let ((class       (car info))
            (method-name (cadr info)))
         (let ((method (list 'lambda (cons 'self params)
                          (list 'declare (list 'method-name method-name))
                          (list* 'block method-name body))))
            (list 'tos.int:%class-push-method! (list 'tos.int:ensure-class class) (list 'quote method-name) method))))

   (defun tos:dot (object variable)
      (tos.int:get-variable-value object variable))
   
   (defun (setf tos:dot) (value object variable)
      (list 'tos.int:set-variable-value object variable value))

   (defun tos:xsend (class object message &ignore-rest)
      (lmud.int:funcall-forward-rest (tos.int:lookup-method-in-class-or-error class message) object))

   (defun tos:send (object message &ignore-rest)
      (lmud.int:funcall-forward-rest #'tos:xsend (tos.int:class-of object) object message))

   (defun tos:make-instance (class &ignore-rest)
      (lmud.int:funcall-forward-rest #'tos.int:pre-make-instance class))

   (defun tos:find (symbol)
      (unless (symbolp symbol)
         (lmud.util:simple-error "Expected a symbol!"))
      (or (get symbol 'tos.int:object-value)
          (let ((object (tos.int:pre-make-instance (tos.int:pre-make-class (list tos.classes:<t>)) :name symbol)))
             (setf (get symbol 'tos.int:object-value) object)
             object)))
   
   (defun tos:all-bound-object-symbols ()
      (remove-if-not #'(lambda (symbol)
                          (get symbol 'tos.int:object-value))
                     (lmud.int:all-symbols)))
   
   (defun tos:all-bound-objects ()
      (domap (symbol (tos:all-bound-object-symbols))
         (get symbol 'tos.int:object-value)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    Streams and I/O
   ;;;

   (tos:defclass io:<basic-stream> (tos.classes:<t>)
      (with (io:text-position 0)))

   (tos:defmethod (io:<basic-stream> io:advance-text-position) (&optional (amount 1))
      (incf .io:text-position amount))

   (tos:defmethod (io:<basic-stream> read-char) ()
      (.io:advance-text-position self 1)
      (io:read-utf8-char self))

   (tos:defmethod (io:<basic-stream> write-char) (char)
      (io:write-utf8-char char self))
   
   (tos:defmethod (io:<basic-stream> unread-char) (char)
      (.io:advance-text-position self -1))
   
   (tos:defmethod (io:<basic-stream> flush) () ())

   (tos:defmethod (io:<basic-stream> cursor-position) () (values nil nil))
   (tos:defmethod (io:<basic-stream> text-position)   () .io:text-position)

   (tos:defmethod (io:<basic-stream> disable-echo) () ())
   (tos:defmethod (io:<basic-stream> enable-echo)  () ())
   
   (tos:defmethod (io:<basic-stream> set-echo) (v)
      (if v
          (.enable-echo  self)
          (.disable-echo self)))


   (tos:defmethod (io:<port-stream> read-byte) ()
      (lmud.int:port-read-byte (.port self)))
   
   (tos:defmethod (io:<port-stream> write-byte) (byte)
      (lmud.int:port-write-byte (.port self) byte))
   
   (tos:defmethod (io:<port-stream> unread-char) (char)
      (.io:advance-text-position self -1)
      (lmud.int:port-unread-char (.port self) char))
   
   (tos:defmethod (io:<port-stream> eof-p) ()
      (lmud.int:port-eof-p (.port self)))
   
   (tos:defmethod (io:<port-stream> flush) ()
      (lmud.int:port-flush (.port self)))

   (tos:defmethod (io:<port-stream> close) ()
      (lmud.int:close-port (.port self)))


   (tos:defclass io:<wrapped-port-stream> (io:<port-stream>)
      (with (port nil)))
   
   (tos:defmethod (io:<wrapped-port-stream> construct) (port)
      (prog1 self
         (setf .port port)))
   
   (tos:defmethod (io:<wrapped-port-stream> port) () .port)

   (tos:defmethod (tos.classes:<port> port) () self)
   

   (tos:defclass io:<string-output-port> ()
      (with (chars nil)))
   
   (tos:defmethod (io:<string-output-port> write-char) (char)
      (push char .chars))
   
   (tos:defmethod (io:<string-output-port> build) ()
      (conversions:->string (reverse .chars)))
   
   (defmacro with-string-output-stream (var &body body)
      (list 'let (list (list var (list 'tos:make-instance 'io:<string-output-port>)))
         (cons 'progn body)
         (list 'tos:send var ''build)))

   
   (defun io:wrap-port (port)
      (.construct (tos:make-instance io:<wrapped-port-stream>) port))
   
   (defun io:unwrap-port (stream)
      (.port stream))

   (defun io:default-stream ()
      (lmud.int:current-port))

   (defun io:the-stream (stream)
      (or stream (io:default-stream)))
   
   (defun io:close-stream (stream)
      (.close stream))
   
   (defun io:raw-eof-p (stream)
      (.eof-p stream))
   
   (defun io:write-utf8-char (char stream)
      (let ((code (char-code char)))
         (cond ((< code #x80)    (.write-byte stream code))
               ((< code #x800)   (.write-byte stream (logior #xC0 (ash code -6)))
                                 (.write-byte stream (logior #x80 (logand code #x3F))))
               ((< code #x10000) (.write-byte stream (logior #xE0 (ash code -12)))
                                 (.write-byte stream (logior #x80 (logand (ash code -6) #x3F)))
                                 (.write-byte stream (logior #x80 (logand code #x3F))))
               (t                (.write-byte stream (logior #xF0 (ash code -18)))
                                 (.write-byte stream (logior #x80 (logand (ash code -12) #x3F)))
                                 (.write-byte stream (logior #x80 (logand (ash code -6) #x3F)))
                                 (.write-byte stream (logior #x80 (logand code #x3F)))))))
   
   (defun io:read-utf8-char (stream)
      (let ((byte (.read-byte stream)))
         (when byte
            (code-char
               (cond ((< byte #x80) byte)
                     ((< byte #xE0)
                      (let ((b1 (.read-byte stream)))
                         (and b1
                              (logior (ash (logand byte #x1F) 6)
                                      (logand b1 #x3F)))))
                     ((< byte #xF0)
                      (let ((b1 (.read-byte stream))
                            (b2 (.read-byte stream)))
                         (and b1
                              b2
                              (logior (ash (logand byte #x0F) 12)
                                      (ash (logand b1   #x3F)  6)
                                      (logand b2 #x3F)))))
                     ((< byte #xF8)
                      (let ((b1 (.read-byte stream))
                            (b2 (.read-byte stream))
                            (b3 (.read-byte stream)))
                         (and b1
                              b2
                              b3
                              (logior (ash (logand byte #x7) 18)
                                      (ash (logand b1 #x3F) 12)
                                      (ash (logand b2 #x3F) 6)
                                      (logand b3 #x3F)))))
                     (t (lmud.util:simple-error "Invalid UTF-8 encoding!")))))))

   (defun io:read-char-from-stream (stream)
      (.read-char stream))
   
   (defun io:write-char-to-stream (stream char)
      (.write-char stream char))
   
   (defun io:eof-p (&optional stream)
      (io:raw-eof-p (io:the-stream stream)))

   (defun write-byte (byte &optional (stream (io:default-stream)))
      (.write-byte stream byte))
   
   (defun read-byte (&optional (stream (io:default-stream)))
      (.read-byte stream))
   
   (defun write-char (char &optional stream)
      (io:write-char-to-stream (io:the-stream stream) char))
   
   (defun read-char (&optional stream)
      (io:read-char-from-stream (io:the-stream stream)))
   
   (defun unread-char (char &optional (stream (io:default-stream)))
      (.unread-char stream char))
   
   (defun peek-char (stream)
      (let ((char (read-char stream)))
         (unread-char char stream)
         char))
   
   (defun close (stream)
      (io:close-stream stream))
   

   (tos:defclass io.reader:<meta> ()
      (with (eof-error-p t)
            (eof-value   :eof)))
   
   (tos:defmethod (io.reader:<meta> eof-error-p) () .eof-error-p)
   (tos:defmethod (io.reader:<meta> eof-value)   () .eof-value)
   
   (defun io.reader:make-meta (&key (eof-error-p t) (eof-value nil))
      (let ((meta (tos:make-instance io.reader:<meta>)))
         (setf (tos::dot meta 'eof-error-p) eof-error-p)
         (setf (tos::dot meta 'eof-value)   eof-value)
         meta))

   (defun io.reader:breaking-char-p (char)
      (or (char= char (code-char 40))  ; '('
          (char= char (code-char 41))  ; ')'
          (char= char (code-char 91))  ; '['
          (char= char (code-char 93))  ; ']'
          (char= char (code-char 123)) ; '{'
          (char= char (code-char 125)) ; '}'
          (lmud.char:whitespacep char)
          (char= char #\Escape)))

   (defun io.reader:check (stream char)
      (let ((parsed-char (read-char stream)))
         (unless (characterp parsed-char)
            (return (values nil nil)))
         (if (char= parsed-char char)
             parsed-char
             (progn (unread-char parsed-char stream)
                    (values nil nil)))))
   
   (defun io.reader:checkpred (stream predicate)
      (let ((parsed-char (read-char stream)))
         (unless (characterp parsed-char)
            (return nil))
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
   
   (defun io.reader:read-until (stream predicate &key slurp-last)
      (let ((result '()))
         (while (not (io:eof-p stream))
            (let ((char (read-char stream)))
               (when (or (not (characterp char))
                         (funcall predicate char))
                  (unless slurp-last
                     (unread-char char stream))
                  (return (conversions:->string (reverse result))))
               (push char result)))
         (conversions:->string (reverse result))))

   (defun io.reader:read-at-least-one-until (stream predicate &key slurp-last)
      (let ((result '()))
         (while (not (io:eof-p stream))
            (let ((char (read-char stream)))
               (when (or (not (characterp char))
                         (and (funcall predicate char)
                              (not (null result))))
                  (unless slurp-last
                     (unread-char char stream))
                  (return (conversions:->string (reverse result))))
               (push char result)))
         (conversions:->string (reverse result))))
   
   (defun io.reader:read-until-char (stream char)
      (io.reader:read-until stream (lambda (c) (char= c char)) :slurp-last t))
   
   (defun io.reader:read-until-newline (stream)
      (io.reader:read-until-char stream #\Newline))

   (defun io.reader:skip-whitespace (stream)
      (io.reader:skip stream #'lmud.char:whitespacep))
   
   (defun io.reader:read-until-breaking-char (stream)
      (io.reader:read-until stream #'io.reader:breaking-char-p))
   
   (defun io.reader:read-at-least-one-until-breaking-char (stream)
      (io.reader:read-at-least-one-until stream #'io.reader:breaking-char-p))
   
   (defun io.reader:read-escaped (stream escape-sequence terminator)
      (let ((result '()))
         (while (not (io:eof-p stream))
            (cond ((io.reader:checkstr stream escape-sequence)
                   (push (let ((c (read-char stream)))
                           (unless (characterp c)
                              (error "Invalid escape sequence!"))
                           (case c
                              ((#\n) #\Newline)
                              ((#\t) #\Tab)
                              ((#\r) #\Return)
                              ((#\b) #\Backspace)
                              ((#\e) #\Escape)
                              ((#\\) #\\)
                              ((#\") #\")
                              (t c)))
                         result))
                  ((io.reader:checkstr stream terminator)
                   (return (conversions:->string (reverse result))))
                  (t (push (read-char stream) result))))
         (io.reader:eof-error stream)))
   
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

   (defun io.reader:char->digit (char &optional (base 10))
      (let ((value (cond ((and (char>= char #\0) (char<= char #\9))       (- (char-code char) (char-code #\0)))
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
                   (digit (and (characterp char)
                               (io.reader:char->digit char base))))
               (if digit
                   (setq value       (+ (* value base) digit)
                         digits-read (+ digits-read 1))
                   (progn (unread-char char stream)
                          (if (= digits-read 0)
                              (progn (when negative (unread-char #\- stream))
                                     (return nil))
                              (return (if negative (- value) value)))))))))

   (defun io.reader:parse-integer-core (chars base)
      (setq chars (conversions:->list chars))
      
      (let ((value       0)
            (digits-read 0))
         
         (while t
            (let* ((char  (car chars))
                   (digit (and (characterp char)
                               (io.reader:char->digit char base))))
               (if digit
                   (setq value       (+ (* value base) digit)
                         digits-read (+ digits-read 1))
                   (return (values (if (= digits-read 0)
                                       nil
                                       value)
                                   chars))))
            (pop chars))))
   
   (defun io.reader:parse-integer (chars &optional (base 10))
      (multiple-value-bind (number remaining-chars)
            (io.reader:parse-integer-core chars base)
         (and (null remaining-chars) number)))
   
   (defun io.reader:parse-trailing-fractional-part (chars base fraction denominator)
      (if (endp chars)
          (values fraction nil)
          (let* ((char  (car chars))
                 (digit (io.reader:char->digit char base)))
             (if digit
                 (io.reader:parse-trailing-fractional-part (cdr chars)
                                                           base
                                                           (+ fraction (/ digit denominator))
                                                           (* denominator base))
                 (values fraction chars)))))

   (defun io.reader:parse-unsigned-number (chars &optional (base 10))
      (multiple-value-bind (numerator remaining-chars)
            (io.reader:parse-integer-core chars base)
         (cond ((null remaining-chars) numerator)
               ((char= (car remaining-chars) #\/)
                (multiple-value-bind (denominator remaining-chars)
                     (io.reader:parse-integer-core (cdr remaining-chars) base)
                  (if (and denominator (null remaining-chars))
                      (values (/ numerator denominator) nil)
                      (values nil remaining-chars))))
               ((char= (car remaining-chars) #\.)
                (multiple-value-bind (fraction remaining-chars)
                     (io.reader:parse-trailing-fractional-part (cdr remaining-chars) base 0 base)
                  (if (null remaining-chars)
                      (if (minusp numerator)
                          (- numerator fraction)
                          (+ numerator fraction))
                      (values nil remaining-chars))))
               (t (values nil remaining-chars)))))
   
   (defun io.reader:parse-number (chars &optional (base 10))
      (let ((negative nil))
         (setq chars (conversions:->list chars))
         (cond ((char= (car chars) #\-)
                (pop chars)
                (setq negative t))
               ((char= (car chars) #\+)
                (pop chars)))
         (multiple-value-bind (number remaining-chars)
               (io.reader:parse-unsigned-number chars base)
            (if number
                (if negative
                    (- number)
                    number)
                nil))))

   (defun io.reader:read-list (stream meta)
      (io.reader:skip-whitespace stream)
      (cond ((io:eof-p stream) (io.reader:eof-error stream))
            ((io.reader:checkstr stream ")") nil)
            ((io.reader:checkstr stream ". ")
               (prog1 (io.reader:read stream meta)
                      (unless (io.reader:checkstr stream ")")
                         (lmud.util:simple-error "Expected ')' after '.'!"))))
            (t (cons (io.reader:read      stream meta)
                     (io.reader:read-list stream meta)))))
   
   (defun io.reader:read-sequence (stream meta terminator)
      (io.reader:skip-whitespace stream)
      (cond ((io:eof-p stream) (io.reader:eof-error stream))
            ((io.reader:checkstr stream terminator) nil)
            (t (cons (io.reader:read          stream meta)
                     (io.reader:read-sequence stream meta terminator)))))

   (defun io.reader:read-atom (stream meta)
      (let ((text (io.reader:read-until-breaking-char stream)))
         (when (= (length text) 0) (io.reader:eof-error stream))
         (or (io.reader:parse-number text)
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

   (defun io.reader:parse-character (stream)
      (lmud.char:character-by-name (io.reader:read-at-least-one-until-breaking-char stream)))
   
   (defun io.reader:parse-string (stream)
      (io.reader:read-escaped stream "\\" "\""))

   (defun io.reader:core-read (stream meta)
      (cond ((io:eof-p stream)
             (if (.eof-error-p meta)
                 (io.reader:eof-error stream)
                 (.eof-value meta)))
            ((io.reader:checkstr stream ";")
             (io.reader:skip stream (lambda (char) (not (char= char #\Newline))))
             (io.reader:read stream meta))
            ((io.reader:checkstr stream "(")
             (if (io.reader:checkstr stream ".")
                 (let ((message  (io.reader:read      stream meta))
                       (receiver (io.reader:read      stream meta))
                       (args     (io.reader:read-list stream meta)))
                    (list* 'tos:send
                           receiver
                           (list 'quote message)
                           args))
                 (io.reader:read-list stream meta)))
            ((io.reader:checkstr stream ":")
             (io.reader:read-keyword stream))
            ((io.reader:checkstr stream "'")
             (list 'quote (io.reader:read stream meta)))
            ((io.reader:checkstr stream "#'")
             (list 'function (io.reader:read stream meta)))
            ((io.reader:checkstr stream "`")
             (list 'lmud.qq:quasiquote (io.reader:read stream meta)))
            ((io.reader:checkstr stream ",@")
             (list 'lmud.qq:unquote-splice (io.reader:read stream meta)))
            ((io.reader:checkstr stream ",")
             (list 'lmud.qq:unquote (io.reader:read stream meta)))
            ((io.reader:checkstr stream "#\\")
             (io.reader:parse-character stream))
            ((io.reader:checkstr stream "\"")
             (io.reader:parse-string stream))
            ((io.reader:checkstr stream "#b")
             (io.reader:read-integer stream 2))
            ((io.reader:checkstr stream "#o")
             (io.reader:read-integer stream 8))
            ((io.reader:checkstr stream "#x")
             (io.reader:read-integer stream 16))
            ((io.reader:checkstr stream "~")
             (list 'tos:find (list 'quote (io.reader:read stream meta))))
            ((io.reader:checkstr stream ".")
             (list 'tos:dot 'self (list 'quote (io.reader:read stream meta))))
            ((io.reader:checkstr stream "[")
             (let ((sequence (io.reader:read-sequence stream meta "]")))
               (list* 'tos:send
                      (car sequence)
                      (list 'quote (cadr sequence))
                      (cddr sequence))))
            (t (io.reader:read-atom stream meta))))
   
   (defun io.reader:read (stream meta)
      (io.reader:skip-whitespace stream)
      (let ((start (.text-position stream))
            (data  (io.reader:core-read stream meta))
            (end   (.text-position stream)))
         ;; TODO: Report position and content
         data))
   
   (defun io.reader:begin-read (stream &rest rest)
      (io.reader:read stream (apply #'io.reader:make-meta rest)))

   (defun read (&optional (stream (io:default-stream)) &rest rest)
      (apply #'io.reader:begin-read stream rest))
   
   (defun read-line (&optional stream)
      (io.reader:read-until (io:the-stream stream) #'lmud.char:newlinep :slurp-last t))
   
   (defun io:parse-number (chars &optional (base 10))
      (io.reader:parse-number chars base))

   (defun io:parse-number-from-stream (&optional stream (base 10))
      (let* ((stream (io:the-stream stream))
             (chars  (io.reader:read-until stream #'lmud.char:whitespacep)))
         (io.reader:parse-number chars base)))


   (defun io.printer:meta-escaped-p (meta) meta)

   (defun io.printer:write-char (stream meta char)
      (write-char char stream))

   (defun io.printer:write-string (stream meta string)
      (dosequence (char string)
         (write-char char stream)))

   (defun io.printer:print-unsigned-nonzero-integer (stream meta n)
      (when (> n 0)
         (io.printer:print-unsigned-nonzero-integer stream meta (truncate n 10))
         (io.printer:write-char stream
                                meta
                                (code-char (+ (char-code #\0)
                                              (mod n 10))))))

   (defun io.printer:print-integer (stream meta integer &optional (base 10))
      (cond ((= integer 0) (write-char #\0 stream))
            ((< integer 0) (write-char #\- stream)
                           (io.printer:print-unsigned-nonzero-integer stream meta (- integer)))
            (t             (io.printer:print-unsigned-nonzero-integer stream meta integer))))
   
   (defun io.printer:print-floating-point-fractional-digits (stream meta fractional-part)
      (until (= fractional-part 0)
         (setq fractional-part (* fractional-part 10))
         (let* ((digit         (truncate fractional-part))
                (rest          (- fractional-part digit)))
            (io.printer:write-char stream
                                   meta
                                   (code-char (+ (char-code #\0)
                                                 (mod digit 10))))
            (setq fractional-part rest))))

   (defun io.printer:print-floating-point (stream meta ratio)
      "Print a ratio as a floating point number"
      (when (< ratio 0)
         (write-char #\- stream)
         (setq ratio (- ratio)))
         (let* ((integer-part  (truncate ratio))
                (fraction-part (- ratio integer-part)))
            (io.printer:print-integer stream meta integer-part)
            (write-char #\. stream)
            (io.printer:print-floating-point-fractional-digits stream meta fraction-part)))

   (defun io.printer:print-ratio (stream meta ratio)
      (io.printer:print-integer stream meta (numerator ratio))
      (io.printer:write-char stream meta #\/)
      (io.printer:print-integer stream meta (denominator ratio)))

   (defun io.printer:can-be-printed-as-floating-point-p (ratio)
      (when (< ratio 0) (setq ratio (- ratio)))
      (setq ratio (- ratio (truncate ratio)))
      (let ((count 20))
         (until (zerop ratio)
            (when (zerop count) (return nil))
            (setq ratio (* ratio 10))
            (setq ratio (- ratio (truncate ratio)))
            (decf count))
         t))

   (defun io.printer:print-rational (stream meta ratio)
      (if (io.printer:can-be-printed-as-floating-point-p ratio)
          (io.printer:print-floating-point stream meta ratio)
          (io.printer:print-ratio          stream meta ratio)))

   (defun io.printer:print-character (stream meta character)
      (if (io.printer:meta-escaped-p meta)
          (progn (io.printer:write-string stream meta "#\\")
                 (let ((name (lmud.char:character-name-or-nil character)))
                    (if name
                        (io.printer:write-string stream meta name)
                        (if (lmud.char:printable-char-p character)
                            (io.printer:write-char stream meta character)
                              (progn (io.printer:write-string stream meta "U+")
                                     (io.printer:print-integer stream meta (char-code character) 16))))))
          (write-char character stream)))
   
   (defun io.printer:print-string (stream meta string)
      (if (io.printer:meta-escaped-p meta)
          (progn (io.printer:write-char stream meta #\")
                 (dosequence (char string)
                    (cond ((char= char #\")      (io.printer:write-string stream meta "\\\""))
                          ((char= char #\\)      (io.printer:write-string stream meta "\\\\"))
                          ((char= char #\Escape) (io.printer:write-string stream meta "\\e"))
                          (t (io.printer:write-char stream meta char))))
                 (io.printer:write-char stream meta #\"))
          (io.printer:write-string stream meta string)))
   
   (defun io.printer:print-symbol (stream meta symbol)
      (if (lmud.int:gensymp symbol)
          (io.printer:write-string stream meta "#<GENSYM>")
          (let ((package (symbol-package symbol))
                (name    (symbol-name    symbol)))
             (cond ((eq package lmud:*default-package*) nil)
                   ((eq package lmud:*keyword-package*)
                    (io.printer:write-char stream meta #\:))
                   (t (io.printer:write-string stream meta (package-name package))
                      (io.printer:write-string stream meta "::")))
             (io.printer:write-string stream meta name))))
   
   (defun io.printer:print-list-body (stream meta list)
      (while (consp list)
         (io.printer:write-char       stream meta #\Space)
         (io.printer:print-expression stream meta (car list))
         (setq list (cdr list)))
      (unless (null list)
         (io.printer:write-string stream meta " . ")
         (io.printer:print-expression stream meta list)))

   (defun io.printer:print-list (stream meta list)
      (flet ((quote-clause-p (xlist)
               (and (consp xlist)
                    (eq (car xlist) 'quote)
                    (consp (cdr xlist))
                    (null (cddr xlist))))
             (function-clause-p (xlist)
               (and (consp xlist)
                    (eq (car xlist) 'function)
                    (consp (cdr xlist))
                    (null (cddr xlist))))
             (quasiquote-clause-p (xlist)
               (and (consp xlist)
                    (eq (car xlist) 'lmud.qq:quasiquote)
                    (consp (cdr xlist))
                    (null (cddr xlist))))
             (unquote-clause-p (xlist)
               (and (consp xlist)
                    (eq (car xlist) 'lmud.qq:unquote)
                    (consp (cdr xlist))
                    (null (cddr xlist))))
             (unquote-splice-clause-p (xlist)
               (and (consp xlist)
                    (eq (car xlist) 'lmud.qq:unquote-splice)
                    (consp (cdr xlist))
                    (null (cddr xlist)))))
         (cond ((null list) (io.printer:write-string stream meta "()"))
               ((quote-clause-p list) (io.printer:write-string stream meta "'")
                                      (io.printer:print-expression stream meta (cadr list)))
               ((function-clause-p list) (io.printer:write-string stream meta "#'")
                                         (io.printer:print-expression stream meta (cadr list)))
               ((quasiquote-clause-p list) (io.printer:write-string stream meta "`")
                                           (io.printer:print-expression stream meta (cadr list)))
               ((unquote-clause-p list) (io.printer:write-string stream meta ",")
                                        (io.printer:print-expression stream meta (cadr list)))
               ((unquote-splice-clause-p list) (io.printer:write-string stream meta ",@")
                                               (io.printer:print-expression stream meta (cadr list)))
               ((consp list) (io.printer:write-string     stream meta "(")
                             (io.printer:print-expression stream meta (car list))
                             (io.printer:print-list-body  stream meta (cdr list))
                             (io.printer:write-string    stream meta ")"))
               (t (lmud.util:simple-error "Not a list!")))))

   (defun io.printer:print-vector (stream meta vector)
      (io.printer:write-string stream meta "#(")
      (dotimes (i (length vector))
         (io.printer:print-expression stream meta (aref vector i))
         (when (< i (- (length vector) 1))
            (io.printer:write-char stream meta #\Space)))
      (io.printer:write-string stream meta ")"))
   
   (defun io.printer:print-bytes (stream meta vector)
      (io.printer:write-string stream meta "#B(")
      (dotimes (i (length vector))
         (io.printer:print-expression stream meta (aref vector i))
         (when (< i (- (length vector) 1))
            (io.printer:write-char stream meta #\Space)))
      (io.printer:write-string stream meta ")"))

   (defun io.printer:print-expression (stream meta e)
      (cond ((symbolp            e) (io.printer:print-symbol    stream meta e))
            ((consp              e) (io.printer:print-list      stream meta e))
            ((integerp           e) (io.printer:print-integer   stream meta e))
            ((characterp         e) (io.printer:print-character stream meta e))
            ((stringp            e) (io.printer:print-string    stream meta e))
            ((vectorp            e) (io.printer:print-vector    stream meta e))
            ((lmud.int:bytesp    e) (io.printer:print-bytes     stream meta e))
            ((lmud.int:ratiop    e) (io.printer:print-rational  stream meta e))
            ((lmud.int:machine-function-p e)
             (io.printer:write-string stream meta "#<MACHINE-CODE-FUNCTION ")
             (io.printer:write-string stream meta (lmud.int:machine-function-name e))
             (io.printer:write-string stream meta ">"))
            ((lmud.int:bytecode-function-p e)
             (io.printer:write-string stream meta "#<BYTE-COMPILED-FUNCTION>"))
            ((lmud.int:closurep e)
             (io.printer:write-string stream meta "#<CLOSURE>"))
            ((lmud.int:portp e)
             (io.printer:write-string stream meta "#<PORT>"))
            ((lmud.int:processp e)
             (io.printer:write-string stream meta "#<PROCESS ")
             (io.printer:print-expression stream meta (lmud.int:process-state e))
             (io.printer:write-string stream meta ">"))
            ((lmud.int:stack-frame-p e)
             (io.printer:write-string stream meta "#<STACK-FRAME>"))
            ((tos.int:classp e)
             (let ((class-name (tos.int:%class-name e)))
                (if class-name
                    (progn (io.printer:write-string stream meta "#S(CLASS ")
                           (io.printer:print-expression stream meta class-name)
                           (io.printer:write-string stream meta ")"))
                    (io.printer:write-string stream meta "#S(CLASS)"))))
            ((tos.int:oopp e)
             (let ((name (tos.int:%object-name e)))
                (if name
                    (progn (io.printer:write-string stream meta "~")
                           (io.printer:print-expression stream meta name))
                    (progn (io.printer:write-string stream meta "#S(")
                           (io.printer:print-expression stream meta (lmud.int:%custom-meta e))
                           (io.printer:write-string stream meta ")")))))
            ((lmud.int:%customp e)
             (io.printer:write-string stream meta "#<CUSTOM>"))
            (t (io.printer:write-string stream meta "#<UNKNOWN>"))))

   (defun io.printer:prin1 (stream e)
      (io.printer:print-expression stream t e))
   
   (defun io.printer:princ (stream e)
      (io.printer:print-expression stream nil e))
   
   (defun io.printer:terpri (stream)
      (io.printer:write-char stream nil #\Return)
      (io.printer:write-char stream nil #\Newline))
   
   (defun io.printer:fresh-line (stream)
      (multiple-value-bind (x y) (.cursor-position stream)
         (unless (and x (= x 0))
             (io.printer:terpri stream))))

   (defun prin1 (e &optional stream)
      (io.printer:prin1 (io:the-stream stream) e))
   
   (defun princ (e &optional stream)
      (io.printer:princ (io:the-stream stream) e))
   
   (defun terpri (&optional stream)
      (io.printer:terpri (io:the-stream stream)))
   
   (defun fresh-line (&optional stream)
      (io.printer:fresh-line (io:the-stream stream)))
   
   (defun cursor-position (&optional (stream (io:default-stream)))
      (.cursor-position stream))

   (defun io:uformat (stream format-string &rest args)
      (when (null stream)
         (return (with-string-output-stream string-stream
                    (apply #'io:uformat string-stream format-string args))))
      (setq stream
            (io:the-stream (cond ((eq stream nil) nil)
                                 ((eq stream t)   nil)
                                 (t               stream))))
      (let ((index 0)
            (cap   (length format-string)))
         (while (< index cap)
            (let ((char (aref format-string index)))
               (if (and (char= char #\~) (< (1+ index) cap)) 
                   (progn (incf index)
                          (let ((directive (aref format-string index)))
                             (cond ((char= directive #\%) (terpri stream))
                                   ((char= directive #\&) (fresh-line stream))
                                   ((char= directive #\a) (princ (pop args) stream))
                                   ((char= directive #\s) (prin1 (pop args) stream))
                                   (t (lmud.util:simple-error "Unknown format directive!")))))
                   (princ char stream))
               (incf index)))))
   
   (defun format (&ignore-rest)
      (lmud.int:funcall-forward #'io:uformat))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;
   ;;;    REPL and Tools
   ;;;

   (defun error (&ignore-rest)
      (lmud.int:funcall-forward #'lmud.util:simple-error))

   (defun lmud:log (level format)
      (let ((message format))
         (lmud.int:log (case level
                          ((:all)                    0)
                          ((:debug-full :full-debug) 1)
                          ((:debug-half :half-debug) 2)
                          ((:debug)                  3)
                          ((:note)                   4)
                          ((:warning)                5)
                          ((:error)                  6)
                          ((:info)                   7)
                          ((:fatal)                  8)
                          (t (lmud.util:simple-error "Unknown log level!")))
                       message)))

   (defun load (path)
      (let ((start-time (lmud.int:get-clock))
            (port       (lmud.int:open-file path)))
         (unless port (lmud.util:simple-error "Could not open file!"))
         (setq port (io:wrap-port port))
         (lmud:log :note (string:concatenate "Loading file '" path "' ..."))
         (while t
            (let ((expr (read port :eof-error-p nil :eof-value :eof)))
               (if (eq expr :eof)
                   (progn (lmud:log :info (io:uformat nil "Loaded file '~a'! (~a s)" path (/ (- (lmud.int:get-clock) start-time) 1000)))
                          (return (values)))
                   (eval expr))))))

   
   (defun load-kernel-module (path)
      (load (string:concatenate "../lisp/kernel/" path ".lisp")))
   
   (defun load-kernel-modules (&rest paths)
      (dolist (path paths)
         (load-kernel-module path)))

   (load-kernel-module "master")

   ))
)
