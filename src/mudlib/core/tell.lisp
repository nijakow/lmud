
(defun tell:compile-form (form stream-var)
   (cond ((consp form)
          (let ((tag  (car form))
                (args (cdr form)))
             (case tag
               ((:text) `(progn ,@(tell:compile-forms args stream-var)))
               ((:p)    `(progn ,@(tell:compile-forms args stream-var) (terpri ,stream-var)))
               ((:lisp) `(princ (progn ,@args) ,stream-var))
               (t        (error "Unknown tell tag: ~S" tag)))))
         (t `(princ ,form ,stream-var))))

(defun tell:compile-forms (forms stream-var)
   (domap (form forms)
      (tell:compile-form form stream-var)))

(defmacro tell-on-stream (stream &rest forms)
   (let ((stream-var (gensym)))
      `(let ((,stream-var ,stream))
          ,@(tell:compile-forms forms stream-var))))

(defmacro tell (&rest forms)
   `(tell-on-stream (lmud.int:current-port) ,@forms))
