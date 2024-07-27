
(defun tell:color-code (color)
   (case color
      ((:black)   "30")
      ((:red)     "31")
      ((:green)   "32")
      ((:yellow)  "33")
      ((:blue)    "34")
      ((:magenta) "35")
      ((:cyan)    "36")
      ((:white)   "37")
      (t          nil)))

(defun tell:compile-color (color body stream-var)
   (let ((color-code (tell:color-code color)))
      (unless color-code
         (error "Invalid color code!"))
      `(progn (princ ,(string::concatenate "\e[" color-code "m") ,stream-var)
              (unwind-protect
                    (progn ,@(tell:compile-forms body stream-var))
                 (princ "\e[0m" ,stream-var))))) ;; TODO: Keep a stack of active effects

(defun tell:compile-tag (tag props args stream-var)
   (case tag
      ((:text) `(progn ,@(tell:compile-forms args stream-var)))
      ((:p)    `(progn ,@(tell:compile-forms args stream-var) (terpri ,stream-var)))
      ((:color) (tell:compile-color (car props) args stream-var))
      ((:lisp) `(princ (progn ,@args) ,stream-var))
      (t        (error "Unknown tell tag: ~S" tag))))

(defun tell:compile-form (form stream-var)
   (cond ((consp form)
          (multiple-value-bind (tag props args)
               (if (consp (car form))
                   (values (caar form) (cdar form) (cdr form))
                   (values (car form) nil (cdr form)))
            (tell:compile-tag tag props args stream-var)))
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
