
(defvar game:*commands* '())

(defmacro game:defcommand (id pattern &body body)
   (flet ((extract-args (pattern)
            (remove-if-not #'symbolp pattern))
          (create-alist-let (bindings-var vars body)
            `(let (,@(mapcar (lambda (var)
                               `(,var (cdr (assoc ',var ,bindings-var))))
                            vars))
               ,@body)))
      (let ((vars (extract-args pattern))
            (bindings-var (gensym)))
         `(progn
             (push (cons (cons ',id ',pattern)
                         (lambda (,bindings-var)
                           ,(create-alist-let bindings-var vars body)))
                   game:*commands*)))))

(defun game:find-command (input)
   (dolist (command game:*commands*)
      (multiple-value-bind (match-p bindings)
            (game:match-basic-pattern input (cdar command) nil)
         (if match-p
            (return (values command bindings))))))

(defun game:run-command (command bindings)
   (funcall (cadr command) bindings))

(defun game:compare-pattern-elements (input-element pattern-element)
   (string= input-element pattern-element :key #'char-upcase))

(defun game:match-basic-pattern (input pattern bindings-pass)
   (cond ((endp pattern)
          (if (endp input)
              (values t bindings-pass)
              (values nil nil)))
         ((endp input)
          (values nil nil))
         ((game:compare-pattern-elements (car input) (car pattern))
          (game:match-basic-pattern (cdr input) (cdr pattern) bindings-pass))
         ((symbolp (car pattern))
          (if (endp (cdr pattern))
              (values t (cons (cons (car pattern) input) bindings-pass))
              (multiple-value-bind (match-p bindings)
                  (game:match-basic-pattern (cdr input) (cdr pattern) bindings-pass)
                (if match-p
                    (let ((binding (assoc (car pattern) bindings)))
                       (if binding
                           (setf (cdr binding) (cons (car input) (cdr binding)))
                           (push (cons (car pattern) (list (car input))) bindings))
                       (values t bindings))
                    (values nil nil)))))
         (t (values nil nil))))


(game:defcommand :hello ("hello")
   (io:uformat t "Hello!~%"))

(game:defcommand :pick-up ("pick" "up" item)
   (io:uformat t "You pick up ~a.~%" item))
