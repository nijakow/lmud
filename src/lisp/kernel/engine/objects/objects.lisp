
(tos:define-class-hook name (class &rest args)
   (list `(tos.int:%class-push-var! ,class 'name (.construct (tos:make-instance <name>) ,@args))))

(tos:define-class-hook n-to (class &rest args)
   (list `(game:push-direction! ,class 'n-to (progn ,@args))))

(tos:define-class-hook s-to (class &rest args)
   (list `(game:push-direction! ,class 's-to (progn ,@args))))

(tos:define-class-hook e-to (class &rest args)
   (list `(game:push-direction! ,class 'e-to (progn ,@args))))

(tos:define-class-hook w-to (class &rest args)
   (list `(game:push-direction! ,class 'w-to (progn ,@args))))

(tos:define-class-hook up-to (class &rest args)
   (list `(game:push-direction! ,class 'up-to (progn ,@args))))

(tos:define-class-hook down-to (class &rest args)
   (list `(game:push-direction! ,class 'down-to (progn ,@args))))


(tos:defclass <object> ()
   (with (name     (tos:make-instance <name>))
         (parent   nil)
         (children '())
         (directions '())))

(defun game:push-direction! (class direction value)
   nil)

(tos:defclass <room> (<object>))

(tos:defmethod (<object> game:parent)   () .parent)
(tos:defmethod (<object> game:children) () .children)

(defalias make          tos:make-instance)
(defalias define-method tos:defmethod)

(defmacro define-class (name supers &rest body)
   `(tos:defclass ,name ,(or supers '(<object>))
      ,@body))

(defmacro define-object (name supers &rest body)
   `(tos:defobject ,name ,(or supers '(<object>))
      ,@body))

(defmacro define-room (name supers &rest body)
   `(tos:defobject ,name ,(or supers '(<room>))
      ,@body))



(tos:defmethod (<object> game:unlink-child) (child)
   (setf .children (remove child .children)))

(tos:defmethod (<object> game:unlink) ()
   (when .parent
      (.game:unlink-child .parent self)
      (setf .parent nil)))

(tos:defmethod (<object> game:move) (new-parent)
   (.game:unlink self)
   (setf .parent new-parent)
   (when new-parent
      (push self (tos:dot new-parent 'children))))

(defun parent   (e) (.game:parent   e))
(defun children (e) (.game:children e))

(defun article (e)
   (tos:dot (tos:dot e 'name) 'article))

(defun short-name (e)
   (tos:dot (tos:dot e 'name) 'short))

(defun has-parent? (e)
   (not (null (.game:parent e))))

(defun directly-in? (a b)
   (member a (children b)))

(defun in? (a b)
   (or (is-directly-in? a b)
       (and (has-parent? a)
            (in? (parent a) b))))

(defun unlink (a) (.game:unlink a))
(defun move (a b) (.game:move a b))

(defun environment (e)
   (parent e))

(define-method (<object> describe) ()
   (tell (:p "You see nothing special.")))
