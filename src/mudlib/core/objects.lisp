
(tos:defclass <object> ()
   (with (parent   nil)
         (children '())))

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

(defun parent   (e) (.parent   e))
(defun children (e) (.children e))

(defun has-parent? (e)
   (not (null (.parent e))))

(defun directly-in? (a b)
   (member a (children b)))

(defun in? (a b)
   (or (is-directly-in? a b)
       (and (has-parent? a)
            (in? (parent a) b))))

(defun unlink (a) (.game:unlink a))
(defun move (a b) (.game:move b a))

(defalias make          tos:make-instance)
(defalias define-method tos:defmethod)

(defmacro define-class (name supers &rest body)
   `(tos:defclass ,name ,(or supers '(<object>))
      ,@body))

(defmacro define-object (name supers &rest body)
   `(tos:defobject ,name ,(or supers '(<object>))
      ,@body))



(define-method (<object> describe) ()
   (tell (:p "You see nothing special.")))
