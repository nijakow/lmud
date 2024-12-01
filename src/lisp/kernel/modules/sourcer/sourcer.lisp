
(defun sourcer:register-symbol-definition (symbol definition-kind full-source)
   (setf (get symbol definition-kind)
         (list (cons :name   symbol)
               (cons :source full-source))))

(defun sourcer:register-definition (definition-path definition-kind full-source)
   (when (symbolp definition-path)
      (sourcer:register-symbol-definition definition-path definition-kind full-source)))

(defun sourcer:register-defun (name args full-source)
   (sourcer:register-definition name 'sourcer:defun-definition full-source))

(defun sourcer:register-defmacro (name args full-source)
   (sourcer:register-definition name 'sourcer:defmacro-definition full-source))


(defun sourcer:retrieve-symbol-definition (symbol definition-kind)
   (get symbol definition-kind))

(defun sourcer:retrieve-definition (definition-path definition-kind)
   (cond ((symbolp definition-path) (sourcer:retrieve-symbol-definition definition-path definition-kind))
         (t (error "Definition could not be retrieved!"))))

(defun sourcer:defined-p (definition-path definition-kind)
   (if (sourcer:retrieve-definition definition-path definition-kind)
      t
      nil))

(defun sourcer:retrieve-defun (name)
   (sourcer:retrieve-definition name 'sourcer:defun-definition))

(defun sourcer:retrieve-defmacro (name)
   (sourcer:retrieve-definition name 'sourcer:defmacro-definition))


(defun sourcer:default-template-function (definition-path definition-kind)
   (case definition-kind
      ((sourcer:defun-definition)
       `(defun ,definition-path ()
           "Documentation"))
      ((sourcer:defmacro-definition)
       `(defmacro ,definition-path ()
           "Documentation"))
      (t "No template available!")))

(defun sourcer:edit-definition (definition-path definition-kind &key (template-function #'sourcer:default-template-function))
   (let ((definition (sourcer:retrieve-definition definition-path definition-kind)))
      (let ((source-handle (assoc :source definition)))
         (genius:edit-expression (if source-handle
                                     (cdr source-handle)
                                     (funcall template-function definition-path definition-kind))))))


(defun sourcer:edit-function (name)
   (sourcer:edit-definition name 'sourcer:defun-definition))

(defun sourcer:edit-macro (name)
   (sourcer:edit-definition name 'sourcer:defmacro-definition))
