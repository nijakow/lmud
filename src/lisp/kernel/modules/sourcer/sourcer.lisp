
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

(defun sourcer:retrieve-defun (name)
   (sourcer:retrieve-definition name 'sourcer:defun-definition))

(defun sourcer:retrieve-defmacro (name)
   (sourcer:retrieve-definition name 'sourcer:defmacro-definition))


(defun sourcer:edit-definition (definition-path definition-kind)
   (let ((definition (sourcer:retrieve-definition definition-path definition-kind)))
      (unless definition
         (error "Definition not found!"))
      (let ((source-handle (assoc :source definition)))
         (unless source-handle
            (error "Source not available!"))
         (genius:edit-expression (cdr source-handle)))))

(defun sourcer:edit-function (name)
   (sourcer:edit-definition name 'sourcer:defun-definition))

(defun sourcer:edit-macro (name)
   (sourcer:edit-definition name 'sourcer:defmacro-definition))
