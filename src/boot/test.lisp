
(defun square (x) (* x x))

(defun disassemble (function &optional stream)
   (unless (lmud.int:bytecode-function-p function)
      (lmud.util:simple-error "Not a bytecode-compiled function!"))
   (io:uformat t "~&~%Bytecodes for ~s:~%" function)
   (prin1 (lmud.int:function-bytecodes function)) (terpri)
   (io:uformat t "~&~%Constants:~%")
   (let ((constants (lmud.int:function-constants function)))
      (dotimes (i (length constants))
         (io:uformat t "~&  [~s]: ~s~%" i (aref constants i)))))

(defun stack-trace-process (process)
   (let ((frame (lmud.int:process-stack-frames process)))
      (until (null frame)
         (io:uformat t "~&---~%")
         (io:uformat t "~&Function: ~s~%" (lmud.int:stack-frame-function frame))
         (io:uformat t "~&IP:       ~s~%" (lmud.int:stack-frame-ip frame))
         (setq frame (lmud.int:stack-frame-previous frame)))))

(defun slurp-port (port)
   (let ((expression (read port :eof-error-p nil :eof-value :eof)))
      (if (eq expression :eof)
          nil
          (cons expression (slurp-port port)))))

(tos:defclass <point> ()
   (with (x 0)
         (y 0)))

(tos:defmethod (<point> get-x) () .x)
(tos:defmethod (<point> get-y) () .y)

(tos:defmethod (<point> set-x) (v) (setf .x v))
(tos:defmethod (<point> set-y) (v) (setf .y v))


(tos:defclass <3d-mixin> ()
   (with (z 0)))

(tos:defmethod (<3d-mixin> get-z) () .z)
(tos:defmethod (<3d-mixin> set-z) (v) (setf .z v))

(tos:defclass <3d-point> (<point> <3d-mixin>))

(defvar *p* (tos.int:pre-make-instance <3d-point>))

(defun point-values (p)
   (list (tos:send p 'get-x)
         (tos:send p 'get-y)
         (tos:send p 'get-z)))
