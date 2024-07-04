
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
