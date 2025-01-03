
(defun square (x) (* x x))

(defun disassembler:disassemble-bytecode-function (function)
   (let ((bytecodes (conversions:->list (lmud.int:function-bytecodes function)))
         (constants (lmud.int:function-constants function))
         (result    '())
         (offsets   '())
         (offset      0))
      (macrolet ((pop-u8    ()  `(progn (incf offset) (pop bytecodes)))
                 (pop-u16   ()  `(logior (pop-u8) (ash (pop-u8) 8)))
                 (pop-const-small ()  `(aref constants (pop-u8)))
                 (pop-const ()  `(aref constants (pop-u16)))
                 (pop-label ()  `(pop-u16))
                 (instr (i)
                    (let ((v (gensym)))
                       `(let ((,v ,i))
                           (push (cons ,v current-offset) offsets)
                           (push ,v result)))))
         (while bytecodes
            (let ((current-offset  offset)
                  (bytecode        (pop-u8)))
               (case bytecode
                  ((0)  (instr `(nop)))
                  ((1)  (instr `(has-argument)))
                  ((2)  (instr `(pop-argument)))
                  ((3)  (instr `(pop-keyword-argument ,(pop-const))))
                  ((4)  (instr `(cons-rest-arguments)))
                  ((5)  (let ((amount (pop-u8))
                              (lst   '()))
                           (dotimes (i amount)
                              (push (pop-u8) lst))
                           (instr `(multiple-value-bind ,amount ,@(nreverse lst)))))
                  ((6)  (instr `(multiple-value-list)))
                  ((7)  (instr `(load-constant-small ,(pop-const-small))))
                  ((8)  (instr `(load-constant ,(pop-const))))
                  ((9)  (instr `(lambda ,(pop-const))))
                  ((10) (instr `(load-register-local ,(pop-u8))))
                  ((11) (instr `(load-register-lexical ,(pop-u8) ,(pop-u8))))
                  ((12) (instr `(load-symbol-variable ,(pop-const))))
                  ((13) (instr `(load-symbol-function ,(pop-const))))
                  ((14) (instr `(store-register-local ,(pop-u8))))
                  ((15) (instr `(store-register-lexical ,(pop-u8) ,(pop-u8))))
                  ((16) (instr `(store-symbol-variable ,(pop-const))))
                  ((17) (instr `(store-symbol-function ,(pop-const))))
                  ((18) (instr `(push)))
                  ((19) (instr `(call ,(pop-u8))))
                  ((20) (instr `(jump ,(pop-label))))
                  ((21) (instr `(jump-if-nil ,(pop-label))))
                  ((22) (instr `(return)))
                  ((23) (instr `(set-unwind-protect ,(pop-label))))
                  ((24) (instr `(begin-unwind-protect)))
                  ((25) (instr `(end-unwind-protect)))
                  ((26) (instr `(begin-signal-handler ,(pop-u8) ,(pop-label))))
                  (t (instr '?)))))
         (values (nreverse result)
                 offsets))))

(defun disassemble (function &optional stream)
   (unless (lmud.int:bytecode-function-p function)
      (lmud.util:simple-error "Not a bytecode-compiled function!"))
   (io:uformat t "~&~%Bytecodes for ~s:~%" function)
   (prin1 (lmud.int:function-bytecodes function)) (terpri)
   (io:uformat t "~&~%Constants:~%")
   (let ((constants (lmud.int:function-constants function)))
      (dotimes (i (length constants))
         (io:uformat t "~&  [~s]: ~s~%" i (aref constants i))))
   (io:uformat t "~&~%Disassembly:~%")
   (multiple-value-bind (codes offsets)
         (disassembler:disassemble-bytecode-function function)
      (dolist (instr codes)
         (io:uformat t "~& [~s]:   ~s~%"
            (let ((handle (assoc instr offsets)))
               (if handle (cdr handle) '?))
            instr))))

(defun tools:stack-trace-process (process)
   (let ((frame (lmud.int:process-stack-frames process)))
      (until (null frame)
         (io:uformat t "~&---~%")
         (io:uformat t "~&Function: ~s~%" (lmud.int:stack-frame-function frame))
         (io:uformat t "~&IP:       ~s~%" (lmud.int:stack-frame-ip frame))
         (setq frame (lmud.int:stack-frame-previous frame)))))

(defun tools:all-functions ()
   (remove-if-not #'fboundp (lmud.int:all-symbols)))


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
