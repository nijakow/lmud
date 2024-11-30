
(tos:defclass genius::<genius-reporter> (io.reader:<reporter>))

(tos:defmethod (genius::<genius-reporter> report-data) (data &key from to)
   (lmud.dummy:%princ "[GENIUS] From: ")
   (lmud.dummy:%prin1 from)
   (lmud.dummy:%princ " To: ")
   (lmud.dummy:%prin1 to)
   (lmud.dummy:%princ " Data: ")
   (lmud.dummy:%prin1 data)
   (lmud.dummy:%princ " / ")
   (lmud.dummy:%prin1 (car .openings))
   (lmud.dummy:%terpri))

(defun genius::make-reporter ()
   (tos:make-instance genius::<genius-reporter>))

(defun genius:analyze-string (text)
   (let ((stream   (io:make-string-stream text))
         (reporter (genius::make-reporter)))
      (let* ((eof-value nil)
             (result (read stream :reporter    reporter
                                  :eof-error-p nil
                                  :eof-value   eof-value)))
         (if (eq result eof-value)
             (progn (lmud.dummy:%princ "EOF")
                    (lmud.dummy:%terpri))))))

(defun genius:read-line ()
   (readline:read-line :prompt           "@ "
                       :analyze-function #'genius:analyze-string))
