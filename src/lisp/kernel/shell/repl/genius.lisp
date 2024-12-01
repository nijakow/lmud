
(tos:defclass genius::<genius-reporter> (io.reader:<reporter>)
   (with (tokens nil)))

(tos:defmethod (genius::<genius-reporter> report-data) (data &key from to)
   (push (cons (cons from to) (cons :object data)) .tokens))

(tos:defmethod (genius::<genius-reporter> report-comment) (data &key from to)
   (push (cons (cons from to) (cons :comment data)) .tokens))

(tos:defmethod (genius::<genius-reporter> get-tokens) ()
   .tokens)

(defun genius::make-reporter ()
   (tos:make-instance genius::<genius-reporter>))

(defun genius:generate-styles (tokens)
   (mapcar (lambda (token)
              (let ((range  (car  token))
                    (type   (cadr token))
                    (object (cddr token)))
                 (cons range
                       (cond ((eq type :comment)  (list #'vt100-styles:italic
                                                        #'vt100-styles:fg-dark-gray))
                             ((numberp object)    (list #'vt100-styles:fg-red))
                             ((stringp object)    (list #'vt100-styles:fg-yellow))
                             ((characterp object) (list #'vt100-styles:fg-green))
                             ((symbolp object)
                              (let ((color (if (special-operator-p object)
                                               #'vt100-styles:fg-cyan
                                               #'vt100-styles:fg-blue)))
                                 (if (or (fboundp object)
                                         (symbol-macro object))
                                     (list #'vt100-styles:underline
                                           color)
                                     (list color))))
                             ((consp object)
                              (cond ((eq (car object) 'quote) (list #'vt100-styles:fg-blue))))
                             (t '())))))
           tokens))

(defun genius:analyze-string (text)
   (let ((stream   (io:make-string-stream text))
         (reporter (genius::make-reporter)))
      (let* ((eof-value nil)
             (result (read stream :reporter    reporter
                                  :eof-error-p nil
                                  :eof-value   eof-value)))
         (list (cons :success (not (eq result eof-value)))
               (cons :result  result)
               (cons :styles  (genius:generate-styles (reverse (.get-tokens reporter))))))))

(defun genius:read-line ()
   (readline:read-line :prompt           "@ "
                       :analyze-function #'genius:analyze-string
                       :multi-line       t))
