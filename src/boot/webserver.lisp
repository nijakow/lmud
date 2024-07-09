
(defstruct http:request
   method
   uri
   version
   headers)

(defun http:parse-method (method)
   (cond ((string= method "GET")    :get)
         ((string= method "POST")   :post)
         ((string= method "PUT")    :put)
         ((string= method "DELETE") :delete)
         (t                         :unknown)))

(defun http:parse-headers (headers)
   (domap (header headers)
      (multiple-value-bind (part-1 part-2)
         (multiple-value-bind (a b)
               (string:partition header ": ")
            (if b
                (values a b)
                (string:partition header ":")))
         (cons part-1 part-2))))

(defun http::header-equal-p (header-1 header-2)
   (string= header-1 header-2 :key #'char-upcase))

(defun http:find-header (header request)
   (let ((slot (assoc header (http:request-headers request) :test #'http::header-equal-p)))
      (if slot
          (cdr slot)
          nil)))

(defun http:parse-request (request-line headers)
   (let ((split-request-line (string:split request-line " ")))
      (http:make-request :method  (http:parse-method (first split-request-line))
                         :uri     (second split-request-line)
                         :version (third split-request-line)
                         :headers (http:parse-headers headers))))


(defun webserver::read-until-crlf (port)
   (let ((line '()))
      (while t
         (let ((char (read-char port)))
            (cond ((char= char #\Newline) (return (conversions:->string (nreverse line))))
                  ((char= char #\Return)  nil)
                  (t                      (push char line)))))))

(defun webserver::read-request (port)
   (let ((request-line (webserver::read-until-crlf port))
         (headers     '()))
      (while t
         (let ((line (webserver::read-until-crlf port)))
            (if (string= line "")
               (return (http:parse-request request-line headers))
               (push line headers))))))

(defvar webserver::*test-html*
   '(:html
      (:head (:title "LMUD"))
      (:body
         (:h1 "LMUD")
         (:p "This is just a test page."))))

(defun webserver::write-html (html stream)
   (let ((tag-name (symbol-name (car html))))
      (io:uformat stream "<~a>" tag-name)
      (let ((args (cdr html)))
         (until (endp args)
            (let ((arg (pop args)))
               (cond ((listp   arg) (webserver::write-html arg stream))
                     ((stringp arg) (io:uformat stream "~a" arg))))))
      (io:uformat stream "</~a>" tag-name)))

(defun webserver::standard-page ()
   (with-string-output-stream stream
      (webserver::write-html webserver::*test-html* stream)))

(defun webserver::new-connection (port)
   (lmud.int:set-current-port port)
   (let* ((request (webserver::read-request port))
          (page    (webserver::standard-page)))
      (lmud.dummy::%prin1 (list (cons :method (http:request-method request))
                                (cons :uri    (http:request-uri request))
                                (cons :version (http:request-version request))
                                (cons :headers (http:request-headers request))))
      (lmud.dummy::%terpri)
      (lmud.dummy::%prin1 (list (http:find-header "Host" request)
                                (http:find-header "Connection" request)))
      (lmud.dummy::%terpri)
      (io:uformat port "HTTP/1.1 200 OK~%")
      (io:uformat port "Server: LMud/0.1~%")
      (io:uformat port "Content-Type: text/html~%")
      (io:uformat port "Connection: close~%")
      (io:uformat port "~%")
      (io:uformat port "~a~%" page)
      (close port)))

(defun webserver::start-with-new-connection (port)
   (webserver::new-connection (io:wrap-port port)))

(lmud.int:open-v4 "127.0.0.1" 8080 #'webserver::start-with-new-connection "http")
