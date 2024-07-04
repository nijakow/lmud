
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

(defun webserver::standard-page ()
   "
<!DOCTYPE html>
<html>
   <head>
      <title>LMUD</title>
   </head>
   <body>
      <h1>LMUD</h1>
      <p>
         This is a test page.
      </p>
   </body>
</html>
")

(defun webserver::new-connection (port)
   (lmud.int:set-current-port port)
   (let* ((request (webserver::read-request port))
          (page    (webserver::standard-page)))
      (lmud.dummy::%prin1 (list (cons :method (http:request-method request))
                                (cons :uri    (http:request-uri request))
                                (cons :version (http:request-version request))
                                (cons :headers (http:request-headers request))))
      (lmud.dummy::%terpri)
      (io:uformat port "HTTP/1.1 200 OK~%")
      (io:uformat port "Server: LMud/0.1~%")
      (io:uformat port "Content-Type: text/html~%")
      (io:uformat port "Connection: close~%")
      (io:uformat port "~%")
      (io:uformat port "~a~%" page)
      (close port)))

(defun webserver::start-with-new-connection (port)
   (webserver::new-connection port))

(lmud.int:open-v4 "127.0.0.1" 8080 #'webserver::start-with-new-connection "http")
