
(defun webserver::read-until-crlf (port)
   (let ((line '()))
      (while t
         (let ((char (read-char port)))
            (cond ((char= char #\Newline) (return (conversions:->string (nreverse line))))
                  ((char= char #\Return)  nil)
                  (t                      (push char line)))))))

(defun webserver::read-header (port)
   (while t
      (let ((line (webserver::read-until-crlf port)))
         (lmud.dummy::%princ line)
         (lmud.dummy::%terpri)
         (if (string= line "")
            (return)
            nil))))

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
   (let* ((head (webserver::read-header port))
          (page (webserver::standard-page)))
      (io:uformat port "HTTP/1.1 200 OK~%")
      (io:uformat port "Server: LMud/0.1~%")
      (io:uformat port "Content-Type: text/html~%")
      (io:uformat port "Connection: close~%")
      (io:uformat port "~%")
      (io:uformat port "~a~%" page)
      (close port)))

(lmud.int:open-v4 "127.0.0.1" 8080 #'webserver::new-connection "http")
