
(defun square (x) (* x x))

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

(defun webserver::new-connection (port)
   (lmud.int:set-current-port port)
   (let ((head (webserver::read-header port)))
      (io:uformat port "HTTP/1.1 200 OK~%")
      (io:uformat port "Content-Type: text/html~%")
      (io:uformat port "Connection: close~%")
      (io:uformat port "~%")
      (io:uformat port "<!DOCTYPE html>~%")
      (io:uformat port "<html>~%")
      (io:uformat port "  <head>~%")
      (io:uformat port "    <title>LMUD</title>~%")
      (io:uformat port "  </head>~%")
      (io:uformat port "  <body>~%")
      (io:uformat port "    <h1>LMUD</h1>~%")
      (io:uformat port "    <p>~%")
      (io:uformat port "      This is a test page.~%")
      (io:uformat port "    </p>~%")
      (io:uformat port "  </body>~%")
      (io:uformat port "</html>~%")
      (close port)))


(lmud.int:open-v4 "127.0.0.1" 8080 #'webserver::new-connection)
