
(defun readline:dont-analyze (text)
   nil)

(defun readline:read-line (&key (prompt "") (stream (io:default-stream)) (analyze-function #'readline:dont-analyze))
   (let ((characters-left  '())
         (characters-right '()))
      (while t
         (let ((current-line-string (conversions:->string (append (reverse characters-left) characters-right))))
            (funcall analyze-function current-line-string)
            (vt100:clear-full-line stream)
            (vt100:jump-to-beginning-of-line stream)
            (princ prompt stream)
            (princ current-line-string stream)
            (vt100:jump-to-line-pos (+ 1 (length prompt) (length characters-left)) stream)
            (let ((char (read-char stream)))
               (case char
                  ((#\Newline)   (return current-line-string))
                  ((#\Left)      (when characters-left  (push (pop characters-left)  characters-right)))
                  ((#\Right)     (when characters-right (push (pop characters-right) characters-left)))
                  ((#\Backspace) (when characters-left  (pop characters-left)))
                  ((#\Home)      (setf characters-right (append (reverse characters-left) characters-right))
                                 (setf characters-left  '()))
                  ((#\End)       (setf characters-left  (append (reverse characters-right) characters-left))
                                 (setf characters-right '()))
                  (t (push char characters-left))))))))