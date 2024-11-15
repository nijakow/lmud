
(defmacro random:pick (&body body)
   (let ((count (length body))
         (index 0))
      `(case (random ,count)
          ,@(domap (form body)
               (prog1 `((,index) ,form)
                  (incf index))))))

(defun game:punctuationp (char)
   (member char '(#\. #\, #\! #\? #\; #\:)))

(defun game:split-input-chars (chars)
   (setq chars (conversions:->list chars))
   (let ((current-word '())
         (words        '()))
      (until (endp chars)
         (let ((char (pop chars)))
            (cond ((lmud.char:whitespacep char)
                   (when current-word
                      (push (conversions:->string (nreverse current-word)) words)
                      (setq current-word '())))
                  (t (if (game:punctuationp char)
                         (progn (when current-word
                                   (push (conversions:->string (nreverse current-word)) words)
                                   (setq current-word '()))
                                (push (string char) words))
                         (push char current-word))))))
      (when current-word
         (push (conversions:->string (nreverse current-word)) words))
      (nreverse words)))
