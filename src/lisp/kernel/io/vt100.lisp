
(defun vt100:clear-screen (&optional (stream (io:default-stream)))
   (princ "\e[2J\e[1;1H" stream))

(defun vt100:clear-full-line (&optional (stream (io:default-stream)))
   (princ "\e[2K" stream))

(defun vt100:move-up-by (n &optional (stream (io:default-stream)))
   (when (> n 0)
      (format stream "\e[~aA" n)))

(defun vt100:move-down-by (n &optional (stream (io:default-stream)))
   (when (> n 0)
      (format stream "\e[~aB" n)))

(defun vt100:jump-to-beginning-of-line (&optional (stream (io:default-stream)))
   (princ "\e[1G" stream))

(defun vt100:jump-to-line-pos (index &optional (stream (io:default-stream)))
   (format stream "\e[~aG" index))

(defun vt100:move-cursor (x y &optional (stream (io:default-stream)))
   (format stream "\e[~a;~aH" y x))

(defun vt100:get-cursor-position (&optional (stream (io:default-stream)))
   (flet ((subtract-one-if-number (n) (if n (- n 1) n)))
      (.disable-echo stream)
      (princ "\e[6n" stream)
      (io.reader:read-until-char stream #\[)
      (let ((first-number  (io.reader:read-until-char stream #\;))
            (second-number (io.reader:read-until-char stream #\R)))
         (.enable-echo stream)
         (values (subtract-one-if-number (io:parse-number second-number))
                 (subtract-one-if-number (io:parse-number first-number))))))

(defun vt100-styles:no-style   (&optional (stream (io:default-stream))) (princ "\e[0m" stream))
(defun vt100-styles:bold       (&optional (stream (io:default-stream))) (princ "\e[1m" stream))
(defun vt100-styles:italic     (&optional (stream (io:default-stream))) (princ "\e[3m" stream))
(defun vt100-styles:underline  (&optional (stream (io:default-stream))) (princ "\e[4m" stream))
(defun vt100-styles:blink      (&optional (stream (io:default-stream))) (princ "\e[5m" stream))
(defun vt100-styles:reverse    (&optional (stream (io:default-stream))) (princ "\e[7m" stream))
(defun vt100-styles:invisible  (&optional (stream (io:default-stream))) (princ "\e[8m" stream))
(defun vt100-styles:strike     (&optional (stream (io:default-stream))) (princ "\e[9m" stream))
(defun vt100-styles:fg-black   (&optional (stream (io:default-stream))) (princ "\e[30m" stream))
(defun vt100-styles:fg-red     (&optional (stream (io:default-stream))) (princ "\e[31m" stream))
(defun vt100-styles:fg-green   (&optional (stream (io:default-stream))) (princ "\e[32m" stream))
(defun vt100-styles:fg-yellow  (&optional (stream (io:default-stream))) (princ "\e[33m" stream))
(defun vt100-styles:fg-blue    (&optional (stream (io:default-stream))) (princ "\e[34m" stream))
(defun vt100-styles:fg-magenta (&optional (stream (io:default-stream))) (princ "\e[35m" stream))
(defun vt100-styles:fg-cyan    (&optional (stream (io:default-stream))) (princ "\e[36m" stream))
(defun vt100-styles:fg-white   (&optional (stream (io:default-stream))) (princ "\e[37m" stream))

(defun vt100-styles:fg-dark-gray   (&optional (stream (io:default-stream))) (princ "\e[90m" stream))
