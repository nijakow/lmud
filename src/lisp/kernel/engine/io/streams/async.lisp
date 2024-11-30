
(tos:defclass io:<async-stream> (io:<basic-stream>)
   (with (process       nil)
         (process-state nil)
         (master-state  nil)))
