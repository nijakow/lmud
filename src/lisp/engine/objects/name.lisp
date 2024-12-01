
(tos:defclass <name> ()
   (with (article nil)
         (short   "Unnamed Object")))

(tos:defmethod (<name> construct) (&key article (short "Unnamed Object"))
   (prog1 self
      (setf .article article)
      (setf .short   short)))
