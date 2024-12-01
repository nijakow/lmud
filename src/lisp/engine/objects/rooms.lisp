
(define-room ~the-void ()
   (name :short "The Void")
   (n-to ~more-void))

(define-method (~the-void describe) ()
   (tell (:p "You seem to be floating in an endless void.")))


(define-room ~more-void ()
   (name :short "More Void")
   (s-to ~the-void))

(define-method (~more-void describe) ()
   (tell (:p "You seem to be floating in more endless void.")))
