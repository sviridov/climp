
(asdf:defsystem #:climp
  :name "CLiMP"
  :serial t
  :author "Alexander Sviridov (sviridov.vmi@gmail.com)"
  :maintainer "Alexander Sviridov (sviridov.vmi@gmail.com)"
  :licence "Lisp LGPL"
  :version "0.1"
  :pathname "src/"
  :depends-on (#:alexandria
               #:bordeaux-threads 
               #:cffi 
               #:anaphora)
  :components ((:file "package")
               (:file "utilits")
               (:file "thread-pool-interface")
               (:file "commands")
               (:file "reductions")
               (:file "parallel")
               (:file "parallel-for")
               (:file "parallel-dolist")))
