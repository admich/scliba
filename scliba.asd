;;;; scliba.asd

(asdf:defsystem #:scliba
  :description "A authoring system in Common Lisp"
  :author "Andrea De Michele  <andrea.demichele@gmail.com>"
  :license "GPL v3"
  :depends-on (#:uiop #:alexandria #:cl-ppcre #:local-time #:scribble #:physical-dimension #:cl-who)
  :serial t
  :components ((:file "package")
	       (:file "backend")
               (:file "scliba")
	       (:file "format")
	       (:file "formatter") ; to be removed when possible
	       (:file "pedb")
	       (:file "ptnh")
	       ))



