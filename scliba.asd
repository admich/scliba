;;;; scliba.asd

(asdf:defsystem #:scliba
  :description "A authoring system in Common Lisp"
  :author "Andrea De Michele  <andrea.demichele@gmail.com>"
  :license "GPL v3"
  :depends-on (#:uiop #:alexandria #:cl-ppcre #:local-time #:scribble #:physical-dimension #:cl-who #:cxml #:zip)
  :serial t
  :components ((:file "package")
	           (:file "macro")
	           (:file "scliba")
	           (:file "base")
	           (:file "backend")
	           (:file "backend-exporter")
	           (:file "physical-quantity")
	           (:file "physics")
	           (:file "pedb")
	           (:file "ptnh")
               (:file "itas")
	           (:file "itas-odt")))



