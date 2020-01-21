(asdf:defsystem #:scliba-gui
		:description "A CLIM gui for sCLiba authoring system in Common Lisp"
		:author "Andrea De Michele  <andrea.demichele@gmail.com>"
		:license "GPL v3"
		:depends-on (#:mcclim #:scliba #:climacs)
		:serial t
		:components ((:module "gui"
				      :serial t
				      :components ((:file "package")
                                   (:file "scliba-gui")
                                   (:file "climacs-scliba-syntax")))))

