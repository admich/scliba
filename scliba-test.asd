(asdf:defsystem #:scliba-test
		:description "Test suite for sCLiba"
		:author "Andrea De Michele  <andrea.demichele@gmail.com>"
		:license "GPL v3"
		:depends-on (#:scliba #:fiveam)
		:serial t
		:components ((:module "t"
				      :serial t
				      :components ((:file "scliba-test")))))
