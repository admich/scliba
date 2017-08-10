;;;;; backend.lisp
(in-package #:scliba)

(defclass backend ()
  ((outstream :initarg :stream
	      :initform t
	      :accessor backend-outstream)))
(defclass mixin-context-backend (backend) ())
(defclass context-backend (mixin-context-backend) ())
(defclass autarchy-backend (backend) ())
(defclass aut-context-backend (autarchy-backend mixin-context-backend) ())
(defclass html-backend (autarchy-backend) ())

(defmethod export-document :around ((document t) (backend mixin-context-backend))
  (if *top-level-document*
      (let ((*top-level-document* nil))
	(format *outstream*
		"\\starttext~%")
	(call-next-method)
	(format *outstream*
		"~&\\stoptext~%"))
      (call-next-method)))
