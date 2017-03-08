;;;;; backend.lisp
(in-package #:scliba)

(defclass backend ()
  ((outstream :initarg :stream
	      :initform t
	      :accessor backend-outstream)))
(defclass context-backend (backend) ())
(defclass autarchy-backend (backend) ())
(defclass aut-context-backend (autarchy-backend) ())

