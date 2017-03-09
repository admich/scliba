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

