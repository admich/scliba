;;;;; backend.lisp
(in-package #:scliba)

(defclass backend ()
  ((outstream :initarg :stream
	      :initform *standard-output*
	      :accessor backend-outstream)))
(defclass mixin-context-backend (backend) ())
(defclass context-backend (mixin-context-backend) ())

(defclass autarchy-backend (backend) ())
(defclass aut-context-backend (autarchy-backend mixin-context-backend) ())
(defclass html-backend (autarchy-backend) ())

(defconstant +context-backend+ (make-instance 'context-backend))
(defconstant +aut-context-backend+ (make-instance 'aut-context-backend))
(defconstant +html-backend+ (make-instance 'html-backend))

(setf (who:html-mode) :html5)

(defmacro html-output (&body body)
  `(who:with-html-output (*outstream* nil :indent t)
			 (who:htm ,@body)))

(defmethod export-document :around ((document t) (backend mixin-context-backend))
  (if *top-level-document*
      (progn 
	(format *outstream*
		"\\starttext~%")
	(call-next-method)
	(format *outstream*
		"~&\\stoptext~%"))
      (call-next-method)))

(defmethod export-document :around ((document t) (backend html-backend))
  (if *top-level-document*
      (who:with-html-output (*outstream* nil :prologue t  :indent t)
	(:html
	 (:head
	  (:meta :charset "UTF-8")
	  (:title (who:str (get-argument document :title))))
	 (:body (call-next-method))))
      (call-next-method)))

