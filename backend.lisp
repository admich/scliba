;;;;; backend.lispback
(in-package #:scliba)

(defclass backend ()
  ((outstream :initarg :stream
	      :initform *standard-output*
	      :accessor backend-outstream)
   (view-fn :initarg :view-fn
	    :accessor backend-view-fn)
   (compile-fn :initarg :compile-fn
	       :accessor backend-compile-fn
	       :initform #'export-file)))

(defclass mixin-context-backend (backend) ())

(defmethod initialize-instance :after ((obj mixin-context-backend) &rest rest)
  (setf (backend-view-fn obj) #'view-pdf
	(backend-compile-fn obj) (compose #'compila-context #'export-file))
  )


(defclass context-backend (mixin-context-backend) ())

(defclass autarchy-backend (backend) ())
(defclass aut-context-backend (autarchy-backend mixin-context-backend) ())
(defclass html-backend (autarchy-backend) ())

(defmethod initialize-instance :after ((obj html-backend) &rest rest)
  (setf (backend-view-fn obj) #'view-html))

;; (defconstant +context-backend+ (make-instance 'context-backend))
;; (defconstant +aut-context-backend+ (make-instance 'aut-context-backend))
;; (defconstant +html-backend+ (make-instance 'html-backend))

(setf (who:html-mode) :html5)

(defmacro html-output (&body body)
  `(who:with-html-output (*outstream* nil :indent t)
			 (who:htm ,@body)))

