;;;;; backend.lisp
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

(defclass mixin-multiple-random-output-backend (backend) ((n :initarg :n
					     :initform 1
					     :accessor backend-n)))

(defmethod export-file (file (backend mixin-multiple-random-output-backend))
  (let* ((n (backend-n backend))
	 (*randomize* (if (> n 1) t nil)))
    (export-document
     (authoring-document ()
       (loop for *i-random* upto (1- n)
	  collect (read-file file)))
     backend)))

(defclass mixin-context-backend (backend) ())

(defclass mixin-context-xtable-backend (backend) ())

(defclass mixin-context-natural-table-backend (backend) ())

(defmethod initialize-instance :after ((obj mixin-context-backend) &rest rest)
  (setf (backend-view-fn obj) #'view-pdf
	(backend-compile-fn obj) (compose #'compila-context #'export-file)))

(defclass context-backend (mixin-context-backend mixin-context-xtable-backend) ())

(defmethod standard-output-file (file (backend context-backend))
  (merge-pathnames (merge-pathnames (pathname-name file) "context/prova.tex") file))

(defclass autarchy-backend (backend) ())
(defclass aut-context-backend (autarchy-backend mixin-context-backend mixin-context-xtable-backend) ())

(defmethod standard-output-file (file (backend aut-context-backend))
  (merge-pathnames (merge-pathnames (pathname-name file) "aut-context/prova.tex") file))


(defclass html-backend (autarchy-backend) ())

(defmethod standard-output-file (file (backend html-backend))
  (merge-pathnames (merge-pathnames (pathname-name file) "html/prova.html") file))

(defmethod initialize-instance :after ((obj html-backend) &rest rest)
  (setf (backend-view-fn obj) #'view-html))

;; (defconstant +context-backend+ (make-instance 'context-backend))
;; (defconstant +aut-context-backend+ (make-instance 'aut-context-backend))
;; (defconstant +html-backend+ (make-instance 'html-backend))

(setf (who:html-mode) :html5)

(defmacro html-output (&body body)
  `(who:with-html-output (*outstream* nil :indent t)
			 (who:htm ,@body)))

(defparameter *default-backend* (make-instance 'aut-context-backend))

