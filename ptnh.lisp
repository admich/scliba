(defpackage #:scliba-ptnh
  (:use #:cl #:scliba #:pedb)
  (:nicknames #:ptnh))

(in-package #:ptnh)

(defparameter *ptnh-directory* #p"/home/admich/Documenti/scuola/my-didattica/ptnh/")
(defun compila-ptnh (file)
  "genera il sorgente context dal sorgente lisp"
  (with-open-file (stream (merge-pathnames *ptnh-directory* (make-pathname :name file :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((backend (make-instance 'context-backend :stream stream)))
      (format stream "\\environment env_ptnh~%")
      (export-document (read-file (merge-pathnames *ptnh-directory* (make-pathname :name file :type "lisp"))) backend))))



(defun compila-context-ptnh (file)
  (let ((file (uiop:merge-pathnames* *ptnh-directory* file)))
    (compila-context file)))

(defun compila-guarda-ptnh (file)
  (compila-ptnh file)
  (let ((file (uiop:merge-pathnames* *ptnh-directory* file))
	(file-pdf (uiop:merge-pathnames* *ptnh-directory* (uiop:make-pathname* :name file :type "pdf"))))
    (compila-context file)
    (guarda file-pdf)))

(def-authoring-tree attenzione)

(defmethod export-document :before ((document attenzione) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\textrule[top]{Attenzione!}~%")))

(defmethod export-document :after ((document attenzione) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\textrule~%")))

