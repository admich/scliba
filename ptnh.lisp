(in-package #:ptnh)

(defparameter *ptnh-directory* #p"/home/admich/Documenti/scuola/my-didattica/ptnh/")


(defun compila-ptnh (document &key (directory *ptnh-directory*) (backend-type 'context-backend))
  "genera il sorgente context dal sorgente lisp"
  (with-open-file (stream (merge-pathnames directory (make-pathname :name document :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let* ((backend (make-instance backend-type :stream stream))
	   (*outstream* (backend-outstream backend)))
       (format stream "\\environment env_ptnh~%")
      (export-document (read-file (merge-pathnames directory (make-pathname :name document :type "lisp"))) backend))))



;; (defun compila-context-ptnh (file)
;;   (let ((file (uiop:merge-pathnames* *ptnh-directory* file)))
;;     (compila-context file)))

(defun compila-guarda-ptnh (file &key (directory *ptnh-directory*) (backend-type 'context-backend))
  (compila-ptnh file)
  (let ((file (uiop:merge-pathnames* directory file))
	(file-pdf (uiop:merge-pathnames* directory (uiop:make-pathname* :name file :type "pdf"))))
    (compila-context file)
    (guarda file-pdf)))

(def-authoring-tree ptnh-document (authoring-document) "ptnh root document")

(def-authoring-tree attenzione)

(defmethod export-document :before ((document attenzione) (backend mixin-context-backend))
  (format *outstream* "~&\\textrule[top]{Attenzione!}~%"))

(defmethod export-document :after ((document attenzione) (backend mixin-context-backend))
  (format *outstream* "~&\\textrule~%"))

