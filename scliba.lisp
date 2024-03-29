;;; scliba.lisp
;;;; exp-backend
(in-package #:scliba)

(defvar *math* nil)
(defparameter *debug* nil)
(defparameter *outstream* *standard-output*)
(defparameter *output-file* "./file.lisp")
(defparameter *main-backend* nil)
(defparameter *top-level-document* nil)

(defparameter *command-pdf-viewer* "emacsclient -n") ;;emacsclient -n zathura
(defparameter *command-html-viewer* "firefox ")
(defparameter *current-node* nil)

(named-readtables:defreadtable :scribble-antik
  (:fuze :antik :scribble-both))


(setf *read-default-float-format* 'double-float)

(defun read-file (file)
  "Read the file and generate the clos structure of the document.
ATTENTION: don't read untrusted file. You read the file with common lisp reader."
  (named-readtables:in-readtable :scribble-antik) ;scribble-both
  (let ((*source-file* file))
    (with-open-file (ifile file)
      (let ( ;; (*default-pathname-defaults* (uiop:truename* file))
            (old-package *package*))
        (loop for form = (read ifile nil :eof)
           with value = '()
           until (eq form :eof)
           do (push (eval form) value)
           finally
             (setf *package* old-package)
             (return (reverse value)))))))


;;;;;;;;;;;;;;;;;;;;
;;; compile utility os interaction
;;;;;;;;;;;;;;;;;
(defgeneric standard-output-file (file backend)
  (:documentation "Return the output file for the backend")
  (:method (file (backend symbol)) (standard-output-file file (make-instance backend))))

(defgeneric export-file (file backend))

(defmethod export-file :around ((file t) (backend t))
  (let ((outfile (standard-output-file file backend))
	(*top-level-document* t))
    (uiop:ensure-all-directories-exist (list outfile))
    (with-open-file (stream outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (let ((*outstream* stream)
            (*output-file* outfile))
	(call-next-method)))
    outfile))

(defmethod export-file ((file t) (backend t))
  (export-document (read-file file) backend))

;; (defun export-file (file backend &key n)
;;   (let ((outfile (standard-output-file file backend))
;; 	(*top-level-document* t))
;;     (uiop:ensure-all-directories-exist (list outfile))
;;     (with-open-file (stream outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
;;       (let ((*outstream* stream)
;; 	    (*outdirectory* (pathname-directory outfile)))
;; 	(if n
;; 	    (let ((*randomize* t))
;; 	      (export-document
;; 	       (authoring-document ()
;; 		 (loop for *i-random* upto (1- n)
;; 		    collect (read-file file)))
;; 	       backend))
;; 	    (export-document (read-file file) backend))))
;;     outfile))


(defun compila-context (file &key (mode nil) (output nil))
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
    (let* ((filetex (pathname-name file))
	   (command (format nil "context --purgeall ~@[--mode=~a~]  ~@[--result=~a~] ~a" mode output filetex)))
				 (print command)
				 (uiop:run-program command :output t)
				 (or output (merge-pathnames (make-pathname :type "pdf") file)))))	

(defun view-pdf (file)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
    (let* ((file (merge-pathnames (make-pathname :type "pdf") (pathname-name file)))
	   (command (format nil "~a ~a &" *command-pdf-viewer* file)))
      (uiop:launch-program command))))

(defun view-html (file)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
			       (let ((command (format nil "~a ~a &" *command-html-viewer* file)))
				 (uiop:run-program command :output t))))

(defun compila-guarda (filelisp &optional (backend *default-backend*))
  (funcall (compose (backend-view-fn backend) (backend-compile-fn backend)) filelisp backend))

(defun compila (filelisp backend)
  (funcall (backend-compile-fn backend) filelisp backend))

