;;; scliba.lisp
;;;; exp-backend
(in-package #:scliba)

(defvar *math* nil)
(defparameter *debug* nil)
(defparameter *outstream* *standard-output*)
(defparameter *outdirectory* "./")
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
  (with-open-file (ifile file)

    (let ( ;; (*default-pathname-defaults* (uiop:truename* file))
	  (old-package *package*))
      (loop for form = (read ifile nil :eof)
	 with value = '()
	 until (eq form :eof)
	 do (push (eval form) value)
	 finally
	   (setf *package* old-package)
	   (return (reverse value))))))


;;;;;;;;;;;;;;;;;;;;
;;; compile utility os interaction
;;;;;;;;;;;;;;;;;
(defun standard-output-file (file backend)
  (if (symbolp backend) (setf backend (make-instance backend)))
  (typecase backend
    (context-backend (merge-pathnames (merge-pathnames (pathname-name file) "context/prova.tex") file))
    (aut-context-backend (merge-pathnames (merge-pathnames (pathname-name file) "aut-context/prova.tex") file))
    (html-backend (merge-pathnames (merge-pathnames (pathname-name file) "html/prova.html") file) )))

(defgeneric export-file (file backend))

(defmethod export-file :around ((file t) (backend t))
  (let ((outfile (standard-output-file file backend))
	(*top-level-document* t))
    (uiop:ensure-all-directories-exist (list outfile))
    (with-open-file (stream outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (let ((*outstream* stream)
	    (*outdirectory* (pathname-directory outfile)))
	(call-next-method)
	))
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
			       (let ((command (format nil "context --purgeall ~@[--mode=~a~]  ~@[--result=~a~] ~a" mode output file)))
				 (print command)
				 (uiop:run-program command :output t)
				 (or output (merge-pathnames (make-pathname :type "pdf") file)))))	

(defun view-pdf (file)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
			       (let* ((file (if (string= "pdf" (pathname-type file))
						file
						(merge-pathnames (make-pathname :type "pdf") file)))
				      (command (format nil "~a ~a &" *command-pdf-viewer* file)))
				 (uiop:run-program command :output t))))

(defun view-html (file)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
			       (let ((command (format nil "~a ~a &" *command-html-viewer* file)))
				 (uiop:run-program command :output t))))

(defun compila-guarda (filelisp backend)
  (funcall (compose (backend-view-fn backend) (backend-compile-fn backend)) filelisp backend))

(defun compila (filelisp backend)
  (funcall (backend-compile-fn backend) filelisp backend))


#|
(compito (:title "Verifica di fisica" :scuola nil :rfoot "feb 2016 Q2C1 (w07)")
  (infoform)
  (columns (:n 2 :balance :no)
    (input (:file "asada")) 
    (esercizi  ("q-cin2-ch-00166" "q-cin2-ch-00087" "q-cin2-ch-00167" "q-din-ch-00168" "q-din-ch-00169" "q-din-ch-00132" "q-din-ch-00135" "q-din-ch-00122" "q-din-ch-00170")))
  )

(with-open-file (stream (merge-pathnames *compiti-directory* "compito.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
  (export-document (read-file "examples/compito.cla") :context stream))

(with-open-file (stream (merge-pathnames *compiti-directory* "all-exercise.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
  (let ((*section-level* 1)) (export-document (raccolta-esercizi) :context stream)))

|#
