(defpackage #:scliba-pedb
  (:use #:cl #:alexandria #:scliba #:antik)
  (:shadowing-import-from #:antik #:MAXIMIZING #:MAXIMIZE #:MINIMIZING #:MINIMIZE
 #:MULTIPLYING #:MULTIPLY #:SUMMING #:SUM #:FOR #:TIME
 #:LENGTH #:DECF #:INCF #:SIGNUM #:ROUND #:FLOOR
 #:COERCE #:< #:<= #:> #:>= #:= #:MAX #:MIN
 #:ZEROP #:MINUSP #:PLUSP #:ABS #:EXP #:LOG #:EXPT
 #:SQRT #:TANH #:COSH #:SINH #:ATAN #:ACOS #:ASIN
 #:TAN #:COS #:SIN #:/ #:* #:- #:+ GRID:AREF
 #:POLAR-TO-RECTANGULAR #:RECTANGULAR-TO-POLAR #:ACCELERATION
 #:PSI #:KNOTS #:ROTATE)
  (:nicknames #:pedb))

(in-package #:scliba-pedb)

(defparameter *esercizi-directory* #p"/home/admich/Documenti/scuola/my-didattica/pedb/exercises/") ; #p"/home/admich/Documenti/scuola/my-didattica/context/esercizi/"
(defparameter *esercizi-preview-directory* #p"/home/admich/Documenti/scuola/my-didattica/pedb/preview/")
(defparameter *compiti-directory* #p"/home/admich/Documenti/scuola/my-didattica/pedb/compiti/")
(defparameter *eserciziari-directory* #p"/home/admich/Documenti/scuola/my-didattica/pedb/eserciziari/")
(defparameter *esercizi-argomenti* '(("Misure" . "mis") ("Rappresentazione" . "rap") ("Vettori" . "vet") ("Forze" . "for") ("Momenti" . "mom")  ("Fluidi" . "fl") ("Cinematica1d" . "cin1") ("Cinematica2d" . "cin2")  ("Dinamica" . "din") ("Energia" . "ener") ("Termologia" . "term") ("Elettrostatica" . "elect") ("Correnti elettriche" . "electcurr"))
  "argomenti")

(defparameter *esercizi-tipi* '(("true/false" . "tf") ("choices" . "ch") ("free" . "fr") ("fill" . "fl"))
  "tipi")

(def-authoring-tree compito)
;; (defclass compito (authoring-document)
;;   ())

;; (defmacro compito (arguments &body body)
;;   `(make-instance 'compito :arguments (list ,@arguments) :body (flatten (list ,@body))))

(defmethod export-document :before ((document compito) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "
\\usepath[../..]
\\project didattica
~@[\\setupbodyfont[~dpt]~]
~@[\\setupinterlinespace[~a]~]
~:[% ~;~]\\enablemode[soluzioni]
\\starttext
\\compito[title=~A,scuola=none]
\\makecompitotitle
~@[\\def\\rfoot{~A}~]" (get-argument document :bodyfont) (get-argument document :interline) (get-argument document :soluzioni) (get-argument  document :title) (get-argument document :rfoot))))


;; (dolist (tree (slot-value document 'body))
;;   (export-document tree backend outstream))

(defmethod export-document :after ((document compito) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "
% \\doifmode{soluzioni}{\\printsoluzioni}
\\stoptext")))

(defclass infoform (authoring-tree)
  ())
(defmacro infoform ()
  `(make-instance 'infoform))
(defmethod export-document ((document infoform) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "
\\infoform~%")))

(def-startstop esercizio)
(def-startstop soluzione)
;;temp hack I want implement soluzione buffer in lisp
(defmethod export-document :before ((document soluzione) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\beginsoluzione~%")))
(defmethod export-document :after ((document soluzione) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\endsoluzione~%")))

(def-authoring-tree soluzioni)
(defmethod export-document ((document soluzioni) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "
\\doifmode{soluzioni}{\\printsoluzioni}~%")))


(defparameter *last-sol* nil)
(defclass last-sol (authoring-tree)
  ())
(defmacro last-sol ()
  `(make-instance 'last-sol))
(defmethod export-document ((document last-sol) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~[A~;B~;C~;D~;E~;F~] " *last-sol*)))

(def-authoring-tree verofalso (itemize random-body))
(defmethod initialize-instance :after ((class verofalso) &rest rest)
  (setf (getf (authoring-tree-arguments class) :context) "a,packed,joinedup"))



;; (make-instance  ,cl :body ) (vfbox)
(defmacro verofalso (arguments &body body)
  `(macrolet ((item (arguments &body body) `(make-instance 'item  :arguments (list ,@arguments) :body (flatten (list (vfbox) ,@body)))))
     (make-instance 'verofalso :arguments (list ,@arguments) :body (flatten (list  ,@body)))))

(def-simple-authoring-tree vfbox)
(defmethod export-document ((document vfbox) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\framed[width=1em,strut=yes]{V}\\thinspace\\framed[width=1em,strut=yes]{F}\\thinspace ")))


(def-authoring-tree scelte (itemize random-body))
;; columns in itemize don't work inside two column layout
(defmethod initialize-instance :after ((class scelte) &rest rest)
  (let ((cols (get-argument class :columns)))
	     (if cols
		 (setf (getf (authoring-tree-arguments class) :context) (format nil "A,packed,joinedup,columns,~r" cols))
		 (setf (getf (authoring-tree-arguments class) :context) "A,packed,joinedup"))))

(defmethod export-document :after ((document scelte) backend)
  (setf *last-sol* (position :sol (authoring-tree-body document) :key #'authoring-tree-arguments :test #'member)))

(def-authoring-tree parti (itemize))
(defmethod initialize-instance :after ((class parti) &rest rest)
  (setf (getf (authoring-tree-arguments class) :context) "i,packed"))

(defmacro input-esercizio (file)
  (with-unique-names (new-path)
    `(let ((,new-path
	    (or (probe-file ,file)
		(probe-file (merge-pathnames *esercizi-directory* (make-pathname :name ,file :type "lisp")))
		(merge-pathnames *esercizi-directory* (make-pathname :name ,file :type "tex")))))
       (input ,new-path)
       )))

(defmacro esercizi (&rest exes)
  `(loop for x in (list ,@exes)
      collect (input-esercizio x)))

;;;;;

(defun tutti-esercizi ()
  (remove-if-not (lambda (x) (let ((str (pathname-type x))) (or (string= "tex" str) (string= "lisp" str))))
		 (fad:list-directory *esercizi-directory*)))

(defun get-esercizio-argomento (ese)
  (second (ppcre:split "-" (pathname-name ese))))

(defun get-esercizio-numero (ese)
  (parse-integer (fourth (ppcre:split "-" (pathname-name ese)))))

(defun esercizio-numero (n)
  (find-if (lambda (x) (= n (get-esercizio-numero x))) (tutti-esercizi)))

(defun esercizi-di (argomento)
  (remove-if-not (lambda (x) (string= argomento (get-esercizio-argomento x))) (tutti-esercizi)))

(defun raccolta-esercizi ()
  
  (let ((*randomize* nil))
    
    (compito (:title "Eserciziario di fisica" :soluzioni t :rfoot (format nil "Compilato \\date\\ \\currenttime"))
      "
\\enablemode[soluzioni]
% \\setupbodyfont[11pt]
\\setupenumerations[esercizio][margin=yes, way=bysection, prefix=yes,prefixsegments=section,width=fit]
\\setupenumerations[soluzione][margin=yes, way=bysection, prefix=yes,prefixsegments=section,width=fit]"
      (loop for  topic in *esercizi-argomenti*
	 collect
           
	   (section (:title (car topic))
	     (loop for ese in (esercizi-di (cdr topic))
		append
		  (list (input-esercizio ese)
			(format nil "~&\\rightaligned{\\color[middlegray]{~A}}~%" (pathname-name ese))))
	     "
\\doifmode{soluzioni}{\\subject{Soluzioni}
\\selectblocks[soluzione][criterium=section]}")))
    )
  )

(defvar *i-compito* 0)
(defun compila-compito (compito &key n (directory *compiti-directory*))
  "genera il sorgente context dal sorgente lisp con la key :n genera random n compiti"
  (with-open-file (stream (merge-pathnames directory (make-pathname :name compito :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((backend (make-instance 'context-backend :stream stream)))
      (if n
	  (let ((*randomize* t))
	    (format stream "\\starttext~%")
	    (dotimes (*i-compito* n)
	      (export-document (read-file (merge-pathnames directory (make-pathname :name compito :type "lisp"))) backend))
	    (format stream "\\stoptext~%"))
	  (export-document (read-file (merge-pathnames directory (make-pathname :name compito :type "lisp"))) backend)))))

;; (defun compila-context-compito (file &key (directory *compiti-directory*))
;;   (let ((file (uiop:merge-pathnames* directory file)))
;;     (compila-context file)))

;; (defun compila-context-esercizio (file)
;;   (let ((file (uiop:merge-pathnames* *esercizi-directory* file)))
;;     (compila-context file)))

(defun compila-guarda-compito (file &key n (directory *compiti-directory*) (soluzioni nil))
  (compila-compito file :n n :directory directory)
  (let ((file (uiop:merge-pathnames* directory file))
	(file-pdf (uiop:merge-pathnames* directory (uiop:make-pathname* :name file :type "pdf"))))
    (if soluzioni
	(compila-context file :mode "soluzioni")
	(compila-context file))
    (guarda file-pdf)))

(defun compila-guarda-compito-soluzioni (file &key n (directory *compiti-directory*))
  (compila-guarda-compito file :n n :directory directory :soluzioni t))

(defun genera-esercizio-preview (esercizio)
  (with-open-file (stream (merge-pathnames *esercizi-preview-directory* (make-pathname :name esercizio :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((backend (make-instance 'context-backend :stream stream)))
      (format stream "\\usepath[../..]~%\\project didattica~%")
      (export-document (read-file (merge-pathnames *esercizi-directory*
						   (make-pathname :name esercizio :type "lisp"))) backend)
      (format stream "~%\\doifmode{soluzioni}{\\printsoluzioni}~%")))
  (let ((file (uiop:merge-pathnames* *esercizi-preview-directory*
				     esercizio)))
    (compila-context file :mode "soluzioni"))
  ;; (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
  ;;   (let ((command (format nil "context --purgeall --mode=soluzioni ~a"  file)))
  ;;     (uiop:run-program  command   :output t))
    ; (uiop:run-program (list "context" (pathname-name file)) :output t)
  )

(defun pedb-all-exercize ()
  (with-open-file (stream (merge-pathnames *eserciziari-directory* "all-exercise.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((*section-level* 1)
	  (backend (make-instance 'context-backend :stream stream)))
      (export-document (raccolta-esercizi) backend))))



