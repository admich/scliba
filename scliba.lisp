;;;; scliba.lisp
;;;; exp-backend
(in-package #:scliba)

(defvar *math* nil)
(defparameter *debug* nil)

;;; HIGH security issue
;; (defun read-file (file)
;;   "Read the file and generate the clos structure of the document.
;; ATTENTION: don't read untrusted file. You read the file with common lisp reader."
;;   (let (ret)
;;     (scribble:enable-scribble-syntax)
;;     (scribble:enable-scribble-at-syntax)    
;;     (setf ret (with-open-file (ifile file)
;; 		(eval (read ifile))))
;;     (scribble:disable-scribble-syntax)
;;     (scribble:disable-scribble-at-syntax)
;;     ret))

					;(scribble:enable-scribble-syntax)
					;(scribble:enable-scribble-at-syntax)    

;; :merge error n conflict :fuze continue 


(named-readtables:defreadtable :scribble-antik
  (:fuze :antik :scribble-both))

(setf *read-default-float-format* 'double-float)
; (named-readtables:in-readtable :scribble-antik) ;scribble-both
(defun read-file (file)
  "Read the file and generate the clos structure of the document.
ATTENTION: don't read untrusted file. You read the file with common lisp reader."
  (named-readtables:in-readtable :scribble-antik) ;scribble-both
  (with-open-file (ifile file)
    (eval (read ifile))
    ;; (let ((*readtable* (named-readtables:find-readtable :antik))
    ;; 	  (*read-default-float-format* 'double-float))
    ;;   (eval (read ifile)))
    ))

(defclass authoring-tree ()
  ((arguments :initarg :arguments
	      :initform nil
	      :accessor authoring-tree-arguments)
   (body :initarg :body
;	 :initform nil
	 :reader authoring-tree-body))
  (:documentation "Main parent class for scliba documents"))


(defun get-argument (auth-tree arg)
  "get"
  (getf (authoring-tree-arguments auth-tree) arg))

;;; export-document generic
(defgeneric export-document (document backend)
  (:documentation "generate the output"))

(defun export-document-on-string (document backend)
  "return a string"
  (let* ((s (make-string-output-stream))
	 (tmp (backend-outstream backend)))
    (setf (backend-outstream backend) s)
    (export-document document backend)
    (setf (backend-outstream backend) tmp)
    (get-output-stream-string s)))


(defmethod export-document ((document t) backend)
  (let ((outstream (backend-outstream backend)))
    (format outstream "")))

(defclass authoring-document (authoring-tree)
  ())


(defclass startstop (authoring-tree)
  ())

(defmethod export-document ((document startstop) (backend context-backend))
  (let ((outstream (backend-outstream backend))
	(clstr (string-downcase (symbol-name (class-name (class-of document))))))
    (format outstream "~&\\start~A~@[[~A]~]~%" clstr (getf (slot-value document 'arguments) :context))
    (dolist (tree (slot-value document 'body))
      (export-document tree backend outstream))
    (format outstream "~&\\stop~A~%" clstr)))


;;; macro utility
;;math not work try to backquote the let in defmacro
;; (defmacro def-authoring-tree (name &optional (superclass '(authoring-tree)))
;;   `(progn
;;      (defclass ,name (,@superclass)
;;        ())
;;      (defmacro ,name (arguments &body body)
;;        (let ((cl '',name)
;; 	     (*math* (if (typep (make-instance ',name) 'math) t nil)))
;; ;	 (when (typep (make-instance ',name) 'math) (setf *math* t))
;; 	 `(make-instance  ,cl :arguments (list ,@arguments) :body (flatten (list  ,@body)))
;; ;	 (when (typep (make-instance ',name) 'math) (setf *math* nil))
;; 	 ))))
(defmacro def-authoring-tree (name &optional (superclass '(authoring-tree)))
  `(progn
     (defclass ,name (,@superclass)
       ())
     (defmacro ,name (arguments &body body)
       (let ((cl '',name))
	 `(let ((*math* (if (typep (make-instance ,cl) 'math) t nil)))
	    (make-instance  ,cl :arguments (list ,@arguments) :body (flatten (list  ,@body))))))))

(defmacro def-simple-authoring-tree (name &optional (superclass '(authoring-tree)))
  `(progn
     (defclass ,name (,@superclass)
       ())
     (defmacro ,name (&body body)
       (let ((cl '',name))
	 `(let ((*math* (if (typep (make-instance ,cl) 'math) t nil)))
	    (make-instance  ,cl  :body (flatten (list  ,@body))))))))


(defmacro def-startstop% (name &key superclass context-name)
  (let ((namestr (or context-name (string-downcase (symbol-name name)))))
    `(progn
       (def-authoring-tree ,name (startstop ,@superclass))
       
       (defmethod export-document ((document ,name) (backend context-backend))
       	 (let ((outstream (backend-outstream backend)))
	   (format outstream "~&\\start~A~@[[~A]~]~%" ,namestr (getf (slot-value document 'arguments) :context))
	   (dolist (tree (slot-value document 'body))
	     (export-document tree backend))
	   (format outstream "~&\\stop~A~%" ,namestr))))))

(defmacro def-startstop (name &optional superclass)
  (let ((namestr (string-downcase (symbol-name name))))
    `(def-startstop% ,name :superclass ,superclass)))

;; (defmacro def-startstop (name &optional superclass)
;;   (let ((namestr (string-downcase (symbol-name name))))
;;     `(progn
;;        (def-authoring-tree ,name (startstop ,@superclass))

;;        (defmethod export-document ((document ,name) (backend context-backend) outstream)
;;        	 (format outstream "~&\\start~A~@[[~A]~]~%" ,namestr (getf (slot-value document 'arguments) :context))
;;        	 (dolist (tree (slot-value document 'body))
;;        	   (export-document tree backend outstream))
;;        	 (format outstream "~&\\stop~A~%" ,namestr))
;;        )))





(defmethod export-document ((document authoring-tree) backend)
  (dolist (tree (slot-value document 'body))
    (export-document tree backend)))


(defmethod export-document ((document string) backend)
    (let ((outstream (backend-outstream backend)))
      (format outstream  document)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; document part utility
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; input
(defclass input-tex (authoring-tree)
  ((file :initarg :file)))

(defmethod export-document ((document input-tex) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\component ~A~%" (pathname-name (slot-value document 'file)))))

(defun input (filename)
  (if (string= "tex" (pathname-type filename))
      (make-instance 'input-tex :file filename)
      (if *debug*
	  (list (read-file filename) (format nil "~&\\rightaligned{\\color[middlegray]{~A}}~%" (pathname-name filename)))
	  (read-file filename)))) ;; read-file

;; random
(defparameter *randomize* nil)

(defun choose-one-of (seq)
  "If *randomize* is true choose at random from sequence otherwise return the first element in the list"
  (if *randomize* (random-elt seq) (first-elt seq)))

(defclass random-body (authoring-document)
  ())

(defmethod export-document :before ((document random-body) backend)
	   (if (and *randomize* (not (get-argument document :no-random)))
	       (if (get-argument document :no-random-last)
		   (setf (slot-value document 'body) (append (shuffle (butlast (slot-value document 'body))) (last (slot-value document 'body))))
		   (setf (slot-value document 'body) (shuffle (slot-value document 'body))))))

(def-authoring-tree randomize (random-body))
;; (defmethod export-document ((document randomize) backend outstream)
;;   (let ((bodylist (if *randomize* (shuffle (copy-list (slot-value document 'body))) (copy-list (slot-value document 'body)))))
;;       (dolist (tree bodylist)
;; 	(export-document tree backend outstream))))

;;;;;;;;;;;;;;;;;;;;;;
;;; document part core

(def-authoring-tree par)
(defmethod export-document :before ((document par) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\par ~@[\\inouter{~a}~]" (export-document-on-string (getf (authoring-tree-arguments document) :tag) backend))))

(def-startstop framedtext)
(defmethod initialize-instance :after ((class framedtext) &rest rest)
	   (when (getf (authoring-tree-arguments class) :middle)
	     (setf (getf (authoring-tree-arguments class) :context) "middle")))


(def-authoring-tree hline)
(defmethod export-document ((document hline) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\hairline~%")))

;; footnote
(def-authoring-tree footnote)

(defmethod export-document :before ((document footnote) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\footnote{")))

(defmethod export-document :after ((document footnote) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "}")))


(def-authoring-tree section)
(defvar *section-level* 0)
(defparameter *section-context-labels* (list "part" "chapter" "section" "subsection" "subsubsection"))

(defmethod export-document :before ((document section) (backend context-backend))
  (incf *section-level*)
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\start~A~@[[~A]~]~%" (elt *section-context-labels* *section-level*) (getf (slot-value document 'arguments) :context))))

(defmethod export-document :after ((document section) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\stop~A~%" (elt *section-context-labels* *section-level*) (getf (slot-value document 'arguments) :context)))
  (decf *section-level*))


(defmethod initialize-instance :after ((class section) &rest rest)
  (setf (getf (authoring-tree-arguments class) :context) (format nil "title=~A" (getf (authoring-tree-arguments class) :title))))

(def-startstop itemize)
(def-authoring-tree item)
(defmethod export-document :before ((document item) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\item ")))

(def-simple-authoring-tree bf)

(defmethod export-document :before ((document bf) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "{\\bf ")))
(defmethod export-document :after ((document bf) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "}")))
(def-simple-authoring-tree it)
(defmethod export-document :before ((document it) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "{\\it ")))
(defmethod export-document :after ((document it) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "}")))


(def-simple-authoring-tree newpage)
(defmethod export-document ((document newpage) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\page ")))

(defmacro emph (&rest body)
  `(it ,@body))

(def-startstop columns)

(def-authoring-tree newcolumn)
(defmethod export-document ((document newcolumn) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~&\\column~%")))


(def-authoring-tree math)
;;; maybe a counter for *math* in case of nested math environment
(defmethod export-document :before ((document math) backend)
  (setf *math* t))
(defmethod export-document :after ((document math) backend)
  (setf *math* nil))

(def-authoring-tree ref)
(defmethod export-document ((document ref) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\in[~a]" (get-argument document :ref))))

(def-authoring-tree figure)
(defmethod export-document :before ((document figure) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\placefigure[here][~a]{~A}{" (get-argument document :ref) (get-argument document :caption))))
(defmethod export-document :after ((document figure) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "}")))
(def-startstop% mpcode :context-name "MPcode")


;;;;TABLE
(def-authoring-tree table)
(defmethod export-document :before ((document table) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\bTABLE")))
(defmethod export-document :after ((document table) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\eTABLE")))
(def-authoring-tree table-row)
(defmethod export-document :before ((document table-row) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\bTR")))
(defmethod export-document :after ((document table-row) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\eTR")))
(def-authoring-tree table-cell)
(defmethod export-document :before ((document table-cell) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\bTD")))
(defmethod export-document :after ((document table-cell) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "\\eTD")))


;;;;;;;;;;;;;;;;;;
;;; CAOS make-instance e una funzione *math* gli argomenti sono valutati prima di cambiare *math* nei metodi
;;;;;;;;;;;;;;;;;
;; (defmethod initialize-instance :before ((class math) &rest initargs)
;;   (print "set m")
;;   (setf *math* t))

;; (defmethod initialize-instance :after ((class math) &rest initargs)
;;   (print "uset m")
;;   (setf *math* nil))


;; (defmethod export-document :before ((document formula) backend outstream)
;;   (format t "before formula ~A" *math*))
;; (defmethod export-document :after ((document formula) backend outstream)
;;   (format t "after formula ~A" *math*))

;; (defmethod export-document :around ((document formula) backend outstream)
;;   (format t "around formula ~A~%" *math*)
;;   (call-next-method))

;; (defmethod export-document :around ((document math) backend outstream)
;;   (let ((*math* t))
;;     (call-next-method)))

;; (defmethod initialize-instance :around ((document math) &rest initargs)
;;   (let ((*math* t))
;;     (call-next-method)))
(def-simple-authoring-tree imath (math))

(defmethod export-document :before ((document imath) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "$")))
(defmethod export-document :after ((document imath) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "$")))

(def-authoring-tree phys-n)
(defmethod export-document  ((document phys-n) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (scliba-f:n outstream (first (authoring-tree-body document)) nil nil)))

(def-startstop formula (math))
(defmethod export-document :before ((document formula) (backend context-backend))
  (let ((outstream (backend-outstream backend)))
    (format outstream "~@[~&\\placeformula[~a]~%~]" (getf (authoring-tree-arguments document) :ref))))


;;;;;;;;;;;;;;;;;;;;
;;; compile utility os interaction
;;;;;;;;;;;;;;;;;
(defun compila-context (file &key (mode nil) (output nil))
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
    (let ((command (format nil "context --purgeall ~@[--mode=~a~] ~@[--result=~a~] ~a" mode output file )))
      (uiop:run-program command :output t))))

(defun compila-context-soluzioni (file)
  (compila-context file :mode "soluzioni")
  ;; (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
  ;;   (let ((command (format nil "context --purgeall --mode=soluzioni ~a"  file)))
  ;;     (uiop:run-program  command   :output t))
  ;;   ; (uiop:run-program (list "context" (pathname-name file)) :output t)
  ;;   )
  )

;(defparameter *command-pdf-viewer* "zathura")
(defparameter *command-pdf-viewer* "emacsclient -n")

(defun guarda (file)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
    (let ((command (format nil "~a ~a &" *command-pdf-viewer*  file)))
      (uiop:run-program  command   :output t))
    ))

;;;;;;;;;;;;;;;;;;;;
;;; exercise module
;;;;;;;;;;;;;;;;;
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
~@[\\def\\rfoot{~A}~]" (get-argument document :bodyfont) (get-argument document :interline) (get-argument document :soluzioni) (getf (slot-value document 'arguments) :title) (getf (slot-value document 'arguments) :rfoot))))


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
	   (let ((cols (getf (authoring-tree-arguments class) :columns)))
	     (if cols
		 (setf (getf (authoring-tree-arguments class) :context) (format nil "A,packed,joinedup,columns,~r" cols))
		 (setf (getf (authoring-tree-arguments class) :context) "A,packed,joinedup"))))

(defmethod export-document :after ((document scelte) backend)
  (setf *last-sol* (position :sol (slot-value document 'body) :key #'authoring-tree-arguments :test #'member)))

(def-authoring-tree parti (itemize))
(defmethod initialize-instance :after ((class parti) &rest rest)
  (setf (getf (authoring-tree-arguments class) :context) "i,packed"))

(defmacro input-esercizio (file)
  (with-unique-names (new-path)
    `(let ((,new-path
	    (or (probe-file ,file)
		(probe-file (merge-pathnames *esercizi-directory* (make-pathname :name ,file :type "lisp")))
		(merge-pathnames *esercizi-directory* (make-pathname :name ,file :type "tex")))))
       (input ,new-path))))

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
\\selectblocks[soluzione][criterium=section]}")))))

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

;;;;;;;;;PTNH
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

(defun pedb-all-exercize ()
  (with-open-file (stream (merge-pathnames *eserciziari-directory* "all-exercise.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((*section-level* 1)
	  (backend (make-instance 'context-backend :stream stream)))
      (export-document (raccolta-esercizi) backend))))

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
