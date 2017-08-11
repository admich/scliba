(in-package #:scliba-pedb)

(defparameter *pedb-directory* #p"/home/admich/Documents/scuola/my-didattica/pedb/")
(defparameter *esercizi-directory* (uiop:merge-pathnames* #p"exercises/" *pedb-directory*)) ; #p"/home/admich/Documenti/scuola/my-didattica/context/esercizi/"
(defparameter *esercizi-preview-directory* (uiop:merge-pathnames* #p"preview/" *pedb-directory*))
(defparameter *compiti-directory* (uiop:merge-pathnames*  #p"compiti/" *pedb-directory*))
(defparameter *eserciziari-directory* (uiop:merge-pathnames* #p"eserciziari/" *pedb-directory*))
(defparameter *esercizi-argomenti* '(("Misure" . "mis") ("Rappresentazione" . "rap") ("Vettori" . "vet") ("Forze" . "for") ("Momenti" . "mom")  ("Fluidi" . "fl") ("Cinematica1d" . "cin1") ("Cinematica2d" . "cin2")  ("Dinamica" . "din") ("Energia" . "ener") ("Termologia" . "term") ("Elettrostatica" . "elect") ("Correnti elettriche" . "electcurr"))
  "argomenti")

(defparameter *esercizi-tipi* '(("true/false" . "tf") ("choices" . "ch") ("free" . "fr") ("fill" . "fl"))
  "tipi")

(def-authoring-tree compito (authoring-document) :documentation "Compito root document")


(defmethod export-document :around ((document compito) (backend context-backend))
  (format *outstream*"
\\usepath[../..]
\\project didattica
~:[% ~;~]\\enablemode[soluzioni]
"  (get-argument document :soluzioni))
  (call-next-method))

(defmethod export-document ((document compito) (backend html-backend))
  (who:with-html-output (*outstream*)
			(who:htm (:div :class 'compito
				       (:h1 (who:str (get-argument document :title)))
				       (call-next-method)))))

;; (defmethod export-document :before ((document compito) (backend context-backend))
;;   (let ((outstream (backend-outstream backend)))
;;     (format *outstream*"
;; \\usepath[../..]
;; \\project didattica
;; ~@[\\setupbodyfont[~dpt]~]
;; ~@[\\setupinterlinespace[~a]~]
;; ~:[% ~;~]\\enablemode[soluzioni]
;; \\starttext
;; \\compito[title=~A,scuola=none]
;; \\makecompitotitle
;; ~@[\\def\\rfoot{~A}~]" (get-argument document :bodyfont) (get-argument document :interline) (get-argument document :soluzioni) (get-argument  document :title) (get-argument document :rfoot))))

(defmethod export-document :before ((document compito) (backend context-backend))
  (format *outstream*"
\\compito[title=~A,scuola=none]
\\makecompitotitle
~@[\\def\\rfoot{~A}~]"  (get-argument  document :title) (get-argument document :rfoot)))

(defmethod export-document :around ((document compito) (backend aut-context-backend))
  (format *outstream* "~&\\setuppagenumbering[location=footer]
\\setuplayout[
  topspace=1cm,
  backspace=2cm,
  width=middle,
  height=middle,
  header=0pt]
")
  (call-next-method))

(defmethod export-document :before ((document compito) (backend aut-context-backend))
  (export-document (footer (:left "" :right (get-argument document :rfoot))) backend)
  (export-document (title (get-argument document :title)) backend))


(defmacro infoform ()
  "Form nome e cognome per compiti"
  `(table (:widths '(0.5 0.5) :frame nil)
     (table-row ()
       (table-cell () "Nome: " (hlinefill ()))
       (table-cell () "Classe: "(hlinefill ())))
     (table-row ()
       (table-cell () "Cognome: " (hlinefill ()))
       (table-cell () "Data: " (hlinefill ())))))

(def-enumerated esercizio) ;"\\inleft{~d}"

(defmethod export-document ((document esercizio) (backend aut-context-backend))
  (format *outstream* "~&\\inleft{~d}~%" (enumerated-n document))
  (call-next-method)
  (format *outstream*"~&~%"))

(defmethod export-document ((document esercizio) (backend html-backend))
  (html-output (:div :class "esercizio-head" (:h4 (who:fmt "Esercizio ~d" (enumerated-n document)))))
  (call-next-method)
  (format *outstream*"~&~%"))


(def-enumerated-slave-buffered soluzione esercizio) 
(defmethod export-document ((document soluzione) (backend aut-context-backend))
  (format *outstream* "~&Soluzione ~d~%" (enumerated-n document))
  (call-next-method)
  (format *outstream*"~&~%"))

(defmethod export-document ((document soluzione) (backend html-backend))
  (html-output (:div :class "esercizio-head" (:h4 (who:fmt "Soluzione ~d" (enumerated-n document)))))
  (call-next-method)
  (format *outstream*"~&~%"))



;;temp hack I want implement soluzione buffer in lisp
(defmethod export-document :around ((document soluzione) (backend context-backend))
  (format *outstream*"~&\\beginsoluzione~%")
  (call-next-method)
  (format *outstream*"~&\\endsoluzione~%"))

(def-authoring-tree soluzioni)
(defmethod export-document ((document soluzioni) (backend context-backend))
  (format *outstream* "
\\doifmode{soluzioni}{\\printsoluzioni}~%"))

(defmethod export-document ((document soluzioni) (backend autarchy-backend))
  (format *outstream* "{\\tfc Soluzioni. ~%~%}~a" (get-output-stream-string (cdr (assoc 'soluzione *buffers*)))))



(defparameter *last-sol* nil)
(defclass last-sol (authoring-tree)
  ())
(defmacro last-sol ()
  `(make-instance 'last-sol))
(defmethod export-document ((document last-sol) (backend mixin-context-backend))
  (format *outstream*"~[A~;B~;C~;D~;E~;F~]~%~% " *last-sol*))

(def-authoring-tree verofalso (itemize random-body))
(defmethod initialize-instance :after ((class verofalso) &rest rest)
  (setf (getf (authoring-tree-arguments class) :context) "a,packed,joinedup"))

(defmacro verofalso (arguments &body body)
  `(macrolet ((item (arguments &body body) `(make-instance 'item  :arguments (list ,@arguments) :body (flatten (list (vfbox) ,@body)))))
     (make-instance 'verofalso :arguments (list ,@arguments) :body (flatten (list  ,@body)))))

(def-simple-authoring-tree vfbox)
(defmethod export-document ((document vfbox) (backend mixin-context-backend))
  (format *outstream*"\\framed[width=1em,strut=yes]{V}\\thinspace\\framed[width=1em,strut=yes]{F}\\thinspace "))


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
		 (uiop:directory-files *esercizi-directory*)))

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
    ))

(defvar *i-compito* 0)
(defun compila-compito (compito &key n (directory *compiti-directory*) (backend-type 'context-backend))
  "genera il sorgente context dal sorgente lisp con la key :n genera random n compiti"
  (with-open-file (stream (merge-pathnames directory (make-pathname :name compito :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((backend (make-instance backend-type :stream stream)))
      (let ((*outstream* (backend-outstream backend)))
	(if n
	    (let ((*randomize* t))
	      (format stream "\\starttext~%")
	      (dotimes (*i-compito* n)
		(export-document (read-file (merge-pathnames directory (make-pathname :name compito :type "lisp"))) backend))
	      (format stream "\\stoptext~%")
	      )
	    (export-document (read-file (merge-pathnames directory (make-pathname :name compito :type "lisp"))) backend))))))

;; (defun compila-context-compito (file &key (directory *compiti-directory*))
;;   (let ((file (uiop:merge-pathnames* directory file)))
;;     (compila-context file)))

;; (defun compila-context-esercizio (file)
;;   (let ((file (uiop:merge-pathnames* *esercizi-directory* file)))
;;     (compila-context file)))

(defun compila-guarda-compito (file &key n (directory *compiti-directory*) (soluzioni nil) (backend-type 'context-backend))
  (compila-compito file :n n :directory directory  :backend-type backend-type)
  (let ((file (uiop:merge-pathnames* directory file))
	(file-pdf (uiop:merge-pathnames* directory (uiop:make-pathname* :name file :type "pdf"))))
    (if soluzioni
	(compila-context file :mode "soluzioni")
	(compila-context file))
    (view-pdf file-pdf)))

(defun compila-guarda-compito-soluzioni (file &key n (directory *compiti-directory*))
  (compila-guarda-compito file :n n :directory directory :soluzioni t))

(defun genera-esercizio-preview (esercizio)
  (with-open-file (stream (merge-pathnames *esercizi-preview-directory* (make-pathname :name esercizio :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let* ((backend (make-instance 'context-backend :stream stream))
	   (*outstream* (backend-outstream backend)))
      (format stream "\\usepath[../..]~%\\project didattica~%")
      (export-document (read-file (merge-pathnames *esercizi-directory*
						   (make-pathname :name esercizio :type "lisp"))) backend)
      (format stream "~%\\doifmode{soluzioni}{\\printsoluzioni}~%")))
  (let ((file (uiop:merge-pathnames* *esercizi-preview-directory*
				     esercizio)))
    (compila-context file :mode "soluzioni")))

(defun pedb-all-exercize ()
  (with-open-file (stream (merge-pathnames *eserciziari-directory* "all-exercise.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let* ((*section-level* 1)
	   (backend (make-instance 'context-backend :stream stream))
	   (*outstream* (backend-outstream backend)))
      (export-document (raccolta-esercizi) backend))))


#|
(compila-guarda-compito "esercizi-recupero-ii" :directory *eserciziari-directory* :backend-type 'aut-context-backend)

|#
