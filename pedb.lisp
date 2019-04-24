(in-package #:scliba-pedb)

(defparameter *pedb-directory* #p"/home/admich/Documents/scuola/my-didattica/pedb/")
(defparameter *esercizi-directory* (uiop:merge-pathnames* #p"exercises/" *pedb-directory*)) ; #p"/home/admich/Documenti/scuola/my-didattica/context/esercizi/"
(defparameter *esercizi-preview-directory* (uiop:merge-pathnames* #p"preview/" *pedb-directory*))
(defparameter *compiti-directory* (uiop:merge-pathnames*  #p"compiti/file.lisp" *pedb-directory*))
(defparameter *eserciziari-directory* (uiop:merge-pathnames* #p"eserciziari/file.lisp" *pedb-directory*))
(defparameter *esercizi-argomenti* '(("Misure" . "mis") ("Rappresentazione" . "rap") ("Vettori" . "vet") ("Forze" . "for") ("Momenti" . "mom")  ("Fluidi" . "fl") ("Cinematica1d" . "cin1") ("Cinematica2d" . "cin2")  ("Dinamica" . "din") ("Energia" . "ener") ("Termologia" . "term") ("Elettrostatica" . "elect") ("Correnti elettriche" . "electcurr"))
  "argomenti")

(defparameter *esercizi-tipi* '(("true/false" . "tf") ("choices" . "ch") ("free" . "fr") ("fill" . "fl"))
  "tipi")

(defparameter *skeleton-compito*
  "
(in-package :pedb)
(compito (:title \"Esercizi sul movimento\"  :rfoot \"MMM. YYYY qNcN (WNN)\")
  (columns (:context \"n=2, balance=no\")
    )
  (soluzioni ()))")


(defparameter *mpinclusion*
  "
\\setupMPinstance[method=decimal]
\\startMPinclusions
  input metaobj;;
    string LaTeXsetup ; LaTeXsetup := \"\";
    input \"makecirc.mp\";
    vardef latex(expr t) = textext(t) enddef ;

  vardef markanglebetween (expr endofa, endofb, common, length, str) =
        save curve, where ; path curve ; numeric where ;
        where := turningnumber (common--endofa--endofb--cycle) ;
        curve := (unitvector(endofa-common){(endofa-common) rotated (where*90)}
        .. unitvector(endofb-common)) scaled length shifted common ;
        draw thefreelabel(str,point .5 of curve,common) withcolor black ;
       curve
  enddef ;

  u:=5mm;
  def grid(expr xi,xf,yi,yf)=
    for i=xi+1 upto xf-1:
      
      draw (i*u,yi*u)..(i*u,yf*u) withcolor .7white ;
    endfor
    for i=yi+1 upto yf-1:
      draw (xi*u,i*u)..(xf*u,i*u) withcolor .7white;
    endfor
  enddef;
  
  def assi(expr xi,xf,yi,yf)=
    drawarrow (xi*u,0)--(xf*u,0);
    drawarrow (0,yi*u)--(0,yf*u);
    label.bot(btex $x$ etex, (xf*u,0));
    label.lft(btex $y$ etex, (0,yf*u));
  enddef;
  
  def labelgriglia(expr xi,xf,yi,yf)=
    for i=xi+1 upto xf-1:
      draw (i*u,-2pt)--(i*u,2pt);
      draw textext(decimal(i)) scaled .8 shifted (i*u-3pt,-5pt);    
 %     label.llft(decimal(i) infont \"ptmr\" scaled (6pt/fontsize defaultfont), (i*u,0));
    endfor
    for i=yi+1 upto yf-1:
      draw (-2pt,i*u)--(2pt,i*u);
      draw textext(decimal(i)) scaled .8 shifted (-6pt,i*u+4pt);      
%      label.llft(decimal(i) infont \"ptmr\" scaled (6pt/fontsize defaultfont), (0,i*u));
    endfor;

  enddef;
  def righello(expr strt, stp, btick, stick, meas)=
    draw (-5+strt*u,0)--(-5+strt*u,-1cm)--(stp*u+5,-1cm)--(stp*u+5,0)--cycle;
    for i= strt step btick until stp:
      draw (i*u,0)--(i*u,-0.3cm);
      label.bot(decimal i,(i*u,-0.3cm));
    endfor
    for i=strt step stick until stp:
      draw (i*u,0)--(i*u,-0.2cm);
    endfor;
    label(btex mm etex,((strt+stp)*u/2,-0.8cm));
    draw (strt*u,3)--(meas*u,3) withpen pensquare yscaled 3pt;
enddef;

def millimetrata (expr x, y)=
    u := 1mm;
    xmax:= 10 * x;
    ymax := 10 * y;
    color cc;
    cc := .5[red,white];
    pickup pencircle scaled 0.1;
% mm
    for i=0 step 1 until ymax:
       draw (0,i)*u -- (xmax,i)*u withcolor cc;
    endfor;
    for i=0 step 1 until xmax:
       draw (i,0)*u -- (i,ymax)*u withcolor cc;;
    endfor;

pickup pencircle scaled .5;
% 5mm
for i=0 step 5 until ymax:
  draw (0,i)*u -- (xmax,i)*u withcolor cc;
endfor;
for i=0 step 5 until xmax:
  draw (i,0)*u -- (i,ymax)*u withcolor cc;;
endfor;

pickup pencircle scaled 1;
% 10mm
for i=0 step 10 until ymax:
  draw (0,i)*u -- (xmax,i)*u withcolor cc;
endfor;
for i=0 step 10 until xmax:
  draw (i,0)*u -- (i,ymax)*u withcolor cc;;
endfor;
    
enddef;

def puntoconerrore (expr x, y, dx, dy)=
    draw ((x-dx)*u,(y-dy)*u)--((x-dx)*u,(y+dy)*u)--((x+dx)*u,(y+dy)*u)--((x+dx)*u,(y-dy)*u)--cycle;
    drawdot (x*u,y*u) withpen pencircle scaled 3 ;
enddef;


\\stopMPinclusions")

(def-authoring-tree pedb-document (authoring-document) :documentation "A pebb root document")
(def-authoring-tree compito (pedb-document) :documentation "Compito root document")


(defmethod export-document :around ((document pedb-document) (backend context-backend))
  (loop for ff in '("tex/didattica.tex" "tex/env_esercizi.tex" "tex/esercizi.lua") do
       (uiop:copy-file (merge-pathnames ff (asdf:system-source-directory :scliba))
    		       (make-pathname :name (pathname-name ff) :type (pathname-type ff) :directory *outdirectory*)))

  (format *outstream* "
% \\usepath[../..]
\\project didattica
~:[% ~;~]\\enablemode[soluzioni]
"

	  (get-argument document :soluzioni))
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
  (write-string *mpinclusion* *outstream*)
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
"
	  )
  (write-string *mpinclusion* *outstream* )
  (call-next-method))

(defmethod export-document :before ((document compito) (backend aut-context-backend))
  (export-document (footer (:left "" :right (get-argument document :rfoot))) backend)
  (export-document (title (get-argument document :title)) backend))


(defmacro infoform-1line ()
  "Form nome e cognome per compiti in una riga"
  `(table (:widths '(0.5 0.5) :frame nil :stretch t)
     (table-row ()
       (table-cell () "Alunno: " (hlinefill ()))
       (table-cell () "Classe: " (hlinefill ()) "Data: " (hlinefill ())))))

(defmacro infoform ()
  "Form nome e cognome per compiti"
  `(table (:widths '(0.5 0.5) :frame nil :stretch t)
     (table-row ()
       (table-cell () "Nome: " (hlinefill ()))
       (table-cell () "Classe: " (hlinefill ())))
     (table-row ()
       (table-cell () "Cognome: " (hlinefill ()))
       (table-cell () "Data: " (hlinefill ())))))

(def-enumerated esercizio (pedb-document)) ;"\\inleft{~d}"

(defmethod export-document ((document esercizio) (backend aut-context-backend))
  ;; (format *outstream* "~&\\inleft{~d}~%" (enumerated-n document))
  (format *outstream* "~&{\\bf ~d.} " (enumerated-n document))
  ;; (call-next-method)
  ;; (format *outstream*"~&~%")
  )

(defmethod export-document :after ((document esercizio) (backend aut-context-backend))
  (format *outstream* "~&\\blank~%~%"))


(defmethod export-document ((document esercizio) (backend html-backend))
  (html-output (:div :class "esercizio-head" (:h4 (who:fmt "Esercizio ~d" (enumerated-n document)))))
  (call-next-method)
  (format *outstream*"~&~%"))


(def-enumerated-slave-buffered soluzione esercizio) 
(defmethod export-document ((document soluzione) (backend aut-context-backend))
  (format *outstream* "~&~%{\\bf Soluzione ~d}~%" (enumerated-n document))
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
  (format *outstream* "~&{\\printsoluzioni}~%"))

(defmethod export-document ((document soluzioni) (backend autarchy-backend))
  (format *outstream* "~&~%~%{\\tfc Soluzioni. ~%~%}~a" (get-output-stream-string (cdr (assoc 'soluzione *buffers*)))))

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


;; ;aggiustare parent
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


;;; Compiling
(defclass mixin-pedb-backend (mixin-multiple-random-output-backend)
  ())

(defclass pedb-context-backend (mixin-pedb-backend context-backend)
  ())

(defclass pedb-aut-context-backend (mixin-pedb-backend aut-context-backend)
  ())

(defclass pedb-html-backend (html-backend)
  ())


(defparameter *backends* '(pedb-context-backend pedb-aut-context-backend pedb-html-backend))


;; ;; rivedere

(defun compito-lisp-file (name)
  "return the lisp file for compito"
  (make-pathname :name name :type "lisp" :directory (pathname-directory *compiti-directory*)))



(defun export-compito-in-all-backends (compito &key n)
  "genera all the backends for compito "
  (loop for backend in *backends*
     do (export-file (compito-lisp-file compito) (make-instance backend) :n n)))

(defun export-and-view-compito-in-all-backends (compito &key n)
  (export-compito-in-all-backends compito :n n)
  (compila-context (standard-output-file (compito-lisp-file compito) 'context-backend))
  (compila-context (standard-output-file (compito-lisp-file compito) 'aut-context-backend)))



;;;; Compiling functions
;; rimettere n con backend
(defun compila-guarda-compito (document &key (n 1) (directory *compiti-directory*) (soluzioni nil) (backend (make-instance 'pedb-context-backend)))
  (setf (backend-n backend) n)
  (compila-guarda (merge-pathnames document directory) backend)
  ;; (view-pdf (compila-context
  ;; 	     (export-file (merge-pathnames document directory) backend)))
  )

(defmethod export-document :after ((document esercizio) backend)
  (unless (authoring-tree-parent document)
    (format *outstream* (export-document-on-string (soluzioni ()) backend))))

(defun compila-esercizio-preview (document &key (backend *default-backend*))
  (compila document backend))

;;;; Skeletons
(defun new-compito (file)
  (with-open-file (stream file :direction :output :if-does-not-exist :create)
    (format stream "~a" *skeleton-compito*))
  (format nil "~a" file))


;; (defun genera-esercizio-preview (esercizio)
;;   (with-open-file (stream (merge-pathnames *esercizi-preview-directory* (make-pathname :name esercizio :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
;;     (let* ((backend (make-instance 'context-backend :stream stream))
;; 	   (*outstream* (backend-outstream backend)))
;;       (format stream "\\usepath[../..]~%\\project didattica~%")
;;       (export-document (read-file (merge-pathnames *esercizi-directory*
;; 						   (make-pathname :name esercizio :type "lisp"))) backend)
;;       (format stream "~%\\doifmode{soluzioni}{\\printsoluzioni}~%")))
;;   (let ((file (uiop:merge-pathnames* *esercizi-preview-directory*
;; 				     esercizio)))
;;     (compila-context file :mode "soluzioni")))


;; (defun compila-esercizio-preview (document &key (backend *default-backend*))
;;   (compila document backend))

;;; old to remove 

;; (defvar *i-compito* 0)



;; (defun compila-compito (compito &key n (directory *compiti-directory*) (backend-type 'context-backend))
;;   "genera il sorgente context dal sorgente lisp con la key :n genera random n compiti"
;;   (with-open-file (stream (merge-pathnames directory (make-pathname :name compito :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
;;     (let ((backend (make-instance backend-type :stream stream)))
;;       (let ((*outstream* (backend-outstream backend)))
;; 	(if n
;; 	    (let ((*randomize* t))
;; 	      (format stream "\\starttext~%")
;; 	      (dotimes (*i-compito* n)
;; 		(export-document (read-file (merge-pathnames directory (make-pathname :name compito :type "lisp"))) backend))
;; 	      (format stream "\\stoptext~%")
;; 	      )
;; 	    (export-document (read-file (merge-pathnames directory (make-pathname :name compito :type "lisp"))) backend))))))

;; (defun compila-context-compito (file &key (directory *compiti-directory*))
;;   (let ((file (uiop:merge-pathnames* directory file)))
;;     (compila-context file)))

;; (defun compila-context-esercizio (file)
;;   (let ((file (uiop:merge-pathnames* *esercizi-directory* file)))
;;     (compila-context file)))

;; (defun compila-guarda-compito (file &key n (directory *compiti-directory*) (soluzioni nil) (backend-type 'context-backend))
;;   (compila-compito file :n n :directory directory  :backend-type backend-type)
;;   (let ((file (uiop:merge-pathnames* directory file))
;; 	(file-pdf (uiop:merge-pathnames* directory (uiop:make-pathname* :name file :type "pdf"))))
;;     (if soluzioni
;; 	(compila-context file :mode "soluzioni")
;; 	(compila-context file))
;;     (view-pdf file-pdf)))

;; (defun compila-guarda-compito-soluzioni (file &key n (directory *compiti-directory*))
;;   (compila-guarda-compito file :n n :directory directory :soluzioni t))

;; (defun genera-esercizio-preview (esercizio)
;;   (with-open-file (stream (merge-pathnames *esercizi-preview-directory* (make-pathname :name esercizio :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
;;     (let* ((backend (make-instance 'context-backend :stream stream))
;; 	   (*outstream* (backend-outstream backend)))
;;       (format stream "\\usepath[../..]~%\\project didattica~%")
;;       (export-document (read-file (merge-pathnames *esercizi-directory*
;; 						   (make-pathname :name esercizio :type "lisp"))) backend)
;;       (format stream "~%\\doifmode{soluzioni}{\\printsoluzioni}~%")))
;;   (let ((file (uiop:merge-pathnames* *esercizi-preview-directory*
;; 				     esercizio)))
;;     (compila-context file :mode "soluzioni")))

;; (defun pedb-all-exercize ()
;;   (with-open-file (stream (merge-pathnames *eserciziari-directory* "all-exercise.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
;;     (let* ((*section-level* 1)
;; 	   (backend (make-instance 'context-backend :stream stream))
;; 	   (*outstream* (backend-outstream backend)))
;;       (export-document (raccolta-esercizi) backend))))


#|


|#
