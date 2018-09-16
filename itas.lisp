;;;; Un pacchetto per documenti dell'ITAS Anzilotti


(defpackage #:scliba-itas
  (:use #:cl #:scliba)
  (:nicknames #:itas))

(in-package #:scliba-itas)


(defclass tabella-itas () ())

(defclass itas-document (authoring-document) ())

(def-authoring-tree relazione-finale (itas-document))

(defmethod export-document :before ((document tabella-itas) (backend mixin-context-backend))
  (export-document (newline ()) backend))

(def-authoring-tree programmazione (authoring-document tabella-itas) :documentation "programmazione modulare")


(defmethod export-document :before ((document programmazione) (backend mixin-context-backend))
  (format *outstream*
	  "
\\mainlanguage[italian]
\\setuppapersize[A4][A4]
\\setuplayout[
  topspace=1cm,
  backspace=2cm,
  rightmargin=3cm,
%  width=,
  %height=middle,
  header=0pt]

\\setuppagenumbering[alternative=singlesided,location={footer, center}]


")
  (with-document-arguments (title docente materia classe) document
    (export-document
     (table (:frame t :widths '(1) :stretch t)
       (table-row () (table-cell (:nc 2) (bf (format nil "DOCENTE: ~a" docente))))
       (table-row ()
	 (table-cell (:frame '(1 1 nil 1)) (bf (format nil "Materia: ~a" materia)))
	 (table-cell (:frame '(nil 1 1 1) ) (bf (format nil "Classe: ~a" classe)))))
     
     backend)))

(def-counter modulo)
(def-counter unita)

(defclass unita-di-programmazione () ())

(defun durata-ore (unita)
  (with-document-arguments (durata) unita
    (if durata durata 
	(loop for i in (authoring-tree-body unita)
	   when (typep i 'authoring-tree) 
		summing (durata-ore i))
	)))

(def-authoring-tree modulo0 (authoring-tree tabella-itas unita-di-programmazione) :documentation "modulo accoglienza della programmazione")



(defmethod export-document :before ((document modulo0) (backend autarchy-backend))
  (with-document-arguments (title descrizione prerequisiti metodologie valutazione) document
      
    (export-document
     (table (:frame t :stretch t)
       (table-row ()
	 (table-cell (:nc 3) (bf (format nil "MODULO N°~d " (counter-modulo-val))) title (hfill ()) (bf (format nil "  durata ore ~d" (durata-ore document)))) 
	 )
       (table-row ()
	 (table-cell (:nc 3)  descrizione))
       (table-row ()
	 (table-cell ()  (bf "Prerequisiti"))
	 (table-cell ()  (bf "Metodologie e strategie operative"))
	 (table-cell ()  (bf "Modalità di valutazione")))
       (table-row ()
	 (table-cell () prerequisiti)
	 (table-cell () metodologie)
	 (table-cell () valutazione)))
     backend)
    ))

(def-authoring-tree u-didattica0 (authoring-tree tabella-itas unita-di-programmazione) :documentation "ud  accoglienza della programmazione")

(defmethod export-document :before ((document u-didattica0) (backend autarchy-backend))
  (counter-unita-inc)
  (with-document-arguments (title attivita obiettivi durata split) document

    (export-document
     (table (:frame t :stretch t :split split )
       (table-row ()
	 (table-cell (:nc 2) (bf (format nil "UNITÀ DIDATTICA N°~d.~d " (counter-modulo-val) (counter-unita-val))) title (hfill ()) (bf (format nil " durata ore ~d" durata))))
       (table-row ()
	 (table-cell () (bf "Attività"))
	 (table-cell () (bf "Obiettivi")))
       (table-row ()
	 (table-cell () attivita)
	 (table-cell () obiettivi)))
     backend)))

(def-authoring-tree modulo (authoring-tree) :documentation "modulo della programmazione")

(defmethod export-document :before ((document modulo) (backend autarchy-backend))
  (counter-modulo-inc)
  (counter-unita-set)
  (with-document-arguments (title competenze conoscenze livelli-minimi prerequisiti metodologie valutazione split) document
      
    (export-document
     (table (:frame t :stretch t :split split)
       (table-row ()
	 (table-cell (:nc 3) (bf (format nil "MODULO N°~d " (counter-modulo-val))) title (hfill ()) (bf (format nil "  durata ore ~d" (durata-ore document)))))
       (table-row ()
	 (table-cell ()  (bf "Competenze"))
	 (table-cell ()  (bf "Conoscenze"))
	 (table-cell ()  (bf "Livelli minimi")))
       (table-row ()
	 (table-cell () competenze)
	 (table-cell () conoscenze)
	 (table-cell () livelli-minimi))
       (table-row ()
	 (table-cell ()  (bf "Prerequisiti"))
	 (table-cell ()  (bf "Metodologie e strategie operative"))
	 (table-cell ()  (bf "Modalità di valutazione")))
       (table-row ()
	 (table-cell () prerequisiti)
	 (table-cell () metodologie)
	 (table-cell () valutazione)))
     
     backend)
    ))

(defmethod export-document :after ((document modulo) (backend autarchy-backend))
  (with-document-arguments (recupero valutazione-modulo) document
      
    (export-document
     (table (:frame t :stretch t)
       (table-row ()
	 (table-cell () (bf (format nil "ATTIVITÀ DI RECUPERO"))))
       (table-row () (table-cell () recupero))
       (table-row ()
	 (table-cell () (bf (format nil "VALUTAZIONE SOMMATIVA DEL MODULO"))))
       (table-row () (table-cell () valutazione-modulo)))
     
     backend)))

(def-authoring-tree u-didattica (authoring-tree) :documentation "ud della programmazione")

(defmethod export-document :before ((document u-didattica) (backend autarchy-backend))
  (counter-unita-inc)
  (with-document-arguments (title competenze conoscenze livelli-minimi prerequisiti metodologie valutazione split) document
      
    (export-document
     (table (:frame t :stretch t :split split)
       (table-row ()
	 (table-cell (:nc 3) (bf (format nil "Unità didattica N°~d.~d " (counter-modulo-val) (counter-unita-val))) title (hfill ()) (bf (format nil "durata ore ~d" (durata-ore document)))))
       (table-row ()
	 (table-cell ()  (bf "Competenze"))
	 (table-cell ()  (bf "Conoscenze"))
	 (table-cell ()  (bf "Livelli minimi")))
       (table-row ()
	 (table-cell () competenze)
	 (table-cell () conoscenze)
	 (table-cell () livelli-minimi))
       (table-row ()
	 (table-cell ()  (bf "Prerequisiti"))
	 (table-cell ()  (bf "Metodologie e strategie operative"))
	 (table-cell ()  (bf "Modalità di valutazione")))
       (table-row ()
	 (table-cell () prerequisiti)
	 (table-cell () metodologie)
	 (table-cell () valutazione)))
     
     backend)
    ))


(defparameter *testo-valutazione*
      "Scaturisce da verifiche orali e scritte ,dalla considerazione
       dell’atteggiamento, dell’impegno (in particolare per le attività in
       laboratorio si terrà conto dell’autonomia nell’operare e della
       puntualità nella riconsegna delle relazioni sugli esperimenti
       realizzati), dei progressi realizzati e delle competenze acquisite.")

(defun testo-recupero (rif)
  "esempio rif '((1 1) (2 3 4))"
  
  (let ((out nil)) 
    (push  (format nil "È prevista la possibilità di ore destinate al recupero sia in orario
    curricolare (recupero in itinere), sia in orario extracurricolare
    pomeridiano. In particolare è necessaria l’acquisizione delle ") out)
    (push  (underbar "conoscenze") out)
    (push (format nil " relative ") out)
    (loop for i in rif
       with n = 0
       do
	 (incf n)
	 (let* ((mod (car i))
		(uds (cdr i))
		(n-uds (length uds)))
	   (unless (= n 1) (push  (format nil " e ") out))
	   (if uds
	       (progn
		 (push  (format nil "~:[alle ~;all'~]" (= 1 n-uds)) out)
		 (push  (if (= 1 n-uds)
			    (bf (format nil "Unità didattica ~d " (car uds)))
			    (bf (format nil "Unità didattiche ~{~d~#[ ~; e ~:;, ~]~}" uds))) out)
		 (push  (format nil "del ") out))
	       (push  (format nil "al ") out))
	   (push  (bf (format nil "Modulo ~d " mod)) out))
	 )
    (push  (format nil "per un proficuo prosieguo dello studio della disciplina, ulteriori azioni di
    recupero nel corso dell’anno saranno volte a raggiungere i livelli
    minimi e integrare le competenze.") out)
    (nreverse out)))


(defun riassunto-didattica (dida)
  (format t "~{~a~}~%" (loop for i below 50 collect "-"))
  (loop for i in (authoring-tree-body dida) do
       (format t "~31a~10@a~%" (subseq (get-argument i :title) 0 (min 30 (length (get-argument i :title)))) (durata-ore i))
       (loop for j in (authoring-tree-body i) do
	    (format t "     ~20a ~8@a~%" (subseq (get-argument j :title) 0 (min 19 (length (get-argument j :title)))) (durata-ore j))))
  (format t "~{~a~}~%" (loop for i below 50 collect "-"))
  (format t "~41@a" (durata-ore dida)))

