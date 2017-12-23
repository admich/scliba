;;;; Un pacchetto per documenti dell'ITAS Anzilotti

(defpackage #:scliba-itas
  (:use #:cl #:scliba)
  (:nicknames #:itas))

(in-package #:scliba-itas)


(defclass tabella-itas () ())
(defmethod export-document :before ((document tabella-itas) (backend mixin-context-backend))
  (export-document (newline ()) backend))

(def-authoring-tree programmazione (authoring-document tabella-itas) :documentation "programmazione modulare")


(defmethod export-document :before ((document programmazione) (backend mixin-context-backend))
  (with-document-argument (title docente materia classe) document
    (export-document
     (table (:frame t :widths '(1))
       (table-row () (table-cell () (bf (format nil "DOCENTE: ~a" docente))))
       (table-row ()
	 (table-cell () (bf (format nil "Materia: ~a Classe: ~a" materia classe)))))
     
     backend)))

(def-authoring-tree modulo0 (authoring-tree tabella-itas) :documentation "modulo accoglienza della programmazione")

(defmethod export-document :before ((document modulo0) (backend mixin-context-backend))
  (with-document-argument (title descrizione prerequisiti metodologie valutazione) document
      
      (export-document
       (table (:frame t :widths '(0.3333 0.3333 0.3333))
	 (table-row ()
	   (table-cell (:nc 3) (bf (format nil "MODULO N fare ")) title (bf "durata fare ore")))
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

(def-authoring-tree u-didattica0 (authoring-tree tabella-itas) :documentation "ud  accoglienza della programmazione")
(defmethod export-document :before ((document u-didattica0) (backend mixin-context-backend))
  (with-document-argument (title attivita obiettivi durata) document
      
      (export-document
       (table (:frame t :widths '(0.5 0.5))
	 (table-row ()
	   (table-cell (:nc 2) (bf (format nil "UNITA' DIDATTICA N fare ")) title (format nil " durata ore ~d" durata)))
	 (table-row ()
	   (table-cell ()  (bf "Attività"))
	   (table-cell ()  (bf "Obiettivi"))
	   )
	 (table-row ()
	   (table-cell () attivita)
	   (table-cell () obiettivi)
	   ))
       backend)
    )
  )

(def-authoring-tree modulo (authoring-tree) :documentation "modulo della programmazione")

(def-authoring-tree u-didattica (authoring-tree) :documentation "ud della programmazione")


