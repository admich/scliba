;;;; package.lisp
(in-package #:cl-user)
(defpackage #:scliba
  (:use #:cl #:alexandria  #:antik ;; #:local-time conflict with antik
	)
  ;; antik::*antik-user-shadow-symbols*
  (:shadowing-import-from #:antik #:MAXIMIZING #:MAXIMIZE #:MINIMIZING #:MINIMIZE
 #:MULTIPLYING #:MULTIPLY #:SUMMING #:SUM #:FOR #:TIME
 #:LENGTH #:DECF #:INCF #:SIGNUM #:ROUND #:FLOOR
 #:COERCE #:< #:<= #:> #:>= #:= #:MAX #:MIN
 #:ZEROP #:MINUSP #:PLUSP #:ABS #:EXP #:LOG #:EXPT
 #:SQRT #:TANH #:COSH #:SINH #:ATAN #:ACOS #:ASIN
 #:TAN #:COS #:SIN #:/ #:* #:- #:+ GRID:AREF
 #:POLAR-TO-RECTANGULAR #:RECTANGULAR-TO-POLAR #:ACCELERATION
 #:PSI #:KNOTS #:ROTATE)
  (:export #:*math*
	   #:*debug*
	   #:*randomize*
	   #:*top-level-document*
	   
	   #:input
	   #:read-file
	   
	   #:authoring-tree
	   #:authoring-tree-arguments
	   #:authoring-tree-body
	   #:authoring-tree-parent
	   #:get-argument
	   #:export-document
	   #:export-document-on-string
	   
	   #:backend
	   #:backend-outstream
	   #:context-backend
	   #:autarchy-backend
	   #:aut-context-backend
	   #:mixin-context-backend
	   #:def-authoring-tree
	   #:def-simple-authoring-tree
	   #:def-startstop
           
	   #:choose-one-of
           
	   
	   #:authoring-document
	   #:startstop
	   #:input-tex
	   #:random-body
	   #:par
	   #:hline
	   #:footnote
	   #:book
	   #:section
	   #:section-n
	   #:counter-section-inc
	   #:counter-section-set
	   #:counter-section-val
	   #:*section-level*
	   #:*section-context-labels*
	   #:bf
	   #:framedtext
	   #:columns
	   #:align-right
	   #:randomize
	   #:itemize
	   #:item
	   #:it
	   #:newpage
	   #:emph
	   #:newcolumn
	   #:math
	   #:ref
	   #:figure
	   #:mpcode
	   #:table
	   #:table-row
	   #:table-cell
	   #:imath
	   #:phys-n
	   #:formula
	   #:compila-context
	   #:pq-format
	   #:number-format
	   #:pq-change-unit
	   #:hlinefill
	   #:title
	   #:centering
	   ;; #:big
	   #:tfxx
	   #:tfx
	   #:tf
	   #:tfa
	   #:tfb
	   #:tfc
	   #:tfd
	   #:tfe
	   #:footer
	   #:def-counter
	   #:def-enumerated
	   #:*outstream*
	   #:*outdirectory*
	   #:*buffers*
	   #:buffered
	   #:def-buffer
	   #:def-buffered
	   #:def-enumerated-slave
	   #:def-enumerated-slave-buffered
	   #:*counters*
	   #:reset-all-counters
	   #:export-standard-file
	   #:html-backend
	   #:view-html
	   #:view-pdf
	   #:standard-output-file
	   #:export-file
	   #:enumerated
	   #:enumerated-n
	   #:html-output
	   #:*i-random*
	   #:with-document-argument
	   #:newline
	   #:nbsp
	   #:compila-guarda
	   #:*main-backend*
	   #:*section-head-fn*
	   #:roman
	   #:*section-fonts*
	   #:sans-serif
	   #:small-caps
	   #:section-level
	   #:*default-backend*
	   #:compila
	   #:*current-node*))

; to use antik package without name conflict
;;(antik:make-user-package "SCLIBA")

(defpackage #:scliba-formatter
  (:use #:cl #:scliba)
  (:export n
	   #:number-format
	   #:number-scientific-notation
	   #:pq-format)
  (:nicknames #:scliba-f))

(defpackage #:physics
  (:use #:cl )
  (:nicknames #:phys)
  (:export
   #:bigg
   #:littleg))

(antik:make-user-package :physics)

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
  (:nicknames #:pedb)
  (:export
   #:*esercizi-directory*
   #:*compiti-directory*
   #:*eserciziari-directory*
   #:*esercizi-preview-directory*
   #:*mpinclusion*
   #:new-compito
   #:pedb-all-exercize
   #:tutti-esercizi
   #:compila-esercizio-preview))


(defpackage #:scliba-ptnh
  (:use #:cl #:scliba #:pedb)
  (:nicknames #:ptnh))

