;;;; package.lisp
(in-package #:cl-user)

(defpackage #:scliba
  (:use #:cl ;; #:antik ;; #:local-time conflict with antik
	    )
  (:import-from #:alexandria #:compose #:ensure-list #:flatten #:symbolicate #:first-elt #:shuffle)
  ;; antik::*antik-user-shadow-symbols*
  ;; (:shadowing-import-from #:antik #:MAXIMIZING #:MAXIMIZE #:MINIMIZING #:MINIMIZE
  ;; #:MULTIPLYING #:MULTIPLY #:SUMMING #:SUM #:FOR #:TIME
  ;; #:LENGTH #:DECF #:INCF #:SIGNUM #:ROUND #:FLOOR
  ;; #:COERCE #:< #:<= #:> #:>= #:= #:MAX #:MIN
  ;; #:ZEROP #:MINUSP #:PLUSP #:ABS #:EXP #:LOG #:EXPT
  ;; #:SQRT #:TANH #:COSH #:SINH #:ATAN #:ACOS #:ASIN
  ;; #:TAN #:COS #:SIN #:/ #:* #:- #:+ GRID:AREF
  ;; #:POLAR-TO-RECTANGULAR #:RECTANGULAR-TO-POLAR #:ACCELERATION
  ;; #:PSI #:KNOTS #:ROTATE)
  (:shadow #:columns)
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
	       #:authoring-tree-source-file
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
           #:mischia	   
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
	       #:framed
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
           #:table-float
	       #:mpcode
	       #:table
	       #:table-row
	       #:table-cell
	       #:imath
	       #:formula
	       #:compila-context
	       #:pq-format
	       #:number-format
	       #:pq-change-unit
	       #:hlinefill
	       #:title
	       #:centering
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
	       #:*output-file*
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
	       #:with-document-arguments
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
	       #:*current-node*
	       #:mixin-multiple-random-output-backend
	       #:backend-n
	       #:simple-table-rows
	       #:hss
	       #:inmargin
	       #:inframed
	       #:*s-o-u*)
  (:export
   #:scliba-physical-quantity
   #:pq-exponent
   #:pq-new-unit
   #:pq-s-o-u
   #:pq-precision
   #:hfil
   #:hfill
   #:underbar
   #:simple-itemize
   #:vspace
   #:simple-table-by-rows))

(antik:make-user-package :scliba)


(defpackage #:physics
  (:use #:cl #:scliba)
  (:shadowing-import-from #:scliba #:columns)
  (:nicknames #:phys)
  (:export
   #:big-g
   #:little-g
   #:costante-stefan-boltzmann
   #:densita))

(antik:make-user-package :physics)

(defpackage #:scliba-pedb
  (:use #:cl  #:scliba #:physics)
  (:import-from #:alexandria #:with-unique-names #:flatten)
  (:shadowing-import-from #:scliba #:columns)
  ;; (:shadowing-import-from #:antik #:MAXIMIZING #:MAXIMIZE #:MINIMIZING #:MINIMIZE
  ;; #:MULTIPLYING #:MULTIPLY #:SUMMING #:SUM #:FOR #:TIME
  ;; #:LENGTH #:DECF #:INCF #:SIGNUM #:ROUND #:FLOOR
  ;; #:COERCE #:< #:<= #:> #:>= #:= #:MAX #:MIN
  ;; #:ZEROP #:MINUSP #:PLUSP #:ABS #:EXP #:LOG #:EXPT
  ;; #:SQRT #:TANH #:COSH #:SINH #:ATAN #:ACOS #:ASIN
  ;; #:TAN #:COS #:SIN #:/ #:* #:- #:+ GRID:AREF
  ;; #:POLAR-TO-RECTANGULAR #:RECTANGULAR-TO-POLAR #:ACCELERATION
  ;; #:PSI #:KNOTS #:ROTATE)
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
   #:compila-esercizio-preview
   #:*esercizi-argomenti*
   #:input-esercizio
   #:esercizi
   #:genera-all-exercize))

(antik:make-user-package :scliba-pedb)

(defpackage #:scliba-ptnh
  (:use #:cl #:scliba #:pedb #:physics)
    (:shadowing-import-from #:scliba #:columns)
  (:nicknames #:ptnh))

(antik:make-user-package :scliba-ptnh)
