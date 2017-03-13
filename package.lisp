;;;; package.lisp

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
	   
	   #:input
	   #:read-file
	   
	   #:authoring-tree
	   #:authoring-tree-arguments
	   #:authoring-tree-body
	   #:get-argument
	   #:export-document
	   #:export-document-on-string
	   
	   #:backend
	   #:backend-outstream
	   #:context-backend
	   #:autarchy-backend
	   #:aut-context-backend

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
	   #:section
	   #:*section-level*
	   #:*section-context-labels*
	   #:bf
	   #:framedtext
	   #:columns
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
	   #:guarda
	   #:pq-format
	   #:number-format
	   #:pq-change-unit
	   #:hlinefill
	   #:title
	   #:centering
	   #:big
	   #:footer))

; to use antik package without name conflict
;;(antik:make-user-package "SCLIBA")

(defpackage #:scliba-formatter
  (:use #:cl #:scliba)
  (:export n
	   #:number-format
	   #:number-scientific-notation
	   #:pq-format)
  (:nicknames #:scliba-f))

