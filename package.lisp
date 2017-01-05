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
  (:export *math*))

; to use antik package without name conflict
;;(antik:make-user-package "SCLIBA")

(defpackage #:scliba-formatter
  (:use #:cl #:scliba)
  (:export n
	   #:number-format
	   #:number-scientific-notation
	   #:pq-format)
  (:nicknames #:scliba-f))

