(in-package #:scliba)
(defparameter *s-o-u* '(:si :deg))

(define-units 'time '((hour (* 3600 second) (hr hours) "h")))
(define-units 'time '((minute (* 60 second) (min minutes) "min")))
(define-units 'length '((decimeter     0.1       (dm decimeters) "dm")))

(define-units 'pressure
    '((pascal     (/ newton (* meter meter)) (pa) "Pa") ))

(define-units 'current '((ampere     1       (amperes amps amp) "A")
			 (milliampere     (* milli ampere) (milliamp milliamps ma) "mA")
			 (microampere     (* micro ampere) (microamp microamps ua) "\\mu A")))

(define-units 'electric-potential
    '((volt      (/ (* kilogram meter meter)
		  (* ampere second second second))
		 (v volts) "V")
      (millivolt (* milli volt)  (mv millivolts) "mV")
      (microvolt (* micro volt)  (uv microvolts) "\\mu V")
      ))

(define-units 'resistance
    '((ohm      (/ (* kilogram meter meter)
		   (* ampere ampere second second second))
		(ohms) "\\Omega")
      (kilohm   (* kilo ohm)     (kilohms) "k\\Omega")
      (megohm   (* mega ohm)     (megohms) "M\\Omega")
      
       ))

(define-units 'power
    '((watt       (/ (* kilogram meter meter) (* second second second))
		  (w watts) "W")
      (milliwatt  (* milli watt) (mlw milli-watt milli-watts) "mW")
      (microwatt  (* micro watt) (uw micro-watt micro-watts) "\\mu W")
      (kilowatt   (* kilo watt) (kw kilowatts) "kW")
      (megawatt   (* mega watt) (mw megawatts mega-watt mega-watts) "MW")
      (gigawatt   (* giga watt) (gw gigawatts giga-watt giga-watts) "GW")
      (horsepower (* 550 (/ (* foot pound-force) second)) (hp) "hp")))

(define-units 'temperature
    '((kelvin      1.0       (k kelvin kelvins) "K")
      (celsius     1.0       (celsius) "°C")
      (rankine     5/9       ()) ))

(define-units 'charge
    '((coulomb     (* ampere second)     (coul coulombs) "C")))

;; (antik::define-derived-dimensions
;;     '((elastic-costant (/ force length))))

;; (define-units 'elastic-costant
;;     '((si-elastic-costant (/ newton meter) (newton/meter N/m) "N/m")))

;; print number

(defun number-exponent (num)
  (floor (log (abs num) 10)))

(defun number-parts (num &key (precision 3))
  (let* ((fmt (format nil "~~,~d,1,1,,,'ee" (1- precision)))
	 (parts (map 'list #'read-from-string (remove "" (ppcre:split "e|\\+" (format nil fmt num)) :test #'string=))))
    (values-list parts)))


(defun number-format% (num &key (precision 3) (exponent 0) &allow-other-keys)
  (if (zerop num)
      "0"
      (multiple-value-bind (n exp) (number-parts num :precision precision)
        (let* ((exponent (or exponent (number-exponent num)))
               (k (- exp exponent))
               (d (- precision k 1))
               (fmt (format nil "~~,~d,~df" d k))
               (strn% (format nil fmt n))
               (strn (if (or (char= #\0 (elt (remove-if (lambda (x) (char= x #\-)) strn%) 0))
                             (< 0 d))
                         strn%
                         (ppcre:regex-replace "\\..*" strn% ""))))
          (ppcre:regex-replace "\\." (format nil "\\text{~a}~[~:;\\times 10^\{~a\}~]" strn exponent exponent) ",")))))


(define-system-of-units si-my-mod (deg hz) :si)
(set-system-of-units :si-my-mod)

(defun print-unit (unit)
  (cond
    ((null unit) nil)
    ((numberp unit) (format nil "~a" unit))
    ((symbolp unit)
     (let ((print-names (print-name (get-canonical-name unit))))
       (if (listp print-names)
	   (first print-names)
	   print-names)))
    ((stringp unit) unit)
    ((eql '/ (first unit)) (format nil "~a/~a" (print-unit (second unit)) (print-unit (third unit))))
    ((eql 'expt (first unit)) (format nil "~a^{~a}" (print-unit (second unit)) (print-unit (third unit))))
    ((eql '* (first unit)) (format nil "~a\\ ~a" (print-unit (second unit)) (print-unit (third unit))))
    (t "unknown"))
  ;; (when (and unit (not (listp unit)))
  ;;   (let ((print-names (print-name (get-canonical-name unit))))
  ;;    (if (listp print-names)
  ;; 	 (first print-names)
  ;; 	 print-names)))
  )



(defclass pq-format  (authoring-tree)
  ((pq :accessor pq-format-pq
       :initarg :pq
       :initform nil)
   (precision :accessor pq-format-precision
	      :initarg :precision
	      :initform 3)
   (exponent :accessor pq-format-exponent
	     :initarg :exponent
	     :initform 0)
   (s-o-u :accessor pq-format-s-o-u
	  :initarg :s-o-u
	  :initform *s-o-u*)
   (new-unit :accessor pq-format-new-unit
       :initarg :new-unit
       :initform nil)))

(defgeneric pq-format (pq &key &allow-other-keys))

(defmethod pq-format ((pq physical-quantity) &key (precision 3) (exponent 0 exponent-p) (s-o-u *s-o-u*) (new-unit nil new-unit-p) &allow-other-keys)
  "format the physical quantity"
  (make-instance 'pq-format :pq pq :precision precision :exponent exponent :s-o-u s-o-u :new-unit new-unit))

(defmethod pq-format (pq  &key  &allow-other-keys)
  "format the physical quantity"
  "--")

(defmethod pq-format ((pq real)  &rest keys &key  &allow-other-keys)
  (apply  #'number-format%  pq keys))

(defmethod pq-format ((pq cons)  &rest keys &key  &allow-other-keys )
  "format the physical quantity"
  (list "["(apply #'make-instance 'pq-format :pq (car pq)  keys) " -- " (apply #'make-instance 'pq-format :pq (cdr pq)  keys) "]"))

(defmethod export-document ((document pq-format) (backend mixin-context-backend))
  (with-slots (pq precision exponent s-o-u new-unit) document
    (with-nf-options (:system-of-units (funcall #'antik::make-sysunits (cdr s-o-u) (car s-o-u)))
      (multiple-value-bind (num unit) (pqval pq)
	(let* ((exponent (or exponent (number-exponent num)))
	       (unit (or (print-unit new-unit) (print-unit unit)))
	       (str (format nil "~a~:[\\ ~;~]~@[{\\rm ~a}~]"
			    (number-format% num :precision precision :exponent exponent)
			    (or (not unit) (string= unit (print-unit :degree)))
			    unit)))
	  (unless *math* (setf str (concatenate 'string "$" str "$")))
	  (format *outstream* "~a" str))))))

(defun pq-change-unit (pq new-unit)
  "Return a new physical-quantity with the same value of PQ and NEW-UNIT as unit"
  (make-pq (pqval pq) new-unit))
