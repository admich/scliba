(in-package #:scliba)
;; hacking maybe to remove
(define-units 'time '((hour (* 3600 second) (hr hours) "h")))
(define-units 'length '((decimeter     0.1       (dm decimeters) "dm")))

;; print number

(defun number-exponent (num)
  (floor (log (abs num) 10)))

(defun number-parts (num &key (precision 3))
  (let* ((fmt (format nil "~~,~d,1,1,,,'ee" (1- precision)))
	 (parts (map 'list #'read-from-string (remove "" (ppcre:split "e|\\+" (format nil fmt num)) :test #'string=))))
    (values-list parts)))


(defun number-format% (num &key (precision 3) (exponent 0))
  (multiple-value-bind (n exp) (number-parts num :precision precision)
    (let* ((k (- exp exponent))
	   (d (- precision k 1))
	   (fmt (format nil "~~,~d,~df" d k))
	   (strn% (format nil fmt n))
	   (strn (if (or (char= #\0 (elt (remove-if (lambda (x) (char= x #\-)) strn%) 0))
			 (< 0 d))
		     strn%
		     (ppcre:regex-replace "\\..*" strn% ""))))
      (ppcre:regex-replace "\\." (format nil "\\text{~a}~[~:;\\times 10^\{~a\}~]" strn exponent exponent) ","))))

(defun number-format (num &key (precision 3) (exponent 0))
  (let ((str (number-format% num :precision precision :exponent exponent)))
    (if *math* str (concatenate 'string "$" str "$")))
  ;; (multiple-value-bind (n exp) (number-parts num :precision precision)
  ;;   (let* ((k (- exp exponent))
  ;; 	   (d (- precision k 1))
  ;; 	   (fmt (format nil "~~,~d,~df" d k))
  ;; 	   (strn% (format nil fmt n))
  ;; 	   (strn (if (or (char= #\0 (elt (remove-if (lambda (x) (char= x #\-)) strn%) 0))
  ;; 			 (< 0 d))
  ;; 		     strn%
  ;; 		     (ppcre:regex-replace "\\..*" strn% ""))))
  ;;     (let ((str (ppcre:regex-replace "\\." (format nil "~a~[~:;\\times 10^\{~a\}~]" strn exponent exponent) ",")))
  ;;      (if *math* str (concatenate 'string "$" str "$")))))
  )

;; (with-nf-options (:significant-figures 1) (with-system-of-units (:si km deg) (nf #_45.0_deg)))
;; (antik::make-pq-string (nth-value 1 (pqval #_3_km)))

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

(defun pq-change-unit (pq unit)
  (make-pq (pqval pq) unit))

;; (defmacro pq-format (pq &key (precision 3) (exponent 0) (s-o-u (list :si)) (new-unit nil))
;;   `(with-system-of-units ,s-o-u (pq-format% ,pq :precision ,precision :exponent ,exponent :new-unit ,new-unit)))

;; (let ((um :km) (si :si)) (with-nf-options (:system-of-units (funcall #'antik::make-sysunits (list um) si))  (pq-format #_3_m)))

(defun pq-format (pq &key (precision 3) (exponent 0) (s-o-u (list :si)) (new-unit nil))
  "format the physical quantity"
  (with-nf-options (:system-of-units (funcall #'antik::make-sysunits (cdr s-o-u) (car s-o-u)))
      (multiple-value-bind (num unit) (pqval pq)
	(let* ((exponent (or exponent (number-exponent num)))
	       (unit (or (print-unit new-unit) (print-unit unit)))
	       (str (format nil "~a~:[\\ ~;~]~@[{\\rm ~a}~]"
			 (number-format% num :precision precision :exponent exponent)
			 (or (not unit) (string= unit (print-unit :degree)))
			 unit)))
	  (if *math* str (concatenate 'string "$" str "$"))))))

;; (defun pq-format (pq &key (precision 3) (exponent 0) (s-o-u :si) (new-unit nil))
;;   "format the physical quantity"
  
;;   (multiple-value-bind (num unit) (pqval pq)
;;     (let* ((exponent (or exponent (number-exponent num)))
;; 	   (unit (or (print-unit new-unit) (print-unit unit)))
;; 	   (str (format nil "~a~:[\\ ~;~]{\\rm ~a}"
;; 			(number-format% num :precision precision :exponent exponent)
;; 			(string= unit (print-unit :degree))
;; 			unit)))
;;       (if *math* str (concatenate 'string "$" str "$")))))

(defun number-scientific-notation (num &key (precision 3))
  (let ((exp (number-exponent num)))
    (number-format num :precision precision :exponent exp)))

(defun n (stream arg colon-p at-p &optional (precision 3) (exponent 0))
  (cond
    (colon-p (format stream "~a" (number-scientific-notation arg :precision precision)))
    (at-p (format stream "~a" (number-format arg :precision precision :exponent exponent)))
    ((and (floatp arg) (< (abs arg) 1000) (>= (abs arg) 0.01)) (format stream "~a" (number-format arg :precision precision :exponent exponent)))
    ((floatp arg) (format stream "~a" (number-scientific-notation arg :precision precision)))
    (t (format stream "~a" arg))))

;; (defun print-float (stream arg colonp atp
;;                     &optional
;;                       (point-char #\.)
;;                       (comma-char #\,)
;;                       (comma-interval 3))
;;   "A function for printing floating point numbers, with an interface
;; suitable for use with the tilde-slash FORMAT directive.  The full form
;; is 

;;     ~point-char,comma-char,comma-interval/print-float/

;; The point-char is used in place of the decimal point, and defaults to
;; #\\.  If : is specified, then the whole part of the number will be
;; grouped in the same manner as ~D, using COMMA-CHAR and COMMA-INTERVAL.
;; If @ is specified, then the sign is always printed."
;;   (let* ((sign (if (minusp arg) "-" (if (and atp (plusp arg)) "+" "")))
;;          (output (format nil "~F" arg))
;;          (point (position #\. output :test 'char=))
;;          (whole (subseq output (if (minusp arg) 1 0) point))
;;          (fractional (subseq output (1+ point))))
;;     (when colonp
;;       (setf whole (inject-comma whole comma-char comma-interval)))
;;     (format stream "~A~A~C~A"
;;             sign whole point-char fractional)))

;; Here are some examples:

;; (progn 
;;   ;; with @ (for sign) and : (for grouping)
;;   (format t "~','.2@:/print-float/ ~%" 12345.6789) ;=> +1.23.45,679

;;   ;; with no @ (no sign) and : (for grouping)
;;   (format t "~'.'_3:/print-float/ ~%" 12345.678)   ;=>  12_345.678

;;   ;; no @ (but sign, since negative) and : (for grouping)
;;   (format t "~'.'_3:/print-float/ ~%" -12345.678)  ;=> -12_345.678

;;   ;; no @ (no sign) and no : (no grouping)
;;   (format t "~'.'_3@/print-float/ ~%" 12345.678))  ;=> +12345.678 (no



