(in-package #:pedb)



(setf regexp-var "\\\\var\\[([A-Za-z0-9]*_[0-9]*)\\]\\[([0-9.ed\-]*)\\]\\[\\\\\"([A-Za-z ]*)\\\\\"\\]"
      regexp-dervar "\\\\dervar\\[([A-Za-z0-9]*_[0-9]*)\\]\\[[\\\\\"]*([^]]*)[\\\\\"]*\\]\\[\\\\\"([A-Za-z ]*)\\\\\"\\]")

(defun unit-conversion (str)
  (cond
    ((string= str "gram") "g")
    ((string= str "kilogram per cubic metre") "kg/m^3")
    ((string= str "metre") "m")
    ((string= str "newton per metre") "N/m")
    ((string= str "metre per second") "m/s")
    ((string= str "meter per second") "m/s")
    ((string= str "newton") "N")
    (t str)
    ))

(defun collectvar (str)
  (loop 
     for (start stop begins ends) = (multiple-value-list (ppcre:scan regexp-var str))
     then (multiple-value-list (ppcre:scan regexp-var str :start stop))
     while start collect

     
       (loop for b across begins
	  and e across ends
	  collect (subseq str b e))))

(defun collectdervar (str)
  (loop 
     for (start stop begins ends) = (multiple-value-list (ppcre:scan regexp-dervar str))
     then (multiple-value-list (ppcre:scan regexp-dervar  str :start stop))
     while start collect

     
       (loop for b across begins
	  and e across ends
	  collect (subseq str b e))))




(defun bal-par-p (str)
  (loop for c across str
     with n = 0 
     do (if (eq c #\() (incf n)
	    (if (eq c #\)) (decf n)))
     finally (return (zerop  n))))

(defun cerca+- (str start)
  (let ((pos (position-if  (lambda (x) (member x '(#\+ #\-))) str :start start)))
    (when pos
      (if (and  (bal-par-p (subseq str 0 pos)) (bal-par-p (subseq str (1+ pos))))
	  (list (subseq str 0 pos) (subseq str pos (1+ pos)) (subseq str (1+ pos)))
	  (cerca+- str (1+ pos)))
      )))

(defun cerca/ (str start)
  (let ((pos (position-if  (lambda (x) (member x '(#\/))) str :start start)))
    (when pos
      (if (and  (bal-par-p (subseq str 0 pos)) (bal-par-p (subseq str (1+ pos))))
	  (list (subseq str 0 pos) (subseq str pos (1+ pos)) (subseq str (1+ pos)))
	  (cerca/* str (1+ pos)))
      )))

(defun cerca* (str start)
  (let ((pos (position-if  (lambda (x) (member x '(#\*))) str :start start)))
    (when pos
      (if (and  (bal-par-p (subseq str 0 pos)) (bal-par-p (subseq str (1+ pos))))
	  (list (subseq str 0 pos) (subseq str pos (1+ pos)) (subseq str (1+ pos)))
	  (cerca/* str (1+ pos)))
      )))
(defun cerca-operazione (str)
  (or (cerca+- str 0) (cerca/ str 0) (cerca* str 0))
  )

(defun convertexp (str)
  (cond
    ;; ((ppcre:scan  "(.*)\\((.*)\\)(.*)" str)
    ;;  (multiple-value-bind (r v) (ppcre:scan-to-strings "(.*)\\((.*)\\)(.*)"  str)
    ;;    (convertexp (format nil "~a {~a} ~a" (aref v 0) (aref v 1)(aref v 2)))))
    ((ppcre:scan  "^ *\\((.*)\\) *$" str)
     (multiple-value-bind (r v) (ppcre:scan-to-strings "^ *\\((.*)\\) *$"  str)
       (convertexp (format nil "~a" (aref v 0)))))
    
    ((cerca-operazione str)
     (let ((v (cerca-operazione str))) 
       (format nil "(~a ~a ~a)" (nth 1 v ) (convertexp (nth 0 v)) (convertexp (nth 2 v)))))
    ;; ((ppcre:scan  "{(.*)}" str)
    ;;  (multiple-value-bind (r v) (ppcre:scan-to-strings "{(.*)}"  str)
    ;;    (format nil "(~a)" (convertexp (aref v 0)))))
    ((ppcre:scan "u\\.([A-Za-z0-9]*_[0-9]*)\.value" str)
     (multiple-value-bind (r v) (ppcre:scan-to-strings "u\\.([A-Za-z0-9]*_[0-9]*)\.value" str)
       (format nil " ~a " (aref v 0))))
    ((ppcre:scan "u\\.g\.value" str)
     (format nil "#_1.0_gs" ))
    
    (t str)))


(defparameter repexp
  (list
   (list "\\(in-package #:pedb\\)" "")
   (list regexp-var "")
   (list regexp-dervar "")
   (list "\"([^\"]*)\"" "[\\1]")
   (list "\\$\\$([^$]*)\\$\\$" "] (formula () [\\1])")
   (list "\\$([^$]*)\\$" ",(imath [\\1])")
   (list "\\\\\\\\varp{([A-Za-z0-9]*_[0-9]*)}" ",(pq-format \\1)")
   (list "\\\\\\\\dervarp\\[([0-9])\\]{([A-Za-z0-9]*_[0-9]*)}" ",(pq-format \\2 :precision \\1)")))

(defun replacestr (str)
  (loop for (in out) in repexp
     with outstr = str do
       (setf outstr (ppcre:regex-replace-all in outstr out))
     finally (return outstr))
  )

;; ;;;;
(defun converti (file &optional (output nil))
  (let* (
	 (exe (merge-pathnames file *esercizi-directory*))
	 (str (read-file-into-string exe))
	 var
	 dervar
	 )
    (setf var (loop for (n v u) in (collectvar str)
		 collect (format nil "(~a #_~a_~a)" n v (unit-conversion u))))
    (setf dervar (loop for (n v u) in (collectdervar str)
		    collect (format nil "(~a ~a)" n (convertexp v) )))
    (format t "(let* (~{~a~%~}~{~a~%~})" var dervar)
    (format t "~a)" (replacestr str))
    (when output
      (with-open-file (stream exe :direction :output :if-exists :append)
	(format stream "(let* (~{~a~%~}~{~a~%~})" var dervar)
	(format stream "~a)" (replacestr str))))
    ))




