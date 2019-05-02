(in-package :scliba)

(defclass authoring-tree ()
  ((arguments :initarg :arguments
	      :initform nil
	      :accessor authoring-tree-arguments)
   (body :initarg :body
	 :initform nil
	 :accessor authoring-tree-body)
   (parent :initarg :parent
	   :initform *current-node*
	   :reader authoring-tree-parent))
  (:documentation "Main class for scliba documents"))


(defun get-argument (auth-tree arg)
  "get an argument from an authoring-tree"
  (getf (authoring-tree-arguments auth-tree) arg))

;;; export-document generic
(defgeneric export-document (document backend)
  (:documentation "generate the output"))

(defun export-document-on-string (document backend)
  "return a string"
  (let* ((s (make-string-output-stream))
	 (*outstream* s))
    (export-document document backend)
    (get-output-stream-string s)))


(defmethod export-document ((document t) backend)
  (format *outstream* ""))

(defmethod export-document ((document string) backend)
  (format *outstream* document))


(defmethod export-document ((document number) backend)
  (format *outstream* "~a" document))

(defmethod export-document ((document list) backend)
  (dolist (x document)
    (export-document x backend)))


(defmethod export-document :after ((document authoring-tree) backend) 
  (let ((*top-level-document* nil))
    (dolist (tree (slot-value document 'body))
      (export-document tree backend))))

(defclass mixin-div-html ()
  ())

(defclass mixin-startstop-context ()
  ())


(def-authoring-tree authoring-document (authoring-tree) :documentation "Document root")

(defmethod export-document :around ((document authoring-document) backend)
  (reset-all-counters)
  (setf *main-backend* backend)
  (call-next-method)
  )

;;;; TEX
(def-authoring-tree hbox)
(def-authoring-tree hss)
(def-authoring-tree hfil)
(def-authoring-tree hfill)
(def-authoring-tree vss)

;;;; input

(defun input (filename)
  (if *debug*
      (list (read-file filename) (format nil "~&\\rightaligned{\\color[middlegray]{~A}}~%" (pathname-name filename)))
      (read-file filename))) 

;; random
(defparameter *randomize* nil)
(defvar *i-random* 0)
(defun choose-one-of (seq)
  "If *randomize* is true choose at random from sequence otherwise return the first element in the list"
  (if *randomize* (alexandria:random-elt seq) (alexandria:first-elt seq)))

(defclass random-body (authoring-tree)
  ())

(defmethod export-document :before ((document random-body) backend)
	   (if (and *randomize* (not (get-argument document :no-random)))
	       (if (get-argument document :no-random-last)
		   (setf (slot-value document 'body) (append (shuffle (butlast (slot-value document 'body))) (last (slot-value document 'body))))
		   (setf (slot-value document 'body) (shuffle (slot-value document 'body))))))

(def-authoring-tree randomize (random-body))

;; counters
(defparameter *counters* nil)

(defmacro def-counter (name &optional (n 0))
  (let ((val (intern (concatenate 'string (symbol-name 'counter-) (symbol-name name) (symbol-name '-val))))
	(inc (intern (concatenate 'string (symbol-name 'counter-) (symbol-name name) (symbol-name '-inc))))
	(set (intern (concatenate 'string (symbol-name 'counter-) (symbol-name name) (symbol-name '-set)))))
    `(let ((counter ,n))
       (defun ,inc (&optional (delta 1))
	 (incf counter delta))
       (defun ,val ()
	 counter)
       (defun ,set (&optional (n 0))
	 (setf counter n))
       (pushnew (cons ',name (list ',inc ',val ',set)) *counters* :key #'car))))

(defmacro def-leveled-counter (name &optional (n '(list 0)))
  (let ((val (intern (concatenate 'string (symbol-name 'counter-) (symbol-name name) (symbol-name '-val))))
	(inc (intern (concatenate 'string (symbol-name 'counter-) (symbol-name name) (symbol-name '-inc))))
	(set (intern (concatenate 'string (symbol-name 'counter-) (symbol-name name) (symbol-name '-set)))))
    `(let ((counter ,n))
       (defun ,inc (&optional (delta 1))
	 (incf (car counter) delta)
	 (push 0 counter))
       (defun ,val ()
	 counter)
       (defun ,set (&optional (n (list 0)))
	 (if (eq n :pop) (pop counter)
	     (setf counter n)))
       (pushnew (cons ',name (list ',inc ',val ',set)) *counters* :key #'car))))

(defun reset-all-counters ()
  (dolist (x *counters*)
    (funcall (nth 3 x))))

;;;; enumerated

(def-authoring-tree enumerated (authoring-tree mixin-div-html)
  :slot ((n :initarg :n
	    :initform 0
	    :accessor enumerated-n)))


(defmacro def-enumerated (name &optional (superclass '(authoring-tree)) &key (slot '()) (documentation "Enumerated tree"))
  "Define an enumerated tree. fmt-str is the format string for the title. "
  `(progn
     (def-authoring-tree ,name (enumerated ,@superclass) :slot ,slot :documentation ,documentation)
     (def-counter ,name)
     (defmacro ,name (arguments &body body)
       (let ((inc (symbolicate "COUNTER-" ',name "-INC"))
	     (val (symbolicate "COUNTER-" ',name "-VAL"))
	     (cl (symbolicate ',name)))
	 `(progn (,inc)
		 
		 (make-instance ',cl :arguments (list ,@arguments)
				:body (list ,@body)
				:n (,val)))))))


(defmacro def-enumerated-slave (name master)
  `(progn
     (def-authoring-tree ,name (enumerated))
     (defmacro ,name (arguments &body body)
       (let (
	     (val (symbolicate "COUNTER-" ',master "-VAL"))
	     (cl (symbolicate ',name)))
	 `(let* ((tree (make-instance ',cl :arguments (list ,@arguments)
				      :body (list ,@body)
				      :n (,val)))
		 (*current-node* tree))
	    (setf (authoring-tree-body tree) (flatten (list ,@body)))
	    tree)))))


;; buffer
(defparameter *buffers* nil)
(def-authoring-tree buffered)

(defmacro def-buffer (name)
  `(pushnew (cons ',name  (make-string-output-stream)) *buffers* :key 'car))

(defmacro def-buffered (name)
  `(progn
     (def-buffer ,name)
     (def-authoring-tree ,name (buffered))))

(defmacro def-enumerated-slave-buffered (name master)
  `(progn
     (def-buffer ,name)
     (def-authoring-tree ,name (enumerated buffered))
     (defmacro ,name (arguments &body body)
       (let (
	     (val (symbolicate "COUNTER-" ',master "-VAL"))
	     (cl (symbolicate ',name)))
	 `(progn 
	    
	    (make-instance ',cl :arguments (list ,@arguments)
			   :body  (list  ,@body)
			   :n (,val)))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; document part core

(def-authoring-tree book)

(def-authoring-tree par)

(def-authoring-tree framedtext (authoring-tree mixin-div-html mixin-startstop-context))


(defmethod initialize-instance :after ((class framedtext) &rest rest)
	   (when (getf (authoring-tree-arguments class) :middle)
	     (setf (getf (authoring-tree-arguments class) :context) "middle")))

(def-authoring-tree framed (authoring-tree mixin-div-html))
(def-authoring-tree inframed (authoring-tree mixin-div-html))

(def-simple-authoring-tree nbsp)

(def-authoring-tree newline)

(def-authoring-tree hline)

(def-authoring-tree hlinefill)

;; footnote
(def-authoring-tree footnote)


(defmacro title (string)
  `(centering (bf (tfd ,string))))

(def-authoring-tree footer (authoring-tree) :documentation "the footer of the page")


(def-simple-authoring-tree centering (authoring-tree) "Center the content")

;;; font
(def-simple-authoring-tree-fn bf)

(def-simple-authoring-tree-fn it)

(def-simple-authoring-tree-fn underbar)

(def-simple-authoring-tree-fn roman)

(def-simple-authoring-tree-fn sans-serif)

(def-simple-authoring-tree-fn small-caps)



(defmacro emph (&rest body)
  `(it ,@body))


;;;;  font size
#|
;;;; LATEX 
size 	standard classes (except slides), beamer 	AMS classes, memoir 	slides
        [10pt] 	[11pt] 	[12pt] 	[10pt] 	[11pt] 	[12pt]
\tiny 	5 	6 	6 	6 	7 	8 	13.82
\scriptsize 	7 	8 	8 	7 	8 	9 	16.59
\footnotesize 	8 	9 	10 	8 	9 	10 	16.59
\small 	9 	10 	10.95 	9 	10 	10.95 	16.59
\normalsize 	10 	10.95 	12 	10 	10.95 	12 	19.907
\large 	12 	12 	14.4 	10.95 	12 	14.4 	23.89
\Large 	14.4 	14.4 	17.28 	12 	14.4 	17.28 	28.66
\LARGE 	17.28 	17.28 	20.74 	14.4 	17.28 	20.74 	34.4
\huge 	20.74 	20.74 	24.88 	17.28 	20.74 	24.88 	41.28
\Huge 	24.88 	24.88 	24.88 	20.74 	24.88 	24.88 	41.28

;;;; context
size 	factor 	4pt 	5pt 	6pt 	7pt 	8pt 	9pt 	10pt 	11pt 	12pt 	14.4pt 	17.3pt 	20.7pt
xx 	0.6 	4 	5 	5 	5 	5 	5 	6 	7 	8 	10 	12 	14.4
x 	0.8 	4 	5 	5 	6 	6 	7 	8 	9 	10 	12 	14.4 	17.3
tf 	1.0 	4 	5 	6 	7 	8 	9 	10 	11 	12 	14.4 	17.3 	20.7
a 	1.2 	4.8 	6.0 	7.2 	8.4 	9.6 	10.8 	12.0 	13.2 	14.4 	17.3 	20.7 	24.8
b 	1.44 	5.8 	7.2 	8.6 	10.1 	11.5 	13.0 	14.4 	15.8 	17.3 	20.7 	24.9 	29.8
c 	1.728 	6.9 	8.6 	10.4 	12.1 	13.8 	15.6 	17.3 	19.0 	20.7 	24.9 	29.9 	35.8
d 	2.074 	8.3 	10.4 	12.4 	14.5 	16.6 	18.7 	20.7 	22.8 	24.9 	29.9 	35.9 	42.9
e 	2.488 	10.0 	12.4 	14.9 	17.4 	19.9 	22.4 	24.9 	27.4 	29.9 	35.8 	43.0 	52.5
_
scriptscript 	0.5 	4 	5 	5 	5 	5 	5 	5 	6 	7 	9 	10 	12
script 	0.7 	4 	5 	5 	6 	6 	7 	7 	8 	9 	11 	12 	14.4
small 	0.8 	4 	4 	5 	5 	6 	7 	8 	9 	10 	12 	14.4 	17.3
big 	1.2 	6 	7 	8 	9 	10 	11 	12 	12 	14.4 	17.3 	20.7 	20.7 

|#



(def-simple-authoring-tree-fn tfxx)
(def-simple-authoring-tree-fn tfx)
(def-simple-authoring-tree-fn tf)
(def-simple-authoring-tree-fn tfa)
(def-simple-authoring-tree-fn tfb)
(def-simple-authoring-tree-fn tfc)
(def-simple-authoring-tree-fn tfd)
(def-simple-authoring-tree-fn tfe)


;;;; aligned
(def-simple-authoring-tree align-right)


;;;; sections
(def-leveled-counter section)

(defclass section (authoring-document)
  ((n :initarg :n
      :initform '(1)
      :accessor section-n)))

(defun section-level (section)
  (- (length (section-n section)) 2))


(defmacro section (arguments &body body)
  `(progn
     (counter-section-inc)
     (let* ((sec (make-instance 'section
				:arguments (list ,@arguments))))
       (let ((*current-node* sec))
	 (setf (authoring-tree-body sec) (list ,@body))
	 (counter-section-set :pop)
	 sec))))

(defmethod initialize-instance :after ((class section) &rest rest)
  (setf (section-n class) (copy-list (counter-section-val)))
  (setf (getf (authoring-tree-arguments class) :context) (format nil "title=~A" (getf (authoring-tree-arguments class) :title))))


(defparameter *section-fonts* '((roman tfd bf) (roman tfc bf) (roman tfb bf) (roman tfa bf) (roman tf bf)))


(defparameter *section-head-fn*
  (loop for n upto 4
     collect
       (lambda (document)
	 (let* ((font-list (nth (- (length (section-n document)) 2) *section-fonts*))
		(font-fn (compose (first font-list) (second font-list) (third font-list))))
	   (export-document ;; (bf "PPPP")
	     (funcall font-fn (nbsp ) (newline ())  (format nil "~% ~{~a.~} ~a~%" (reverse (cdr (section-n document))) (get-argument document :title))  (newline ()) (nbsp ))
			    *main-backend*)))))



(defvar *section-level* 0)

(defparameter *section-context-labels* (list "part" "chapter" "section" "subsection" "subsubsection"))

;;; intemize
(def-authoring-tree itemize)

(def-authoring-tree item)

(defmacro simple-itemize (arguments &body body)
  
  `(itemize (,@arguments)
     ,@(loop for i in body collect (item () i))
     ))

;;;; 

(def-simple-authoring-tree newpage)

(def-authoring-tree columns (authoring-tree mixin-startstop-context))

(def-authoring-tree newcolumn)

(defclass mixin-math ()
  ())
;;; maybe a counter for *math* in case of nested math environment
(defmethod export-document :around ((document mixin-math) backend)
  (let ((*math* t))
    (call-next-method)
    ))

(def-authoring-tree ref)

(def-authoring-tree table-float)

(def-authoring-tree figure)

(def-authoring-tree mpcode)


;;;; space
(def-authoring-tree vspace)

;;;; inmargin
(def-authoring-tree inmargin)
;;;;TABLE
(def-authoring-tree table)

(def-authoring-tree table-row)

(def-authoring-tree table-cell)

(defmacro simple-table-rows (args list)
  `(loop for row in ,list
      collect (table-row ()
		(loop for cell in row collect
		     (table-cell () cell)))))

;; (defmacro simple-table-by-rows (args list)
;;   (table () 
;;     `(loop for row in ,list
;; 	collect (table-row ()
;; 		  (loop for cell in row collect
;; 		       (table-cell () cell))))))

;;;; MATH
(def-simple-authoring-tree imath (authoring-tree mixin-math))

(def-authoring-tree phys-n)

(def-authoring-tree formula (authoring-tree mixin-math mixin-div-html mixin-startstop-context))
