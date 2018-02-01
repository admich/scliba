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

(defmethod export-document :around ((document mixin-div-html) (backend html-backend))
  (html-output (:div :class (type-of document) (call-next-method))))

(defclass mixin-startstop-context ()
  ())

(defmethod export-document :around ((document mixin-startstop-context) (backend mixin-context-backend))
  (let ((namestr (string-downcase (format nil "~a" (type-of document)))))
    (format *outstream* "~&\\start~A~@[[~A]~]~%" namestr (getf (slot-value document 'arguments) :context))
    (call-next-method)
    (format *outstream* "~&\\stop~A~%" namestr)))

(def-authoring-tree authoring-document (authoring-tree) :documentation "Document root")

(defmethod export-document :around ((document authoring-document) backend)
  (reset-all-counters)
  (setf *main-backend* backend)
  (call-next-method)
  )

(defmethod export-document :before ((document authoring-document) (backend mixin-context-backend))
  (format *outstream* "~%
~@[\\setupbodyfont[~dpt]~]
~@[\\setupinterlinespace[~a]~]~%" (get-argument document :bodyfont) (get-argument document :interline)))

(defmethod export-document :around ((document authoring-document) (backend mixin-context-backend))
  (if *top-level-document*
      (progn 
	(format *outstream*
		"\\starttext~%")
	(call-next-method)
	(format *outstream*
		"~&\\stoptext~%"))
      (call-next-method)))

(defmethod export-document :around ((document authoring-document) (backend html-backend))
  (if *top-level-document*
      (who:with-html-output (*outstream* nil :prologue t  :indent t)
	(:html
	 (:head
	  (:meta :charset "UTF-8")
	  (:title (who:str (get-argument document :title))))
	 (:body (call-next-method))))
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; document part utility
;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (if *randomize* (random-elt seq) (first-elt seq)))

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

(defmethod export-document :before ((document enumerated) (backend context-backend))
  (format *outstream* "~&\\start~A~@[[~A]~]~%" (string-downcase (symbol-name (class-name (class-of document)))) (getf (slot-value document 'arguments) :context))
  )

(defmethod export-document :after ((document enumerated) (backend context-backend))
  (format *outstream* "~&\\stop~A~%" (string-downcase (symbol-name (class-name (class-of document))))))

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
(defmethod export-document :around ((document buffered) (backend autarchy-backend))
  (let ((*outstream* (cdr (assoc (type-of document) *buffers*))))
    (call-next-method)))


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
(defmethod export-document :before ((document par) (backend mixin-context-backend))
  (format *outstream* "~&\\par ~@[\\inouter{~a}~]" (and (getf (authoring-tree-arguments document) :tag)  (export-document-on-string (getf (authoring-tree-arguments document) :tag) backend))))

(def-authoring-tree framedtext (authoring-tree mixin-div-html mixin-startstop-context))

(defmethod initialize-instance :after ((class framedtext) &rest rest)
	   (when (getf (authoring-tree-arguments class) :middle)
	     (setf (getf (authoring-tree-arguments class) :context) "middle")))

(def-simple-authoring-tree nbsp)
(defmethod export-document ((document nbsp) (backend mixin-context-backend))
  (format *outstream*"\\nbsp "))

(def-authoring-tree newline)
(defmethod export-document ((document newline) (backend mixin-context-backend))
  (format *outstream*"~&\\crlf~%"))

(def-authoring-tree hline)
(defmethod export-document ((document hline) (backend mixin-context-backend))
  (format *outstream*"~&\\hairline~%"))

(def-authoring-tree hlinefill)
(defmethod export-document ((document hlinefill) (backend mixin-context-backend))
  (format *outstream*"~~\\hrulefill ~~"))
(defmethod export-document ((document hlinefill) (backend html-backend))
  (format *outstream*"_________________"))

;; footnote
(def-authoring-tree footnote)

(defmethod export-document :before ((document footnote) (backend mixin-context-backend))
  (format *outstream*"\\footnote{"))

(defmethod export-document :after ((document footnote) (backend mixin-context-backend))
  (format *outstream*"}"))


(defmacro title (string)
  `(centering (bf (tfd ,string))))

(def-authoring-tree footer (authoring-tree) :documentation "the footer of the page")

(defmethod export-document :before ((document footer) (backend aut-context-backend))
  (format *outstream* "\\setupfootertexts[~A][~A]" (get-argument document :left) (get-argument document :right)))

(def-simple-authoring-tree centering (authoring-tree) "Center the content")

(defmethod export-document :around ((document centering) (backend mixin-context-backend))
  (format *outstream* "~&\\midaligned{")
  (call-next-method)
  (format *outstream* "}"))


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

(defmacro def-font-size (name context)
  `(progn
     (def-simple-authoring-tree ,name)
     (defmethod export-document :around ((document ,name) (backend mixin-context-backend))
		(format *outstream* (concatenate 'string "{\\" ,context " "))
		(call-next-method)
		(format *outstream* "}"))))

(def-font-size tfxx "tfxx")
(def-font-size tfx "tfx")
(def-font-size tf "tf")
(def-font-size tfa "tfa")
(def-font-size tfb "tfb")
(def-font-size tfc "tfc")
(def-font-size tfd "tfd")
(def-font-size tfe "tfe")


;;;; aligned
(def-simple-authoring-tree align-right)

(defmethod export-document :before ((document align-right) (backend mixin-context-backend))
  (format *outstream* "{\\rightaligned "))

(defmethod export-document :after ((document align-right) (backend mixin-context-backend))
  (format *outstream* "}"))

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
	   (export-document
	    (funcall font-fn (nbsp ) (newline ())  (format nil "~% ~{~a.~} ~a~%" (reverse (cdr (section-n document))) (get-argument document :title))  (newline ()) (nbsp )) *main-backend*)))))



(defmethod export-document ((document section) (backend autarchy-backend))
  (funcall (nth (1- (length (section-n document))) *section-head-fn*) document)
  (call-next-method))

(defvar *section-level* 0)
(defparameter *section-context-labels* (list "part" "chapter" "section" "subsection" "subsubsection"))

(defmethod export-document :before ((document section) (backend context-backend))
  (incf *section-level*)
  (format *outstream*"~&\\start~A~@[[~A]~]~%" (elt *section-context-labels* *section-level*) (getf (slot-value document 'arguments) :context)))

(defmethod export-document :after ((document section) (backend context-backend))
  (format *outstream*"~&\\stop~A~%" (elt *section-context-labels* *section-level*) (getf (slot-value document 'arguments) :context))
  (decf *section-level*))


;;; intemize
(def-authoring-tree itemize)

(defmethod export-document :before ((document itemize) (backend mixin-context-backend))
  (format *outstream* "~&\\startitemize~@[[~A]~]~%" (getf (slot-value document 'arguments) :context)))

(defmethod export-document  :after ((document itemize) (backend mixin-context-backend))
  (format *outstream* "~&\\stopitemize~%"))

(def-authoring-tree item)
(defmethod export-document :before ((document item) (backend mixin-context-backend))
  (format *outstream*"~&\\item "))

(def-simple-authoring-tree bf)

(defmethod export-document :before ((document bf) (backend mixin-context-backend))
  (format *outstream*"{\\bf "))
(defmethod export-document :after ((document bf) (backend mixin-context-backend))
  (format *outstream*"}"))

(def-simple-authoring-tree it)
(defmethod export-document :before ((document it) (backend mixin-context-backend))
  (format *outstream*"{\\it "))
(defmethod export-document :after ((document it) (backend mixin-context-backend))
  (format *outstream*"}"))

(def-simple-authoring-tree roman)
(defmethod export-document :before ((document roman) (backend mixin-context-backend))
  (format *outstream*"{\\rm "))
(defmethod export-document :after ((document roman) (backend mixin-context-backend))
  (format *outstream*"}"))

(def-simple-authoring-tree sans-serif)
(defmethod export-document :before ((document sans-serif) (backend mixin-context-backend))
  (format *outstream*"{\\ss "))
(defmethod export-document :after ((document sans-serif) (backend mixin-context-backend))
  (format *outstream*"}"))

(def-simple-authoring-tree small-caps)
(defmethod export-document :before ((document small-caps) (backend mixin-context-backend))
  (format *outstream*"{\\sc "))
(defmethod export-document :after ((document small-caps) (backend mixin-context-backend))
  (format *outstream*"}"))

(def-simple-authoring-tree newpage)
(defmethod export-document ((document newpage) (backend mixin-context-backend))
  (format *outstream*"\\page "))

(defmacro emph (&rest body)
  `(it ,@body))

(def-authoring-tree columns (authoring-tree mixin-startstop-context))

(def-authoring-tree newcolumn)
(defmethod export-document ((document newcolumn) (backend mixin-context-backend))
  (format *outstream*"~&\\column~%"))


(defclass mixin-math ()
  ())
;;; maybe a counter for *math* in case of nested math environment
(defmethod export-document :around ((document mixin-math) backend)
  (let ((*math* t))
    (call-next-method)
    ))


(def-authoring-tree ref)
(defmethod export-document ((document ref) (backend context-backend))
  (format *outstream*"\\in[~a]" (get-argument document :ref)))

(def-authoring-tree figure)
(defmethod export-document :before ((document figure) (backend context-backend))
  (format *outstream*"\\placefigure[here][~a]{~A}{" (get-argument document :ref) (get-argument document :caption)))
(defmethod export-document :after ((document figure) (backend context-backend))
  (format *outstream*"}"))

(def-authoring-tree mpcode)

(defmethod export-document :before  ((document mpcode) (backend mixin-context-backend))
  (format *outstream* "~&\\startMPcode~%")
  )

(defmethod export-document :after  ((document mpcode) (backend mixin-context-backend))
  (format *outstream* "~&\\stopMPcode~%")
  )

;;;;TABLE
(def-authoring-tree table)

(defmethod export-document :before ((document table) (backend mixin-context-backend))
  (with-document-argument (frame stretch) document
    (format *outstream* "~& \\startxtable[frame=~:[off~;on~]~:[~;,option=stretch~]] ~%" frame stretch)))

(defmethod export-document :after ((document table) (backend mixin-context-backend))
  (format *outstream*"~&\\stopxtable~%"))

(defmethod export-document ((document table) (backend html-backend))
  (html-output
   (:table (call-next-method))))

(def-authoring-tree table-row)

(defmethod export-document :before ((document table-row) (backend mixin-context-backend))
  (format *outstream*"\\startxrow "))
(defmethod export-document :after ((document table-row) (backend mixin-context-backend))
  (format *outstream*"\\stopxrow~%"))

(defmethod export-document ((document table-row) (backend html-backend))
  (html-output
    (:tr (call-next-method))))

(def-authoring-tree table-cell)

(defmethod export-document :before ((document table-cell) (backend mixin-context-backend))
  
  (let ((nc (get-argument document :nc)))
    (format *outstream* "\\startxcell ")))

(defmethod export-document :after ((document table-cell) (backend mixin-context-backend))
  (format *outstream*"\\stopxcell "))


(defmethod export-document ((document table-cell) (backend html-backend))
  (html-output
    (:td (call-next-method))))

(def-simple-authoring-tree imath (authoring-tree mixin-math))

(defmethod export-document :before ((document imath) (backend mixin-context-backend))
  (format *outstream*"$"))
(defmethod export-document :after ((document imath) (backend mixin-context-backend))
  (format *outstream*"$"))

(def-authoring-tree phys-n)
(defmethod export-document  ((document phys-n) (backend context-backend))
  (scliba-f:n outstream (first (authoring-tree-body document)) nil nil))

(def-authoring-tree formula (authoring-tree mixin-math mixin-div-html mixin-startstop-context))

(defmethod export-document :before ((document formula) (backend context-backend))
  (format *outstream*"~@[~&\\placeformula[~a]~%~]" (getf (authoring-tree-arguments document) :ref)))

