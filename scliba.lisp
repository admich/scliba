;;;; scliba.lisp
;;;; exp-backend
(in-package #:scliba)

(defvar *math* nil)
(defparameter *debug* nil)
(defparameter *outstream* *standard-output*)
(defparameter *top-level-document* nil)

(defparameter *command-pdf-viewer* "emacsclient -n")
(defparameter *command-html-viewer* "firefox ") 
;;; HIGH security issue
;; (defun read-file (file)
;;   "Read the file and generate the clos structure of the document.
;; ATTENTION: don't read untrusted file. You read the file with common lisp reader."
;;   (let (ret)
;;     (scribble:enable-scribble-syntax)
;;     (scribble:enable-scribble-at-syntax)    
;;     (setf ret (with-open-file (ifile file)
;; 		(eval (read ifile))))
;;     (scribble:disable-scribble-syntax)
;;     (scribble:disable-scribble-at-syntax)
;;     ret))

					;(scribble:enable-scribble-syntax)
					;(scribble:enable-scribble-at-syntax)    

;; :merge error n conflict :fuze continue 


(named-readtables:defreadtable :scribble-antik
  (:fuze :antik :scribble-both))


(setf *read-default-float-format* 'double-float)
; (named-readtables:in-readtable :scribble-antik) ;scribble-both
;; (defun read-file (file)
;;   "Read the file and generate the clos structure of the document.
;; ATTENTION: don't read untrusted file. You read the file with common lisp reader."
;;   (named-readtables:in-readtable :scribble-antik) ;scribble-both
;;   (with-open-file (ifile file)
;;     (eval (read ifile))
;;     ;; (let ((*readtable* (named-readtables:find-readtable :antik))
;;     ;; 	  (*read-default-float-format* 'double-float))
;;     ;;   (eval (read ifile)))
;;     ))

(defun read-file (file)
  "Read the file and generate the clos structure of the document.
ATTENTION: don't read untrusted file. You read the file with common lisp reader."
  (named-readtables:in-readtable :scribble-antik) ;scribble-both
  (with-open-file (ifile file)

    (let ((*default-pathname-defaults* (uiop:truename* file))
	  (old-package *package*))
      (loop for form = (read ifile nil :eof)
	 with value
	 until (eq form :eof)
	 do (setf value (eval form))
	 finally
	   (setf *package* old-package)
	   (return value)))))

(defclass authoring-tree ()
  ((arguments :initarg :arguments
	      :initform nil
	      :accessor authoring-tree-arguments)
   (body :initarg :body
	 :initform nil
	 :reader authoring-tree-body))
  (:documentation "Main parent class for scliba documents"))


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

(defmethod export-document ((document list) backend)
  (dolist (x document)
    (export-document x backend)))

(defmethod export-document ((document authoring-tree) backend) 
  (let ((*top-level-document* nil))
    (dolist (tree (slot-value document 'body))
      (export-document tree backend))))


;;; macro utility
(defmacro def-authoring-tree (name &optional (superclass '(authoring-tree)) &key (slot '()) (documentation "No documentation") )
  `(progn
     (defclass ,name (,@superclass)
       ,slot
       (:documentation ,documentation))
     (defmacro ,name (arguments &body body)
       (let ((cl '',name))
	 `(let ((*math* (if (typep (make-instance ,cl) 'math) t nil)))
	    (make-instance  ,cl :arguments (list ,@arguments) :body (flatten (list  ,@body))))))))

(defmacro def-simple-authoring-tree (name &optional (superclass '(authoring-tree)) (documentation "No documentation"))
  `(progn
     (defclass ,name (,@superclass)
       ())
     (defmacro ,name (&body body)
       (let ((cl '',name))
	 `(let ((*math* (if (typep (make-instance ,cl) 'math) t nil)))
	    (make-instance  ,cl  :body (flatten (list  ,@body))))))))


(defclass startstop (authoring-tree)
  ())

;; (defmethod export-document ((document startstop) (backend context-backend))
;;   (let ((clstr (string-downcase (symbol-name (class-name (class-of document))))))
;;     (format *outstream*"~&\\start~A~@[[~A]~]~%" clstr (getf (slot-value document 'arguments) :context))
;;     (dolist (tree (slot-value document 'body))
;;       (export-document tree backend outstream))
;;     (format *outstream*"~&\\stop~A~%" clstr)))

(defmacro def-startstop% (name &key superclass context-name)
  (let ((namestr (or context-name (string-downcase (symbol-name name)))))
    `(progn
       (def-authoring-tree ,name (startstop ,@superclass))

       (defmethod export-document :around ((document ,name) (backend mixin-context-backend))
		  (format *outstream* "~&\\start~A~@[[~A]~]~%" ,namestr (getf (slot-value document 'arguments) :context))
		  ;; (dolist (tree (slot-value document 'body))
		  ;;   (export-document tree backend))
		  (call-next-method)
		  (format *outstream* "~&\\stop~A~%" ,namestr))
       (defmethod export-document ((document ,name) (backend html-backend))
	 (html-output (:div :class ,namestr (call-next-method)))))))

(defmacro def-startstop (name &optional superclass)
  (let ((namestr (string-downcase (symbol-name name))))
    `(def-startstop% ,name :superclass ,superclass)))


(def-authoring-tree authoring-document (authoring-tree) :documentation "Document root")
(defmethod export-document :around ((document authoring-document) backend)
  (reset-all-counters)
  (call-next-method)
  )

(defmethod export-document :around ((document authoring-document) (backend mixin-context-backend))
  (format *outstream*"~%
~@[\\setupbodyfont[~dpt]~]
~@[\\setupinterlinespace[~a]~]
\\starttext~%" (get-argument document :bodyfont) (get-argument document :interline))
  (call-next-method)
  (format *outstream*"~&\\stoptext~%"))






;;;;;;;;;;;;;;;;;;;;;;;;;
;;; document part utility
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; input
(defclass input-tex (authoring-tree)
  ((file :initarg :file)))

(defmethod export-document ((document input-tex) (backend context-backend))
  (format *outstream*"\\component ~A~%" (pathname-name (slot-value document 'file))))

(defun input (filename)
  (if (string= "tex" (pathname-type filename))
      (make-instance 'input-tex :file filename)
      (if *debug*
	  (list (read-file filename) (format nil "~&\\rightaligned{\\color[middlegray]{~A}}~%" (pathname-name filename)))
	  (read-file filename)))) ;; read-file

;; random
(defparameter *randomize* nil)

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
;; (defmethod export-document ((document randomize) backend outstream)
;;   (let ((bodylist (if *randomize* (shuffle (copy-list (slot-value document 'body))) (copy-list (slot-value document 'body)))))
;;       (dolist (tree bodylist)
;; 	(export-document tree backend outstream))))


;; counters
(defparameter *counters* nil)

(defmacro def-counter (name &optional (n 0))
  (let ((val (intern (concatenate 'string (symbol-name '#:counter-) (symbol-name name) (symbol-name '#:-val))))
	(inc (intern (concatenate 'string (symbol-name '#:counter-) (symbol-name name) (symbol-name '#:-inc))))
	(set (intern (concatenate 'string (symbol-name '#:counter-) (symbol-name name) (symbol-name '#:-set)))))
    `(let ((counter ,n))
       (defun ,inc (&optional (n 1))
         (incf counter n))
       (defun ,val ()
	 counter)
       (defun ,set (&optional (n 0))
         (setf counter n))
       (pushnew (cons ',name (list ',inc ',val ',set)) *counters* :key #'car)
       )))

(defun reset-all-counters ()
  (dolist (x *counters*)
    (funcall (nth 3 x))))

(def-authoring-tree enumerated (startstop)
  :slot ((n :initarg :n
	    :initform 0
	    :accessor enumerated-n)))

(defmethod export-document :around ((document enumerated) (backend context-backend))
  (format *outstream* "~&\\start~A~@[[~A]~]~%" (string-downcase (symbol-name (class-name (class-of document)))) (getf (slot-value document 'arguments) :context))
  (call-next-method)
  (format *outstream* "~&\\stop~A~%" (string-downcase (symbol-name (class-name (class-of document))))))

(defmacro def-enumerated (name)
  "Define an enumerated tree. fmt-str is the format string for the title. "
  `(progn
     (def-authoring-tree ,name (enumerated))
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
	 `(progn 
		 
		 (make-instance ',cl :arguments (list ,@arguments)
				:body (list  ,@body)))))))


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
			   :body  (list  ,@body)))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; document part core

(def-authoring-tree book)

(def-authoring-tree par)
(defmethod export-document :before ((document par) (backend context-backend))
  (format *outstream*"~&\\par ~@[\\inouter{~a}~]" (export-document-on-string (getf (authoring-tree-arguments document) :tag) backend)))

(def-startstop framedtext)
(defmethod initialize-instance :after ((class framedtext) &rest rest)
	   (when (getf (authoring-tree-arguments class) :middle)
	     (setf (getf (authoring-tree-arguments class) :context) "middle")))


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

(defmethod export-document :before ((document footnote) (backend context-backend))
  (format *outstream*"\\footnote{"))

(defmethod export-document :after ((document footnote) (backend context-backend))
  (format *outstream*"}"))


;; (def-authoring-tree title)
(defmacro title (string)
  `(centering (bf (big ,string))))

(def-authoring-tree footer (authoring-tree) :documentation "the footer of the page")

(defmethod export-document :before ((document footer) (backend aut-context-backend))
  (format *outstream* "\\setupfootertexts[~A][~A]" (get-argument document :left) (get-argument document :right))
  )

(def-simple-authoring-tree centering (authoring-tree) "Center the content")
(defmethod export-document :around ((document centering) (backend mixin-context-backend))
  (format *outstream* "~&\\midaligned{")
  (call-next-method)
  (format *outstream* "}"))
(def-simple-authoring-tree big)
(defmethod export-document :around ((document big) (backend mixin-context-backend))
  (format *outstream* "{\\tfb ")
  (call-next-method)
  (format *outstream* "}"))
(def-authoring-tree section)
(defvar *section-level* 0)
(defparameter *section-context-labels* (list "part" "chapter" "section" "subsection" "subsubsection"))

(defmethod export-document :before ((document section) (backend context-backend))
  (incf *section-level*)
  (format *outstream*"~&\\start~A~@[[~A]~]~%" (elt *section-context-labels* *section-level*) (getf (slot-value document 'arguments) :context)))

(defmethod export-document :after ((document section) (backend context-backend))
  (format *outstream*"~&\\stop~A~%" (elt *section-context-labels* *section-level*) (getf (slot-value document 'arguments) :context))
  (decf *section-level*))


(defmethod initialize-instance :after ((class section) &rest rest)
  (setf (getf (authoring-tree-arguments class) :context) (format nil "title=~A" (getf (authoring-tree-arguments class) :title))))

(def-startstop itemize)
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


(def-simple-authoring-tree newpage)
(defmethod export-document ((document newpage) (backend mixin-context-backend))
  (format *outstream*"\\page "))

(defmacro emph (&rest body)
  `(it ,@body))

(def-startstop columns)

(def-authoring-tree newcolumn)
(defmethod export-document ((document newcolumn) (backend mixin-context-backend))
  (format *outstream*"~&\\column~%"))


(def-authoring-tree math)
;;; maybe a counter for *math* in case of nested math environment
(defmethod export-document :before ((document math) backend)
  (setf *math* t))
(defmethod export-document :after ((document math) backend)
  (setf *math* nil))

(def-authoring-tree ref)
(defmethod export-document ((document ref) (backend context-backend))
  (format *outstream*"\\in[~a]" (get-argument document :ref)))

(def-authoring-tree figure)
(defmethod export-document :before ((document figure) (backend context-backend))
  (format *outstream*"\\placefigure[here][~a]{~A}{" (get-argument document :ref) (get-argument document :caption)))
(defmethod export-document :after ((document figure) (backend context-backend))
  (format *outstream*"}"))
(def-startstop% mpcode :context-name "MPcode")


;;;;TABLE
(def-authoring-tree table)
(defmethod export-document :before ((document table) (backend mixin-context-backend))
  (let (
	(widths (get-argument document :widths))
	(frame (get-argument document :frame))
	)
    (unless frame (format *outstream*"~&\\setupTABLE[frame=off]~%"))
    (when widths
      (dotimes (n (length widths))
	(format *outstream*"\\setupTABLE[c][~d][width=~a\\textwidth]~%" (+ n 1) (nth n widths))))
    (format *outstream*"\\bTABLE~%")))
(defmethod export-document :after ((document table) (backend mixin-context-backend))
  (format *outstream*"~&\\eTABLE~%"))

(defmethod export-document ((document table) (backend html-backend))
  (html-output
   (:table (call-next-method))))

(def-authoring-tree table-row)
(defmethod export-document :before ((document table-row) (backend mixin-context-backend))
  (format *outstream*"\\bTR "))
(defmethod export-document :after ((document table-row) (backend mixin-context-backend))
  (format *outstream*"\\eTR "))

(defmethod export-document ((document table-row) (backend html-backend))
  (html-output
    (:tr (call-next-method))))

(def-authoring-tree table-cell)
(defmethod export-document :before ((document table-cell) (backend mixin-context-backend))
  (format *outstream*"\\bTD "))
(defmethod export-document :after ((document table-cell) (backend mixin-context-backend))
  (format *outstream*"\\eTD "))


(defmethod export-document ((document table-cell) (backend html-backend))
  (html-output
    (:td (call-next-method))))
;;;;;;;;;;;;;;;;;;
;;; CAOS make-instance e una funzione *math* gli argomenti sono valutati prima di cambiare *math* nei metodi
;;;;;;;;;;;;;;;;;
;; (defmethod initialize-instance :before ((class math) &rest initargs)
;;   (print "set m")
;;   (setf *math* t))

;; (defmethod initialize-instance :after ((class math) &rest initargs)
;;   (print "uset m")
;;   (setf *math* nil))


;; (defmethod export-document :before ((document formula) backend outstream)
;;   (format t "before formula ~A" *math*))
;; (defmethod export-document :after ((document formula) backend outstream)
;;   (format t "after formula ~A" *math*))

;; (defmethod export-document :around ((document formula) backend outstream)
;;   (format t "around formula ~A~%" *math*)
;;   (call-next-method))

;; (defmethod export-document :around ((document math) backend outstream)
;;   (let ((*math* t))
;;     (call-next-method)))

;; (defmethod initialize-instance :around ((document math) &rest initargs)
;;   (let ((*math* t))
;;     (call-next-method)))
(def-simple-authoring-tree imath (math))

(defmethod export-document :before ((document imath) (backend context-backend))
  (format *outstream*"$"))
(defmethod export-document :after ((document imath) (backend context-backend))
  (format *outstream*"$"))

(def-authoring-tree phys-n)
(defmethod export-document  ((document phys-n) (backend context-backend))
  (scliba-f:n outstream (first (authoring-tree-body document)) nil nil))

(def-startstop formula (math))
(defmethod export-document :before ((document formula) (backend context-backend))
  (format *outstream*"~@[~&\\placeformula[~a]~%~]" (getf (authoring-tree-arguments document) :ref)))


;;;;;;;;;;;;;;;;;;;;
;;; compile utility os interaction
;;;;;;;;;;;;;;;;;
(defun standard-output-file (file backend)
  (typecase backend
    (context-backend (merge-pathnames (merge-pathnames (pathname-name file) "context/prova.tex") file))
    (aut-context-backend (merge-pathnames (merge-pathnames (pathname-name file) "aut-context/prova.tex") file))
    (html-backend (merge-pathnames (merge-pathnames (pathname-name file) "html/prova.html") file) )))


(defun export-file (file backend)
  (let ((outfile (standard-output-file file backend))
	(*top-level-document* t)
	)
    (uiop:ensure-all-directories-exist (list outfile))
    (with-open-file (stream outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (let ((*outstream* stream))
	(export-document (read-file file) backend)
	))))


(defun compila-context (file &key (mode nil) (output nil))
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
    (let ((command (format nil "context --purgeall ~@[--mode=~a~] ~@[--result=~a~] ~a" mode output file )))
      (uiop:run-program command :output t))))

(defun view-pdf (file)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
    (let* ((file (if (string= "pdf" (pathname-type file))
		     file
		     (merge-pathnames (make-pathname :type "pdf") file)))
	   (command (format nil "~a ~a &" *command-pdf-viewer*  file)))
      (uiop:run-program  command   :output t))
    ))

(defun view-html (file)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
    (let ((command (format nil "~a ~a &" *command-html-viewer*  file)))
      (uiop:run-program  command   :output t))
    ))




#|
(compito (:title "Verifica di fisica" :scuola nil :rfoot "feb 2016 Q2C1 (w07)")
  (infoform)
  (columns (:n 2 :balance :no)
    (input (:file "asada")) 
    (esercizi  ("q-cin2-ch-00166" "q-cin2-ch-00087" "q-cin2-ch-00167" "q-din-ch-00168" "q-din-ch-00169" "q-din-ch-00132" "q-din-ch-00135" "q-din-ch-00122" "q-din-ch-00170")))
  )

(with-open-file (stream (merge-pathnames *compiti-directory* "compito.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
  (export-document (read-file "examples/compito.cla") :context stream))

(with-open-file (stream (merge-pathnames *compiti-directory* "all-exercise.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
  (let ((*section-level* 1)) (export-document (raccolta-esercizi) :context stream)))

|#
