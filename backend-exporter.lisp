(in-package :scliba)
(defun output-context-with-option (command context-option)
  (format nil "~a~@[[~a]~]~%" command context-option))

(defmethod export-document :around ((document mixin-div-html) (backend html-backend))
  (html-output (:div :class (type-of document) (call-next-method))))

(defmethod export-document :around ((document mixin-startstop-context) (backend mixin-context-backend))
  (let ((namestr (string-downcase (format nil "~a" (type-of document)))))
    (format *outstream* "~&\\start~A~@[[~A]~]~%" namestr (getf (slot-value document 'arguments) :context))
    (call-next-method)
    (format *outstream* "~&\\stop~A~%" namestr)))

(defmethod export-document :before ((document authoring-document) (backend mixin-context-backend))
  (format *outstream* "~%
~&~@[\\setupbodyfont[~a]~]
~&~@[\\setupbodyfont[~dpt]~]
~@[\\setupinterlinespace[~a]~]~%" (get-argument document :fontfamily) (get-argument document :bodyfont) (get-argument document :interline)))

(defmethod export-document :around ((document authoring-document) (backend mixin-context-backend))
  (if *top-level-document*
      (progn 
	    (format *outstream*
		        "\\starttext~%\\setupcolors[state=start]~%")
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

(defmacro generate-export-document-tex (name)
  
  (let ((str (format nil "\\~a\\ " (string-downcase (symbol-name name)))))
      `(defmethod export-document ((document ,name) (backend mixin-context-backend))
	 (format *outstream* ,str))))

(defmethod export-document :before ((document hbox) (backend aut-context-backend))
  (format *outstream* "\\hbox{ "))

(defmethod export-document :after ((document hbox) (backend aut-context-backend))
  (format *outstream* "} "))

(defmethod export-document :before ((document hss) (backend aut-context-backend))
  (format *outstream* "\\hss "))
(defmethod export-document :before ((document vss) (backend aut-context-backend))
  (format *outstream* "\\vss "))

(generate-export-document-tex hfil)
(generate-export-document-tex hfill)

(defmethod export-document :before ((document framed) (backend aut-context-backend))
  (with-document-arguments (context) document
    (format *outstream* "\\framed~@[[~a]~]{ " context)))

(defmethod export-document :after ((document framed) (backend aut-context-backend))
  (format *outstream* "} "))

(defmethod export-document :before ((document inframed) (backend aut-context-backend))
  (with-document-arguments (context) document
    (format *outstream* "\\inframed~@[[~a]~]{ " context)))

(defmethod export-document :after ((document inframed) (backend aut-context-backend))
  (format *outstream* "} "))

(defmethod export-document :before ((document inmargin) (backend aut-context-backend))
  (with-document-arguments (margin) document
    (let ((context-com
	   (case margin
	     (:right "inright")
	     (:left "inleft")
	     (otherwise "inmargin"))))
      
      (format *outstream* "\\~a{ " context-com))))

(defmethod export-document :after ((document inmargin) (backend aut-context-backend))
  (format *outstream* "} "))



(defmethod export-document :before ((document enumerated) (backend context-backend))
  (format *outstream* "~&\\start~A~@[[~A]~]~%" (string-downcase (symbol-name (class-name (class-of document)))) (getf (slot-value document 'arguments) :context))
  )

(defmethod export-document :after ((document enumerated) (backend context-backend))
  (format *outstream* "~&\\stop~A~%" (string-downcase (symbol-name (class-name (class-of document))))))

(defmethod export-document :around ((document buffered) (backend autarchy-backend))
  (let ((*outstream* (cdr (assoc (type-of document) *buffers*))))
    (call-next-method)))

(defmethod export-document :before ((document par) (backend mixin-context-backend))
  (format *outstream* "~&\\par ~@[\\inouter{~a}~]" (and (getf (authoring-tree-arguments document) :tag)  (export-document-on-string (getf (authoring-tree-arguments document) :tag) backend))))

(defmethod export-document ((document nbsp) (backend mixin-context-backend))
  (format *outstream*"\\nbsp "))

(defmethod export-document ((document newline) (backend mixin-context-backend))
  (format *outstream*"~&\\crlf~%"))

(defmethod export-document ((document hline) (backend mixin-context-backend))
  (format *outstream*"~&\\hairline~%"))

(defmethod export-document ((document hlinefill) (backend mixin-context-backend))
  (format *outstream*"~~\\hrulefill ~~"))

(defmethod export-document ((document hlinefill) (backend html-backend))
  (format *outstream*"_________________"))

(defmethod export-document :before ((document footnote) (backend mixin-context-backend))
  (format *outstream*"\\footnote{"))

(defmethod export-document :after ((document footnote) (backend mixin-context-backend))
  (format *outstream*"}"))

(defmethod export-document :before ((document footer) (backend aut-context-backend))
  (format *outstream* "\\setupfootertexts[~A][~A]" (get-argument document :left) (get-argument document :right)))

(defmethod export-document :around ((document centering) (backend mixin-context-backend))
  (format *outstream* "~&\\midaligned{")
  (call-next-method)
  (format *outstream* "}"))

(defmethod export-document :before ((document align-right) (backend mixin-context-backend))
  (format *outstream* "{\\rightaligned "))

(defmethod export-document :after ((document align-right) (backend mixin-context-backend))
  (format *outstream* "}"))

(defmacro def-font-size (name context)
  `(progn
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

(defmethod export-document ((document section) (backend autarchy-backend))
  (funcall (nth (1- (length (section-n document))) *section-head-fn*) document)
  (call-next-method))

(defmethod export-document :before ((document section) (backend context-backend))
  (incf *section-level*)
  (format *outstream*"~&\\start~A~@[[~A]~]~%" (elt *section-context-labels* *section-level*) (getf (slot-value document 'arguments) :context)))

(defmethod export-document :after ((document section) (backend context-backend))
  (format *outstream*"~&\\stop~A~%" (elt *section-context-labels* *section-level*) (getf (slot-value document 'arguments) :context))
  (decf *section-level*))

(defmethod export-document :before ((document itemize) (backend mixin-context-backend))
  (format *outstream* "~&\\startitemize~@[[~A]~]~%" (getf (slot-value document 'arguments) :context)))

(defmethod export-document  :after ((document itemize) (backend mixin-context-backend))
  (format *outstream* "~&\\stopitemize~%"))

(defmethod export-document :before ((document item) (backend mixin-context-backend))
  (format *outstream*"~&\\item "))

(defmethod export-document :before ((document bf) (backend mixin-context-backend))
  (format *outstream*"{\\bf "))

(defmethod export-document :after ((document bf) (backend mixin-context-backend))
  (format *outstream*"}"))

(defmethod export-document :before ((document underbar) (backend mixin-context-backend))
  (format *outstream*"{\\underbar "))

(defmethod export-document :after ((document underbar) (backend mixin-context-backend))
  (format *outstream*"}"))


(defmethod export-document :before ((document it) (backend mixin-context-backend))
  (format *outstream*"{\\it "))
(defmethod export-document :after ((document it) (backend mixin-context-backend))
  (format *outstream*"}"))

(defmethod export-document :before ((document roman) (backend mixin-context-backend))
  (format *outstream*"{\\rm "))
(defmethod export-document :after ((document roman) (backend mixin-context-backend))
  (format *outstream*"}"))

(defmethod export-document :before ((document sans-serif) (backend mixin-context-backend))
  (format *outstream*"{\\ss "))
(defmethod export-document :after ((document sans-serif) (backend mixin-context-backend))
  (format *outstream*"}"))

(defmethod export-document :before ((document small-caps) (backend mixin-context-backend))
  (format *outstream*"{\\sc "))
(defmethod export-document :after ((document small-caps) (backend mixin-context-backend))
  (format *outstream*"}"))

(defmethod export-document ((document newpage) (backend mixin-context-backend))
  (format *outstream*"\\page "))

(defmethod export-document ((document newcolumn) (backend mixin-context-backend))
  (format *outstream*"~&\\column~%"))

(defmethod export-document ((document ref) (backend mixin-context-backend))
  (format *outstream*"\\in[~a]" (get-argument document :ref)))


(defmethod export-document :before ((document table-float) (backend mixin-context-backend))
  (format *outstream*"~&\\placetable[here][~a]{~A}{" (get-argument document :ref) (get-argument document :caption)))

(defmethod export-document :after ((document table-float) (backend mixin-context-backend))
  (format *outstream*"}~%"))


(defmethod export-document :before ((document figure) (backend mixin-context-backend))
  (format *outstream*"~&\\placefigure[~@[~a~]][~a]{~A}{" (or (get-argument document :place) "here") (get-argument document :ref) (get-argument document :caption)))

(defmethod export-document :after ((document figure) (backend mixin-context-backend))
  (format *outstream*"}~%"))

(defmethod export-document :before  ((document mpcode) (backend mixin-context-backend))
  (format *outstream* "~&\\startMPcode~%")
  )

(defmethod export-document :after  ((document mpcode) (backend mixin-context-backend))
  (format *outstream* "~&\\stopMPcode~%")
  )

;;;; TABLE
(defun context-option (&rest args)
  (let ((firstp t))
    (with-output-to-string (str)
      (format str "[")
      (loop for opt in args
	 unless (or (null opt) (string= opt "")) do
	      (unless firstp (format str ","))
	      (format str "~a" opt)
	      (when firstp (setf firstp nil)))
      (format str "]"))))

(defun context-frame (frame-list)
  (let ((firstp t)
	(labels '("left" "top" "right" "bottom")))
    (with-output-to-string (str)
      (loop for i in frame-list
	    and label in labels
	     do
	      (unless firstp (format str ","))
	      (format str "~aframe=~:[off~;on~]" label i)
	      (when firstp (setf firstp nil))))))


(defun context-align (lisp-align)
  (let ((possibilities '((:l . "flushleft") (:c . "middle") (:r . "flushright") (:lohi . "lohi"))))
    (and lisp-align (format nil "align=~a" (alexandria:assoc-value  possibilities lisp-align)))))

(defun setup-context-table-align (align)
  (loop for x in align
     for i from 1 upto 100
     with context-align = '((:l . "flushleft") (:c . "middle") (:r . "flushright"))
     collect (format nil "\\setupTABLE[column][~d][align=~a]~%" i (assoc-value  context-align x))))

(defun context-multiple-align (align)
  (let ((context-align '((:l . "flushleft") (:c . "middle") (:r . "flushright"))))
    (format nil "~@[align={~{~a~^,~}}~]" (map 'list (lambda (x) (alexandria:assoc-value context-align x)) align))))


(defmethod export-document :before ((document table) (backend mixin-context-natural-table-backend))
  (with-document-arguments (frame stretch align caption split) document
    (when caption (format *outstream* "~%~%{\\bf ~a}\\blank~%" caption))
    (format *outstream* "~& \\bTABLE~a~%" (context-option (format nil "frame=~:[off~;on~]" frame ) (format nil "~:[~;option=stretch~]" stretch) (format nil "~:[~;split=yes~]" split)))
    (when align
      (loop for str in (setup-context-table-align align) do
	(format *outstream* str)))))

(defmethod export-document :after ((document table) (backend mixin-context-natural-table-backend))
  (format *outstream*"~&\\eTABLE~%~%~%"))

(defmethod export-document :before ((document table) (backend mixin-context-xtable-backend))
  (with-document-arguments (frame stretch  caption split align) document
    (when caption (format *outstream* "~%~%{\\bf ~a}\\blank~%" caption))
    (format *outstream* "~& \\startxtable~a~%" (context-option (format nil "frame=~:[off~;on~]" frame) (format nil "~@[~a~]" (context-multiple-align align))  (format nil "~:[~;option=stretch~]" stretch) (format nil "~:[~;split=yes~]" split)))))

(defmethod export-document :after ((document table) (backend mixin-context-xtable-backend))
  (format *outstream*"~&\\stopxtable~%~%~%"))

(defmethod export-document ((document table) (backend html-backend))
  (html-output
    (:table (call-next-method))))

(defmethod export-document :before ((document table-row) (backend mixin-context-natural-table-backend))
  (with-document-arguments (context) document
    (format *outstream*   (output-context-with-option "\\bTR" context))))

(defmethod export-document :after ((document table-row) (backend mixin-context-natural-table-backend))
  (format *outstream*"\\eTR~%"))

(defmethod export-document :before ((document table-row) (backend mixin-context-xtable-backend))
  (with-document-arguments (context) document
    (format *outstream*   (output-context-with-option "\\startxrow" context))))

(defmethod export-document :after ((document table-row) (backend mixin-context-xtable-backend))
  (format *outstream*"\\stopxrow~%"))


(defmethod export-document ((document table-row) (backend html-backend))
  (html-output
    (:tr (call-next-method))))

(defmethod export-document :before ((document table-cell) (backend mixin-context-natural-table-backend))
  (with-document-arguments (nc frame) document
    (format *outstream* "\\bTD~a " (context-option (format nil "~@[nc=~d~]" nc) (context-frame frame)))))

(defmethod export-document :after ((document table-cell) (backend mixin-context-natural-table-backend))
  (format *outstream*"\\eTD "))

(defmethod export-document :before ((document table-cell) (backend mixin-context-xtable-backend))
  (with-document-arguments (nc frame align) document
    (format *outstream* "\\startxcell~a " (context-option (format nil "~@[nx=~d~]" nc) (context-frame frame) (context-align align)))))

(defmethod export-document :after ((document table-cell) (backend mixin-context-xtable-backend))
  (format *outstream*"\\stopxcell "))


(defmethod export-document ((document table-cell) (backend html-backend))
  (html-output
    (:td (call-next-method))))

;;;; SPACE
(defmethod export-document :before ((document vspace) (backend mixin-context-backend))
  (format *outstream* "~%\\blank ~%~%"))
;;;; MATH
(defmethod export-document :before ((document imath) (backend mixin-context-backend))
  (format *outstream*"$"))
(defmethod export-document :after ((document imath) (backend mixin-context-backend))
  (format *outstream*"$"))

(defmethod export-document :before ((document formula) (backend context-backend))
  (format *outstream*"~@[~&\\placeformula[~a]~%~]" (getf (authoring-tree-arguments document) :ref)))

