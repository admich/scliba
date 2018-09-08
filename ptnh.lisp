(in-package #:ptnh)

(defparameter *ptnh-directory* #p"/home/admich/Documents/scuola/my-didattica/ptnh/")

(defclass ptnh-mixin-backend (backend)
  ())

(defclass ptnh-context-backend (ptnh-mixin-backend aut-context-backend)
  ())


(defmethod export-document :before ((document authoring-document) (backend ptnh-context-backend))
  (format *outstream*
	  "
\\mainlanguage[italian]
\\enablemode[lmmath]
\\setuppapersize[A4][A4]
\\setuplayout[
  topspace=1cm,
  backspace=2cm,
  rightmargin=3cm,
%  width=,
  height=middle,
  header=0pt]

\\setupbodyfont[10pt]
\\setuppagenumbering[alternative=doublesided]

\\useexternalfigure[cc][by-sa.png][height=3mm]
\\setupfootertexts[\\setups{footr}][\\pagenumber][\\pagenumber][\\setups{footl}]
\\startsetups[footr]
  \\externalfigure[cc]
  \\rlap{ A. De Michele}
\\stopsetups
\\startsetups[footl]
  \\llap{A. De Michele }
  \\externalfigure[cc]
\\stopsetups

"))



(defmethod export-document ((document section) (backend ptnh-mixin-backend))
  (let* ((newfonts (copy-list *section-fonts*))
	 (*section-fonts* (loop for i in newfonts collect
			       (cons 'sans-serif (cdr i)))))
    (if (eq 0 (section-level document))
	(export-document (list (inmargin (:margin :right) (tfd  (small-caps (format nil "~{~a~}" (reverse (cdr (section-n document)))))))
 			       (align-right (tfd (small-caps (format nil "~a~%" (get-argument document :title))
							     (inframed (:context "frame=off,rightframe=on,rulethickness=2pt,framecolor=red") ""))))) *main-backend*)
	(funcall (nth (- (length (section-n document)) 2) *section-head-fn*) document)
	)))


;; (framed (:context "frame=off,rightframe=on,rulethickness=2pt,framecolor=red") (align-right (tfd (small-caps (format nil "~% ~{~a.~} ~a~%" (reverse (cdr (section-n document))) (get-argument document :title)))) (newline ())))
 
 

(def-authoring-tree ptnh-document (authoring-document) :documentation "ptnh root document")

(defmethod export-document :before ((document ptnh-document) (backend mixin-context-backend))
  (format *outstream* "~&\\environment env_ptnh~%"))


(def-authoring-tree attenzione)

(defmethod export-document :before ((document attenzione) (backend mixin-context-backend))
  (format *outstream* "~&\\textrule[top]{Attenzione!}~%"))

(defmethod export-document :after ((document attenzione) (backend mixin-context-backend))
  (format *outstream* "~&\\textrule~%"))

;;;; Formulari
(def-authoring-tree formulario (authoring-document))
(defmethod export-document :before ((document formulario) (backend mixin-context-backend))
  (format *outstream* "~&\\setupheader[state=stop]~%"))
(defmethod export-document :before ((document formulario) backend)
  (with-document-arguments (title) document
    (export-document (title (format nil "Formulario: ~a" (export-document-on-string title backend))) backend)))

;; (def-authoring-tree argomento)

;; (defmethod export-document :before ((document argomento) backend)
;;   (with-document-arguments (title) document
;;     (export-document (list (newline ())
;; 			   (bf (format nil "~a" (export-document-on-string title backend))) (newline ())) backend)))

;; (defmethod export-document  ((document argomento) backend)
;;   (with-document-arguments (title) document
;;     (export-document (framedtext ()
;; 		       (list (newline ())
;; 			     (bf (format nil "~a" (export-document-on-string title backend))) (newline ()) (authoring-tree-body document))) backend)))

(defmacro argomento ((&key title) &body body)
  `(list (framedtext (:context "corner=00, width=local") (framedtext (:context "width=local,align=flushright,frame=off,background=color,backgroundcolor=blue,foreground=color,foregroundcolor=white" ) (bf ,title)) (newline ()) ,@body) ))
;;;; Compiler

(defun compila-guarda-formulario (document &key (directory (merge-pathnames "formulari/file.lisp" *ptnh-directory*)) (backend (make-instance 'ptnh-context-backend)))
  (compila-guarda (merge-pathnames document directory) backend)
  ;; (view-pdf (compila-context
  ;; 	     (export-file (merge-pathnames document directory) backend)))
  )

(defun compila-guarda-ptnh (document &key (directory  (merge-pathnames "file.lisp" *ptnh-directory*)) (backend (make-instance 'ptnh-context-backend)))
  ;; (view-pdf (compila-context
  ;; 	     (export-file (merge-pathnames document directory) backend)))

   (compila-guarda (merge-pathnames document directory) backend)
  )


;;;; Old to remove

(defun compila-ptnh-old (document &key (directory *ptnh-directory*) (backend-type 'context-backend))
  "genera il sorgente context dal sorgente lisp"
  (with-open-file (stream (merge-pathnames directory (make-pathname :name document :type "tex")) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let* ((backend (make-instance backend-type :stream stream))
	   (*outstream* (backend-outstream backend)))
       (format stream "\\environment env_ptnh~%")
      (export-document (read-file (merge-pathnames directory (make-pathname :name document :type "lisp"))) backend))))


(defun compila-guarda-ptnh-old (file &key (directory *ptnh-directory*) (backend-type 'context-backend))
  (compila-ptnh-old file)
  (let ((file (uiop:merge-pathnames* directory file))
	(file-pdf (uiop:merge-pathnames* directory (uiop:make-pathname* :name file :type "pdf"))))
    (compila-context file)
    (view-pdf file-pdf)))
