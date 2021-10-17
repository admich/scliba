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

(defmacro definizione-box ((title) &body body)
  `(list (framedtext (:context "corner=00, width=local, background=color,backgroundcolor=gray") (bf ,title) (newline ()) ,@body)))

(defmacro argomento ((&key title) &body body)
  `(list (framedtext (:context "corner=00, width=local") (framedtext (:context "width=local,align=flushright,frame=off,background=color,backgroundcolor=blue,foreground=color,foregroundcolor=white" ) (bf ,title)) (newline ()) ,@body) ))

;;;; Compiler

(defun compila-guarda-formulario (document &key (directory (merge-pathnames "formulari/file.lisp" *ptnh-directory*)) (backend (make-instance 'ptnh-context-backend)))
  (compila-guarda (merge-pathnames document directory) backend))

(defun compila-guarda-ptnh (document &key (directory  (merge-pathnames "file.lisp" *ptnh-directory*)) (backend (make-instance 'ptnh-context-backend)))
   (compila-guarda (merge-pathnames document directory) backend))

