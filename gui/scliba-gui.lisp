;;
(in-package #:scliba-gui)

(defparameter *pedb* (make-hash-table))

; (define-gesture-name :compile :pointer-button-press (:left :shift))

(defclass scliba-document ( )
  ()
  (:documentation "general scliba-document"))

(defclass scliba-file-document (scliba-document)
  ((file :accessor document-file
	 :initarg :file
	 :type 'pathname))
  (:documentation "scliba-document stored in a file"))

(defclass scliba-exercise-document (scliba-file-document)
  ((number :accessor exercise-number
	   :initarg :number
	   :type 'integer)
   (subject :accessor exercise-subject
	    :initarg :subject
	    :documentation "subject of the exercise")
   (type :initarg :type
	 :accessor exercise-type
	 :documentation "type of exercise"))
  (:documentation "scliba exercise"))

(defun make-exercise (filepath)
  (let*  ((exe (make-instance 'scliba-exercise-document :file filepath))
	  (filename-parts (ppcre:split "-" (pathname-name (document-file exe)))))
    (setf (exercise-number exe) (parse-integer (fourth filename-parts))
	  (exercise-subject exe) (second filename-parts)
	  (exercise-type exe) (third filename-parts))
    exe))

(defun pedb-preview-file (exe)
  (merge-pathnames  (pathname-name (document-file exe)) (merge-pathnames "prova.pdf" *esercizi-preview-directory*))) 

(defun push-exercise (exercise)
  (setf (gethash (document-file exercise) *pedb*) exercise))

(defun refresh-pedb ()
  (dolist (exe (uiop:directory-files *esercizi-directory* "*.lisp"))
    (push-exercise (make-exercise exe))))

(defclass tabular-view (view)
  ())

(defconstant +tabular-view+ (make-instance 'tabular-view))

(define-presentation-type scliba-exercise-document ())

(define-presentation-method present (obj (type scliba-exercise-document) stream view &key)
  (declare (ignore view))
  (format stream "~d ~a ~a ~a" (exercise-number obj) (pathname-name (document-file obj)) (exercise-type obj) (exercise-subject obj))
  )


(define-presentation-method present (obj (type scliba-exercise-document) stream (view tabular-view) &key)
  (declare (ignore view))
  (formatting-row ()
    (formatting-cell () (present (exercise-number obj)))
    (formatting-cell () (present (pathname-name (document-file obj))))
    (formatting-cell () (present (exercise-type obj)))
    (formatting-cell () (present (exercise-subject obj)))
    )
  )


(defclass file-view (view) 
  ((%file :initarg :file :reader file :initform nil)))

(defclass pdf-view (file-view) 
  ())

(defparameter *file-view* (make-instance 'file-view))
(defgeneric display-pane-with-view (frame pane view))


(define-application-frame scliba-gui ()
  ()
  (:menu-bar t)
  (:panes
   (list :application
         :display-function 'display-list
         :scroll-bars :both)
   (main :application
         :display-function 'display-main
         :scroll-bars :both
         :default-view *file-view*)
   (button :application
           :display-time :command-loop
           :display-function 'display-button
           :max-height 5)
   (inter :interactor
          :max-height 10))
  (:layouts (default
             (vertically ()
               button
               (horizontally ()
                 (1/4 list)
                 (3/4 main))
               inter))))

(defun scliba-gui (&key new-process)
  (clim-sys:make-process (lambda () (run-frame-top-level (make-application-frame 'scliba-gui)) :name "scliba-gui")))

(defun display-main (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

(defun cat (file &optional (stream *standard-output*))
  (with-open-file (input file )
    (loop for line = (read-line input nil nil)
          while line
          do 
          (write-string line stream)
          (terpri stream))))

(defmethod display-pane-with-view (frame pane (view file-view))
  (let ((file (file view)))
    (if file
      (cat file pane)
      (write-string "no file" pane))))

(defmethod display-pane-with-view (frame pane (view pdf-view))
  (let ((file (file view)))
    (if (uiop:file-exists-p file)
      (progn 
        (format pane "~a~%" file)
        (uiop:run-program (format nil "mutool draw -o /home/admich/tmp/tmp.png ~a 1" file))
        (let ((pattern (make-pattern-from-bitmap-file "/home/admich/tmp/tmp.png")))
	  (multiple-value-bind (x y) (stream-cursor-position pane)
	    (draw-pattern* pane pattern x y)
	    (stream-increment-cursor-position pane 0 (pattern-height pattern)))))
      (write-string "no pdf file" pane))))
  

(defun display-list (frame pane)
  (list-exercises frame pane))

(defun list-exercises (frame pane &key (directory *esercizi-directory*))
  (formatting-table (pane :x-spacing 50)
    (loop for exe being the hash-value of *pedb* do

	 (present exe 'scliba-exercise-document :stream pane :view +tabular-view+)))
  (format pane "~%"))

(defun display-button (frame pane)
  (let ((medium (sheet-medium pane)))
   (with-text-style (medium (make-text-style :sans-serif :bold :huge)) (format pane "sCLiba GUI"))))

(define-scliba-gui-command (com-refresh-pedb :name t :menu t) ()
  (refresh-pedb))

(define-scliba-gui-command (com-show-exercise :name t :menu t)
    ((exe 'scliba-exercise-document :prompt "Esercizio" :gesture :select))
  (setf  (stream-default-view (find-pane-named *application-frame* 'main)) (make-instance 'file-view :file (document-file exe))))



(define-scliba-gui-command (com-preview-exercise :name t :menu t)
    ((exe 'scliba-exercise-document :prompt "Esercizio"))  
  (setf  (stream-default-view (find-pane-named *application-frame* 'main)) (make-instance 'pdf-view :file (pedb-preview-file exe))))

(define-scliba-gui-command (com-edit-exercise :name t :menu t)
    ((exe 'scliba-exercise-document :prompt "Esercizio" :gesture :edit))
  (climacs:edit-file (document-file exe)))

;; (define-gesture-name :compile :pointer-button-press (:middle :control))
(define-gesture-name :compile :pointer-button-press (:left :shift))

(define-scliba-gui-command (com-compile-exercise :name t :menu t)
    ((exe 'scliba-exercise-document :prompt "Esercizio" :gesture :compile))
  (climacs:edit-file (document-file exe)))

(define-scliba-gui-command (com-new-compito :name t :menu t)
  ((file-name 'string :prompt "File name (es: 16-bio-i-q1c1)"))
  (let ((file (merge-pathnames file-name *compiti-directory*)))
    (pedb:new-compito file)
    (format (frame-query-io *application-frame*) "Creato il compito ~a" file)
    (climacs:edit-file file)))


(define-scliba-gui-command (com-filter-exercises :name t :menu t)
    ((str 'string :prompt "String"))
  (format (find-pane-named *application-frame* 'inter) "pol: ~a" str))

(define-scliba-gui-command (com-quit :name t :menu t :keystroke (#\q :control)) ()
  (frame-exit *application-frame*))

