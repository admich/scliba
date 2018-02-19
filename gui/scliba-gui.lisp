;;
(in-package #:scliba-gui)

(defparameter *pedb* (make-hash-table))

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
  (merge-pathnames (make-pathname :type "pdf") (standard-output-file (document-file exe) *default-backend*)))


(defun push-exercise (exercise)
  (setf (gethash (document-file exercise) *pedb*) exercise))

(defun refresh-pedb ()
  (dolist (exe (uiop:directory-files *esercizi-directory* "*.lisp"))
    (push-exercise (make-exercise exe)))
  (update-list-exercises))


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
    (formatting-cell () (present (exercise-subject obj)))))


(defclass file-view (view) 
  ((%file :initarg :file :reader file :initform nil)))

(defclass pdf-view (file-view) 
  ())

(defparameter *file-view* (make-instance 'file-view))
(defgeneric display-pane-with-view (frame pane view))



;;;; command table
(define-command-table exercise-selector-commands)
(define-command (com-next-exercise :name "Next"
				   :command-table exercise-selector-commands
				   :keystroke (#\i :control)
				   :menu t
				   :name t
				   :provide-output-destination-keyword nil
				   )
    ()
  (format (find-pane-named (find-application-frame 'scliba-gui) 'inter) "PIPPO"))


(define-application-frame scliba-gui ()
  ((%filter-criteria :accessor filter-criteria
		     :initform nil)
   (%listed-exercises :accessor listed-exercises
		      :initform nil)
   (%current-exercise :accessor current-exercise
		      :initform nil))
  (:command-table (scliba-gui
		   :inherit-from (exercise-selector-commands)
		   :inherit-menu t))
  (:menu-bar t)
  (:panes
   (list :application
         :display-function 'display-list
	 :min-width 10
	 :width 400
	 :max-width 800
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
          :max-height 100))
  (:layouts (default
		(vertically ()
		  button
		  (horizontally ()
		    ;; (1/4 list)
		    list
		    (make-pane 'clim-extensions:box-adjuster-gadget)
		    main
		    ;; (3/4 main)
		    )
		  (make-pane 'clim-extensions:box-adjuster-gadget)
		  inter)
		)))

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
	(if (current-exercise *application-frame*)
	    (cat (document-file (current-exercise *application-frame*)) pane)
	    (write-string "no file" pane)))))

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
      (format pane "no pdf file: ~a" file))))
  

(defun update-list-exercises ()
  (setf (listed-exercises *application-frame*)
	(loop for exe being the hash-value of *pedb*
	   when (exe-match-criteria exe) collect exe
	     ))
  (setf (current-exercise *application-frame*) (first (listed-exercises *application-frame*) ))
  )

(defun display-list (frame pane)
  (list-exercises frame pane))

(defun list-exercises (frame pane &key (directory *esercizi-directory*))
  (let (current-presentation)
    (formatting-table (pane :x-spacing 50 :y-spacing 15)
      (loop for exe in (listed-exercises *application-frame*)
	 do
	   (if (equal exe (current-exercise *application-frame*)) 
	       (surrounding-output-with-border (pane :shape :rounded :background +red+) (setf current-presentation (present exe 'scliba-exercise-document :stream pane :view +tabular-view+)))
	       (present exe 'scliba-exercise-document :stream pane :view +tabular-view+)))
      ;; (loop for exe being the hash-value of *pedb* do
      ;; 	 (when (exe-match-criteria exe)
      ;; 	   (surrounding-output-with-border (pane :shape :rounded  :background +red+)
      ;; 	     (present exe 'scliba-exercise-document :stream pane :view +tabular-view+))
      ;; 	   ))
      )
    (format pane "~%")
    
    (when current-presentation (multiple-value-bind (x y) (output-record-position current-presentation)
       (scroll-extent pane 0 y)))
    ))

(defun exe-match-criteria (exe)
  (if (filter-criteria *application-frame*)
      (equal (filter-criteria *application-frame*) (exercise-subject exe)) t))

(defun display-button (frame pane)
  (let ((medium (sheet-medium pane)))
   (with-text-style (medium (make-text-style :sans-serif :bold :huge)) (format pane "sCLiba GUI"))))

(define-scliba-gui-command (com-refresh-pedb :name t :menu t) ()
  (refresh-pedb))

(define-scliba-gui-command (com-next-exercise :name t :menu t :keystroke (#\n :control))
    ()
  (let* ((exercises (listed-exercises *application-frame*))
	 (n-exe (position (current-exercise *application-frame*) exercises)))
    (setf (current-exercise *application-frame*) (nth (mod (1+ n-exe) (length exercises)) exercises))
    
    ))

(define-scliba-gui-command (com-previous-exercise :name t :menu t :keystroke (#\p :control))
    ()
  (let* ((exercises (listed-exercises *application-frame*))
	 (n-exe (position (current-exercise *application-frame*) exercises)))
    (setf (current-exercise *application-frame*) (nth (mod (1- n-exe) (length exercises)) exercises))
    
    ))


(define-scliba-gui-command (com-show-exercise :name t :menu t)
    ((exe 'scliba-exercise-document :prompt "Esercizio" :gesture :select))
  (setf (current-exercise *application-frame*) exe)
  ;; (setf  (stream-default-view (find-pane-named *application-frame* 'main)) (make-instance 'file-view :file (document-file exe)))
  )

(define-gesture-name :view :pointer-button-press (:middle :control))

(define-scliba-gui-command (com-preview-exercise :name t :menu t)
    ((exe 'scliba-exercise-document :prompt "Esercizio" :gesture :view))  
  (setf  (stream-default-view (find-pane-named *application-frame* 'main)) (make-instance 'pdf-view :file (pedb-preview-file exe))))

(define-scliba-gui-command (com-edit-exercise :name t :menu t)
    ((exe 'scliba-exercise-document :prompt "Esercizio" :gesture :edit))
  (climacs:edit-file (document-file exe)))

(define-gesture-name :compile :pointer-button-press (:left :control))

(define-scliba-gui-command (com-compile-exercise :name t :menu t)
    ((exe 'scliba-exercise-document :prompt "Esercizio" :gesture :compile))
  (pedb:compila-esercizio-preview (document-file exe))
  (format *query-io* "compilato esercizio: ~a" exe))

(define-scliba-gui-command (com-new-compito :name t :menu t)
  ((file-name 'string :prompt "File name (es: 16-bio-i-q1c1)"))
  (let ((file (merge-pathnames file-name *compiti-directory*)))
    (pedb:new-compito file)
    (format (frame-query-io *application-frame*) "Creato il compito ~a" file)
    (climacs:edit-file file)))

(define-presentation-type-abbreviation esercizi-argomenti ()
  `(completion ,(cons '("Tutti" . nil) *esercizi-argomenti*)))

(define-scliba-gui-command (com-filter-exercises :name t :menu t)
    ((arg 'esercizi-argomenti :prompt "Argomento:"))
  (setf (filter-criteria *application-frame*) (cdr arg))
  (format (find-pane-named *application-frame* 'inter) "Filtro: ~a" (car arg))
  (update-list-exercises))

(define-scliba-gui-command (com-quit :name t :menu t :keystroke (#\q :control)) ()
  (frame-exit *application-frame*))

