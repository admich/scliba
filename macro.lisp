(in-package :scliba)

(defmacro with-document-arguments (args document &body body)
  (let ((in (gensym)))
    `(let ((,in ,document))
         ;; ,@(maybe-rebinding in documents 'with-slots)
         ;; ,in
         (symbol-macrolet
             ,(mapcar (lambda (arg-entry)
			(destructuring-bind
			      (var-name &optional (arg-name (alexandria:make-keyword (symbol-name var-name))))
                              (ensure-list arg-entry)
                            `(,var-name
                              (get-argument ,in ,arg-name)))
                        )
                      args)
           ,@body))))

;;; macro utility
(defmacro def-authoring-tree (name &optional (superclass '(authoring-tree)) &key (slot '()) (documentation "No documentation"))
  `(progn
     (defclass ,name (,@superclass)
       ,slot
       (:documentation ,documentation))

     (defmacro ,name (arguments &body body)
       (let ((cl '',name))
	 `(let ((*math* (if (typep (make-instance ,cl) 'mixin-math) t nil))
		(tree (make-instance ,cl :arguments (list ,@arguments))))
	    (let ((*current-node* tree))
	      (setf (authoring-tree-body tree) (flatten (list ,@body)))
	      tree))))))

(defmacro def-simple-authoring-tree (name &optional (superclass '(authoring-tree)) (documentation "No documentation"))
  `(progn
     (defclass ,name (,@superclass)
       ())
     (defmacro ,name (&body body)
       (let ((cl '',name))
     	 `(let ((*math* (if (typep (make-instance ,cl) 'mixin-math) t nil))
		(tree (make-instance ,cl)))
	    (let ((*current-node* tree))
	      (setf (authoring-tree-body tree) (flatten (list ,@body)))
	      tree))))))

(defmacro def-simple-authoring-tree-fn (name &optional (superclass '(authoring-tree)) (documentation "No documentation"))
  `(progn
     (defclass ,name (,@superclass)
       ())
     (defun ,name (&rest body)
       (let ((*math* (if (typep (make-instance ',name) 'mixin-math) t nil)))
     	 (make-instance ',name :body body)))))
