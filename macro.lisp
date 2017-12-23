(in-package :scliba)


(defmacro with-document-argument (args document &body body)
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




#|
(let ((doc (itemize (:a 1 :b 2))))
  (with-document-argument ((a :a) (b :b)) doc
    (format t "a: ~a b: ~a" a b)))

(let ((doc (itemize (:a 2 :b 3))))
  (with-document-argument (a b) doc
    (format t "a: ~a b: ~a" a b)))

|#


#|

(defmacro with-slots (slots instance &body body)
    (let ((in (gensym)))
      `(let ((,in ,instance))
         (declare (ignorable ,in))
         ,@(maybe-rebinding in instance 'with-slots)
         ,in
         (symbol-macrolet
             ,(mapcar (lambda (slot-entry)
                        (with-current-source-form (slot-entry slots)
                          (unless (typep slot-entry
                                         '(or symbol
                                           (cons symbol (cons symbol null))))
                            (error "Malformed slot entry: ~s, should be ~
                                  either a symbol or (variable-name ~
                                  slot-name)"
                                   slot-entry))
                          (destructuring-bind
                                (var-name &optional (slot-name var-name))
                              (ensure-list slot-entry)
                            `(,var-name
                              (slot-value ,in ',slot-name)))))
                      slots)
           ,@body))))




(defmacro with-slots (slot-entries instance-form &body body)
  (let* ((temp (gensym))
         (accessors
          (do ((scan slot-entries (cdr scan))
               (res))
              ((null scan) (nreverse res))
            (if (symbolp (first scan))
                (push `(,(first scan) (slot-value ,temp ',(first scan))) res)
                (push `(,(caar scan)
                         (slot-value ,temp ',(cadar scan))) res)))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))
|#
