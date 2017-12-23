(defpackage scliba-test
  (:use #:cl #:scliba #:fiveam))

(in-package #:scliba-test)

(def-suite scliba-test :description "Test suite for sCLiba")

(in-suite scliba-test)

(test compito
  (let ((sclibafile (uiop:merge-pathnames* "examples/compito.lisp" (asdf:system-source-directory "scliba"))))
    (finishes (read-file sclibafile))
    (dolist (x (list 'context-backend 'aut-context-backend 'html-backend))
      (finishes (export-file sclibafile (make-instance x))))
    (finishes (view-html (standard-output-file sclibafile (make-instance 'html-backend))))
    (finishes (compila-context (standard-output-file sclibafile (make-instance 'context-backend))))
    (finishes (compila-context (standard-output-file sclibafile (make-instance 'aut-context-backend))))
    (finishes (view-pdf (standard-output-file sclibafile (make-instance 'context-backend))))
    (finishes (view-pdf (standard-output-file sclibafile (make-instance 'aut-context-backend))))))

(test all-exercise
  (let ((sclibafile (uiop:merge-pathnames* "examples/all-exercise.lisp" (asdf:system-source-directory "scliba"))))
    (finishes (read-file sclibafile))
    (dolist (x (list 'context-backend 'aut-context-backend 'html-backend))
      (finishes (export-file sclibafile (make-instance x))))
    (finishes (view-html (standard-output-file sclibafile (make-instance 'html-backend))))
    (finishes (compila-context (standard-output-file sclibafile (make-instance 'context-backend))))
    (finishes (compila-context (standard-output-file sclibafile (make-instance 'aut-context-backend))))
    (finishes (view-pdf (standard-output-file sclibafile (make-instance 'context-backend))))
    (finishes (view-pdf (standard-output-file sclibafile (make-instance 'aut-context-backend))))))
