;;;;; backend.lisp
(in-package #:scliba)

(defclass backend () ())
(defclass context-backend (backend) ())
(defclass autarchy-backend (backend) ())
(defclass aut-context-backend (autarchy-backend) ())

