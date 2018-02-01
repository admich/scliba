(defpackage :climacs-scliba-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base :drei-syntax :flexichain :drei :drei-fundamental-syntax :drei-lisp-syntax :drei-lr-syntax)
  (:shadowing-import-from :drei-lisp-syntax :form))

(in-package :climacs-scliba-syntax)

(define-syntax-command-table scliba-table
    :errorp nil
    :inherit-from '(lisp-table))

(define-syntax scliba-syntax (lisp-syntax)
  ()
  (:name "sCLiba")
  (:pathname-types "scl")
  (:command-table scliba-table)
  )

(defmethod name-for-info-pane ((syntax scliba-syntax) &key view)
  (format nil "sCLiba~@[:~(~A~)~]"
          (drei-lisp-syntax::provided-package-name-at-mark syntax (if (typep view 'point-mark-view)
                                                    (point view)
                                                    0))))

(defmethod display-syntax-name ((syntax scliba-syntax) (stream extended-output-stream) &key view)
  (princ "sCLiba:" stream)		; FIXME: should be `present'ed
                                        ; as something.
  (let ((package-name (drei-lisp-syntax::provided-package-name-at-mark syntax (if (typep view 'point-mark-view)
										  (point view)
										  0))))
    (if (find-package package-name)
        (with-output-as-presentation (stream (find-package package-name) 'expression)
          (princ package-name stream))
        (with-text-face (stream :italic)
          (princ package-name stream)))))
;;;;;;;;;;;;;;;; String
(in-package :drei-lisp-syntax)
;;; parse trees
;; (defclass string-form (form) ())
;; (defclass complete-string-form (string-form complete-form-mixin) ())
;; (defclass incomplete-string-form (string-form incomplete-form-mixin) ())

(define-parser-state |[ word* | (drei-lisp-syntax::lexer-string-state parser-state) ())
(define-parser-state |[ word* ] | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|[ word* | drei-lisp-syntax::word-lexeme) |[ word* |)
(define-new-lisp-state (|[ word* | delimiter-lexeme) |[ word* |)
(define-new-lisp-state (form-may-follow string-start-lexeme) |[ word* |)
(define-new-lisp-state (|[ word* | string-end-lexeme) |[ word* ] |)

;;; reduce according to the rule form -> " word* "
(define-lisp-action (|[ word* ] | t)
  (reduce-until-type complete-string-form string-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|[ word* | (eql nil))
  (reduce-until-type incomplete-string-form string-start-lexeme t))

;; ;;;;;;;;;;;; comm prove
;;;;;;;;;;;;;;;; Line comment

;;; parse trees
;; (defclass line-comment-form (comment) ())

(define-parser-state |% word* | (lexer-line-comment-state parser-state) ())
(define-parser-state |% word* NL | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow line-comment-start-lexeme) |% word* |)
(define-new-lisp-state (|% word* | word-lexeme) |% word* |)
(define-new-lisp-state (|% word* | delimiter-lexeme) |% word* |)
(define-new-lisp-state (|% word* | comment-end-lexeme) |% word* NL |)

;;; reduce according to the rule form -> ; word* NL
(define-lisp-action (|% word* NL | t)
  (reduce-until-type line-comment-form line-comment-start-lexeme))





  
;; ;;;;;; prove

(in-package :climacs-scliba-syntax)

(define-syntax-command-table prova-table
    :errorp nil
    )

(define-syntax prova-syntax (lr-syntax-mixin fundamental-syntax)
  ()
  (:name "Prova")
  (:pathname-types "prv")
  (:command-table prova-table)
  (:default-initargs :initial-state |initial-state |))

(defmethod name-for-info-pane ((syntax scliba-syntax) &key view)
  (format nil "PROVA"
          ))

(defmethod display-syntax-name ((syntax prova-syntax)
				(stream extended-output-stream) &key pane)
  (declare (ignore pane))
  (princ "PROVA2" stream))

(define-lexer-state lexer-line-comment-state ()
  ()
  (:documentation "A comment start with sharp"))

(defclass prova-nonterminal (nonterminal) ())

(defclass comment (prova-nonterminal) ())

(defmethod skip-inter ((syntax lisp-syntax) state scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop when (end-of-buffer-p scan)
       do (return nil)
       until (not (whitespacep syntax (object-after scan)))
       do (fo)
       finally (return t))))


(define-parser-state |initial-state | (form-may-follow) ())
