(defpackage :climacs-scliba-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base :drei-syntax :flexichain :drei :drei-fundamental-syntax :drei-lisp-syntax :drei-lr-syntax)
  (:shadowing-import-from :drei-lisp-syntax :form)
  (:export #:scliba-syntax))

(in-package :climacs-scliba-syntax)

(define-syntax-command-table scliba-table
    :errorp nil
    :inherit-from '(lisp-table))

(define-syntax scliba-syntax (lisp-syntax)
  ()
  (:name "sCLiba")
  (:pathname-types "scl")
  (:command-table scliba-table))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEXER

(define-lexer-state lexer-scliba-state (lexer-toplevel-state)
  ()
  (:documentation "In this state, the lexer assumes it can skip
    whitespace and should recognize ordinary lexemes of the language"))

(defclass scribble-start-lexeme (drei-lisp-syntax::lisp-lexeme) ())
(defclass scribble-stop-lexeme (drei-lisp-syntax::lisp-lexeme) ())
(defclass scribble-unmatched-stop-lexeme (drei-lisp-syntax::lisp-lexeme) ())

(defmethod lex ((syntax scliba-syntax) (state drei-lisp-syntax::lexer-toplevel-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\[ (fo) (make-instance 'scribble-start-lexeme))
	(#\] (fo) (make-instance 'scribble-unmatched-stop-lexeme))
	(t (call-next-method))
	))))

(defmethod lex ((syntax scliba-syntax) (state lexer-scliba-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\]) (fo) (make-instance 'scribble-stop-lexeme))
	    ((eql object #\\)
	     (fo)
	     (unless (end-of-buffer-p scan)
	       (fo))
	     (make-instance 'drei-lisp-syntax::delimiter-lexeme))
	    ((constituentp object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (constituentp (object-after scan))))
		   do (fo))
	     (make-instance 'drei-lisp-syntax::word-lexeme))
	    (t (fo) (make-instance
                     (if (characterp object)
                         'drei-lisp-syntax::delimiter-lexeme
                         'drei-lisp-syntax::literal-object-delimiter-lexeme)))))))
;; parse (action?  new-state parser-state

(defclass scribble-form (drei-lisp-syntax::form-lexeme) ())
(defclass complete-scribble-form (scribble-form drei-lisp-syntax::complete-form-mixin) ())
(defclass incomplete-scribble-form (scribble-form drei-lisp-syntax::incomplete-form-mixin) ())

(define-parser-state |[ word* | (lexer-scliba-state parser-state) ())
(define-parser-state |[ word* ] | (lexer-toplevel-state parser-state) ())

(defmacro define-new-scliba-state ((state parser-symbol) &body body)
  `(defmethod new-state ((syntax scliba-syntax) (state ,state) (tree ,parser-symbol))
     ,@body))

(define-new-scliba-state (|[ word* | drei-lisp-syntax::word-lexeme) |[ word* |)
(define-new-scliba-state (|[ word* | drei-lisp-syntax::delimiter-lexeme) |[ word* |)
(define-new-scliba-state (drei-lisp-syntax::form-may-follow scribble-start-lexeme) |[ word* |)
(define-new-scliba-state (|[ word* | scribble-stop-lexeme) |[ word* ] |)


(defmacro define-scliba-action ((state lexeme) &body body)
  `(defmethod action ((syntax scliba-syntax) (state ,state) (lexeme ,lexeme))
     ,@body))

;;; reduce according to the rule form -> " word* "
(define-scliba-action (|[ word* ] | t)
  (reduce-until-type complete-scribble-form scribble-start-lexeme))

;;; reduce at the end of the buffer
(define-scliba-action (|[ word* | (eql nil))
  (reduce-until-type incomplete-scribble-form scribble-start-lexeme t))

;; ;highlight

(define-syntax-highlighting-rules scliba-style-highlighting
  (drei-lisp-syntax::error-lexeme (*error-drawing-options*))
  (drei-lisp-syntax::string-form (*string-drawing-options*))
  (scribble-form (*string-drawing-options*))
  (drei-lisp-syntax::comment (*comment-drawing-options*))
  (drei-lisp-syntax::literal-object-form (:options :function (object-drawer)))
  (drei-lisp-syntax::complete-token-form (:function #'(lambda (view form)
                                      (cond ((drei-lisp-syntax::symbol-form-is-keyword-p (syntax view) form)
                                             *keyword-drawing-options*)
                                            ((drei-lisp-syntax::symbol-form-is-macrobound-p (syntax view) form)
                                             *special-operator-drawing-options*)
                                            ((drei-lisp-syntax::symbol-form-is-boundp (syntax view) form)
                                             *special-variable-drawing-options*)
                                            (t +default-drawing-options+)))))
  (drei-lisp-syntax::parenthesis-lexeme (:function #'drei-lisp-syntax::parenthesis-highlighter))
  (drei-lisp-syntax::reader-conditional-positive-form
   (:function (drei-lisp-syntax::reader-conditional-rule-fn t *comment-drawing-options*)))
  (drei-lisp-syntax::reader-conditional-negative-form
   (:function (drei-lisp-syntax::reader-conditional-rule-fn nil *comment-drawing-options*))))


(defmethod syntax-highlighting-rules ((syntax scliba-syntax))
  'scliba-style-highlighting)

;; (setq *syntax-highlighting-rules* 'my-style-highlighting
;;   )

(defmethod form-to-object ((syntax scliba-syntax) (form scribble-form)
                           &key &allow-other-keys)
  (values (read-from-string "PIPPO"))
  ;; (values (read-from-string (concatenate 'string (form-string syntax form) "\"")))
  )

;; command
(setf scliba::*command-pdf-viewer* "zathura")
 
(define-command (com-compila-guarda-file :name t :command-table scliba-table) ()
  (let ((file (filepath (buffer (current-view)))))
    (scliba:compila-guarda file scliba:*default-backend*)))

(esa:set-key 'com-compila-guarda-file
             'scliba-table
             '((#\c :control) (#\c :control)))
