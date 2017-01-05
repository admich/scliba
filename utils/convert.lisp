(in-package #:scliba)
(ql:quickload "split-sequence")
(defvar *in-string* nil)
(defvar *in-item* nil)

(defun remove-comment (line)
  "remove comment"
  (let ((pos% (position #\% line))
	(pos%% (search "\\%" line)))
    (cond
      ((not pos%) line)
      ((not pos%%) (subseq line 0 (position #\% line)))
      ((= pos% (+ 1 pos%%)) (concatenate 'string (subseq line 0 (+ 1 pos%)) (remove-comment (subseq line (+ 1 pos%))))))))

(defun pre-process-line (line)
  "process line"
  (let ((line (substitute #\Space #\Tab (string-trim '(#\Space #\Linefeed #\Return #\Newline) (remove-comment line)))))
    (cond
      ((search "\\startcomponent" line) "")
      ((search "\\stopcomponent" line) "")
      ((search "\\project" line) "")
      ((search "\\product" line) "")
      ((search "\\usepath" line) "")
      ((search "\\doifmode{esercizio}{\\printsoluzioni}" line) "")      
      (t (format nil "~a" line)))))

(defun final-process-line (line)
  (ppcre:regex-replace-all "\"" (ppcre:regex-replace-all "[\\\\ ~]" line "\\&\\&") "\\\""))

(defun exit-from-string ()
  (when *in-string* (format t "\"") (setf *in-string* nil)))

(defun exit-from-item ()
  (when *in-item* (format t ")~%") (setf *in-item* nil)))


(defun analyse-word (word)
  (cond
					; ((string= "\\startesercizio" word) (progn (exit-from-string) (format t "~a~%""(esercizio ()")))
    ((string= "\\startesercizio" word) (progn (exit-from-string) (format t "~%")))
    ((string= "\\stopesercizio" word) (progn (exit-from-string) (format t ")~%")))
    ((string= "\\startformula" word) (progn (exit-from-string) (format t "~%~a~%""(formula ()")))
    ((string= "\\stopformula" word) (progn (exit-from-string) (format t ")~%")))
    ((string= "\\startMPcode" word) (progn (exit-from-string) (format t "~%~a~%""(mpcode ()")))
    ((string= "\\stopMPcode" word) (progn (exit-from-string) (format t ")~%")))
    
    ((string= "\\startsoluzione" word) (progn (exit-from-string) (format t "~a~%""(soluzione ()")))
    ((string= "\\stopsoluzione" word) (progn (exit-from-string) (format t ")~%")))
    ((string= "\\beginsoluzione" word) (progn (exit-from-string)))
    ((string= "\\endsoluzione" word) (progn (exit-from-string)))    
    ((string= "\\startchoices" word) (progn (exit-from-string) (format t "~%~a~%" "(scelte ()")))
    ((string= "\\stopchoices" word) (progn (exit-from-string) (exit-from-item) (format t ")~%")))
    ((string= "\\startparts" word) (progn (exit-from-string) (format t "~%~a~%" "(parti ()")))
    ((string= "\\stopparts" word) (progn (exit-from-string) (exit-from-item) (format t ")~%")))
    ((ppcre:scan "\\item\\[.*\\]" word) (progn (exit-from-string) (exit-from-item) (format t "~a " "(item (:sol)") (setf *in-item* t)))
    ((ppcre:scan "\\in\\[.*\\]" word) (progn (exit-from-string) (format t "~a " "(last-sol)")))
    ((string= "\\item" word) (progn (exit-from-string) (exit-from-item) (format t "~a " "(item ()") (setf *in-item* t)))
    ((ppcre:scan "\\var\\[.*\\]\\[.*\\]\\[.*\\]" word)
     (progn (format t "~:[\"~;~]~a%~% " *in-string* (final-process-line word))
	    (setf *in-string* t)))
    ((ppcre:scan "\\dervar\\[.*\\]\\[.*\\]\\[.*\\]" word)
     (progn (format t "~:[\"~;~]~a%~% " *in-string* (final-process-line word))
	 (setf *in-string* t)))
    (t (progn
	 (format t "~:[\"~;~]~a " *in-string* (final-process-line word))
	 (setf *in-string* t)))))

(defun analyse-line (line)
  (let ((words (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)))
    (loop with out = ""
       for w in words
       do ;; (setf out (concatenate 'string out (analyse-word w)))
       ;; finally (return out)
	 (analyse-word w)
	 )))

(defun process-line (line stream)
  (analyse-line (pre-process-line line)))

(defun convert-from-context (file)
  "convert the context file to lisp"
  (setf *in-item* nil *in-string* nil)
  (let ((f (merge-pathnames *esercizi-directory* (make-pathname :name file :type "tex"))))
    (with-open-file (stream f)
      (format t "~a~%""(esercizio ()")
      (loop for line = (read-line stream nil 'foo)
	 until (eq line 'foo)
	 do (process-line line stream)))))

(defun convert-from-context-tofile (file)
  (with-open-file (*standard-output* (merge-pathnames *esercizi-directory* (make-pathname :name file :type "lisp")) :direction :output :if-exists :supersede)
    (convert-from-context file)))

(defun convert-multiple-from-context-tofile (n1 n2)
  (loop for i from n1 to n2
     do (convert-from-context-tofile (pathname-name (esercizio-numero i)))))


(defun test-convert (&optional (n1 1) (n2 2))
  (compito (:title "Prova conversione" :soluzioni t)
    (loop for i from n1 to n2
       append
	 (list (input-esercizio (esercizio-numero i))
	       (format nil "~&\\rightaligned{\\color[middlegray]{~A}}~%" (pathname-name (esercizio-numero i)))))
    (soluzioni ())))
#|
(with-open-file (stream (merge-pathnames *compiti-directory* "testauto.tex") :direction :output :if-exists :supersede :if-does-not-exist :create)
  (export-document (test-convert 1 2) :context stream))

|#
