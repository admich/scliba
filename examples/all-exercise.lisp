(in-package #:pedb)
(setf *randomize* nil)

(compito (:title "Eserciziario di fisica" :soluzioni t :rfoot (format nil "Compilato \\date\\ \\currenttime"))
      "
\\enablemode[soluzioni]
% \\setupbodyfont[11pt]
\\setupenumerations[esercizio][margin=yes, way=bysection, prefix=yes,prefixsegments=section,width=fit]
\\setupenumerations[soluzione][margin=yes, way=bysection, prefix=yes,prefixsegments=section,width=fit]"
      (loop for topic in *esercizi-argomenti*
	 collect 
	   (section (:title (car topic))
	     (append
	      (loop for ese in (esercizi-di (cdr topic))
		 collect 
		   (list (input-esercizio ese)
			 (format nil "~&\\rightaligned{\\color[middlegray]{~A}}~%" (pathname-name ese))
			 ))
	      (list "
	     \\doifmode{soluzioni}{\\subject{Soluzioni}
	     \\selectblocks[soluzione][criterium=section]}"))
	     
	     )))
