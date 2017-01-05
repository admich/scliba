A system in common lisp to write book and other document
* Philosophy
 - write once for different output (pdf, ConTeXt, html, texinfo, epub,
   text, org)
 - programmability
 - easy and fast use of the already written exercise in context
 - possibility to control the specific low level output for different
 output format
 - different input format
 
* Implementation ideas
 1) CLOS based: read the file -> build a clos class that is the
   document -> use generic function for export
 2) Use function or macro directly
 3) Low level parser
   

* Similar projects
 - [[http://www.nongnu.org/skribilo/][Skribilo]] written in guile scheme
 - [[http://docs.racket-lang.org/scribble/index.html][Scribble: The Racket Documentation Tool]] racket documentation tool
 - [[http://www-sop.inria.fr/mimosa/fp/Skribe/][Skribe]] from Manuel Serrano in scheme (no more mantained)
 - [[http://cliki.net/Exscribe][Exscribe]] in Common Lisp
 - [[https://github.com/CommonDoc/common-doc][CommonDoc]] a framework in  Common Lisp for document with vertex.
 - [[https://github.com/mbattyani/cl-typesetting][cl-typesetting]] 

