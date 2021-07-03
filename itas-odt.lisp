(in-package :scliba-itas)

(defparameter *xml-indentation* 2)

(defparameter *odf-version* "1.3")
(eval-when (::compile-toplevel :load-toplevel :execute)
  (defparameter *namespaces*
    '(("manifest" . "urn:oasis:names:tc:opendocument:xmlns:manifest:1.0")
      ("office" . "urn:oasis:names:tc:opendocument:xmlns:office:1.0")
      ("grddl" . "http://www.w3.org/2003/g/data-view#")
      ("dc" . "http://purl.org/dc/elements/1.1/")
      ("xlink" . "http://www.w3.org/1999/xlink")
      ("meta" . "urn:oasis:names:tc:opendocument:xmlns:meta:1.0")
      ("ooo" . "http://openoffice.org/2004/office")
      ("css3t" . "http://www.w3.org/tr/css3-text/")
      ("rpt" . "http://openoffice.org/2005/report")
      ("chart" . "urn:oasis:names:tc:opendocument:xmlns:chart:1.0")
      ("svg" . "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0")
      ("draw" . "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0")
      ("text" . "urn:oasis:names:tc:opendocument:xmlns:text:1.0")
      ("oooc" . "http://openoffice.org/2004/calc")
      ("style" . "urn:oasis:names:tc:opendocument:xmlns:style:1.0")
      ("ooow" . "http://openoffice.org/2004/writer")
      ("fo" . "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0")
      ("dr3d" . "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0")
      ("table" . "urn:oasis:names:tc:opendocument:xmlns:table:1.0")
      ("number" . "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0")
      ("of" . "urn:oasis:names:tc:opendocument:xmlns:of:1.2")
      ("calcext" . "urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0")
      ("tableooo" . "http://openoffice.org/2009/table")
      ("drawooo" . "http://openoffice.org/2010/draw")
      ("dom" . "http://www.w3.org/2001/xml-events")
      ("field" . "urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0")
      ("math" . "http://www.w3.org/1998/math/mathml")
      ("form" . "urn:oasis:names:tc:opendocument:xmlns:form:1.0")
      ("script" . "urn:oasis:names:tc:opendocument:xmlns:script:1.0")
      ("xhtml" . "http://www.w3.org/1999/xhtml")
      ("formx" . "urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0")
      ("xsi" . "http://www.w3.org/2001/xmlschema-instance")
      ("xsd" . "http://www.w3.org/2001/xmlschema")
      ("xforms" . "http://www.w3.org/2002/xforms")
      ;; extendend conformant
      ("loext" . "urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0")
      ("officeooo" . "http://openoffice.org/2009/office"))))

(defmacro with-namespaces (namespaces &body body)
  (if namespaces
      (let* ((aa (car namespaces))
             (bb (cdr (assoc aa *namespaces* :test #'string=)))
             (cc (cdr namespaces)))
        `(cxml:with-namespace (,aa ,bb)
           (with-namespaces ,cc ,@body)))
      `(progn ,@body)))

(defun output-font-face-decls ()
  (cxml:with-element* ("office" "font-face-decls")    
    (cxml:with-element* ("style" "font-face")
      (cxml:attribute* "style" "name" "liberation serif")
      (cxml:attribute* "svg" "font-family" "'liberation serif'")
      (cxml:attribute* "style" "font-family-generic" "roman")
      (cxml:attribute* "style" "font-pitch" "variable"))
    (cxml:with-element* ("style" "font-face")
      (cxml:attribute* "style" "name" "liberation sans")
      (cxml:attribute* "svg" "font-family" "'liberation sans'")
      (cxml:attribute* "style" "font-family-generic" "swiss")
      (cxml:attribute* "style" "font-pitch" "variable"))
    (cxml:with-element* ("style" "font-face")
      (cxml:attribute* "style" "name" "dejavu sans")
      (cxml:attribute* "svg" "font-family" "'dejavu sans'")
      (cxml:attribute* "style" "font-family-generic" "system")
      (cxml:attribute* "style" "font-pitch" "variable"))))

(defun write-manifest-file (file mimetype &key (indentation *xml-indentation*))
  (with-open-file (ostream file :direction :output :if-exists :supersede)
    (cxml:with-xml-output (cxml:make-character-stream-sink ostream :indentation indentation)
      (cxml:with-namespace ("manifest" "urn:oasis:names:tc:opendocument:xmlns:manifest:1.0") ;; manifest:version=??
	    (cxml:with-element* ("manifest" "manifest")
          (cxml:attribute* "manifest" "version" *odf-version*)
	      (cxml:with-element* ("manifest" "file-entry")
	        (cxml:attribute* "manifest" "full-path" "/") ;; version ??
	        (cxml:attribute* "manifest" "media-type" mimetype))
	      (cxml:with-element* ("manifest" "file-entry")
	        (cxml:attribute* "manifest" "full-path" "styles.xml")
	        (cxml:attribute* "manifest" "media-type" "text/xml"))
	      (cxml:with-element* ("manifest" "file-entry")
	        (cxml:attribute* "manifest" "full-path" "content.xml")
	        (cxml:attribute* "manifest" "media-type" "text/xml"))
	      (cxml:with-element* ("manifest" "file-entry")
	        (cxml:attribute* "manifest" "full-path" "meta.xml")
	        (cxml:attribute* "manifest" "media-type" "text/xml")))))))

(defun write-meta-file (file author &key (indentation *xml-indentation*))
  (let ((date (local-time:format-rfc3339-timestring nil (local-time:now))))
    (with-open-file (ostream file :direction :output :if-exists :supersede)
      (cxml:with-xml-output (cxml:make-character-stream-sink ostream :indentation indentation)
        (cxml:with-namespace ("office" "urn:oasis:names:tc:opendocument:xmlns:office:1.0")
          (cxml:with-namespace ("grddl" "http://www.w3.org/2003/g/data-view#")
            (cxml:with-namespace  ("dc" "http://purl.org/dc/elements/1.1/")
              (cxml:with-namespace ("xlink" "http://www.w3.org/1999/xlink")
                (cxml:with-namespace ("meta" "urn:oasis:names:tc:opendocument:xmlns:meta:1.0")
                  (cxml:with-namespace ("ooo" "http://openoffice.org/2004/office")
                    (cxml:with-element* ("office" "document-meta")
                      (cxml:attribute* "office" "version" *odf-version*) 
                      (cxml:with-element* ("office" "meta")
                        (cxml:with-element* ("meta" "generator")
                          (cxml:text "admich-cl-odf"))
                        (cxml:with-element* ("meta" "initial-creator")
                          (cxml:text author))
                        (cxml:with-element* ("dc" "creator")
                          (cxml:text author))
                        (cxml:with-element* ("meta" "creation-date")
                          (cxml:text date))
                        (cxml:with-element* ("dc" "date")
                          (cxml:text date))))))))))))))

(defun write-style-file (file &key (indentation *xml-indentation*))
  (with-open-file (ostream file :direction :output :if-exists :supersede)
    (cxml:with-xml-output (cxml:make-character-stream-sink ostream :indentation indentation)
      (with-namespaces ("office" "grddl" "dc" "xlink" "ooo" "css3t" "rpt" "chart" "svg" "draw" "text" "oooc" "style" "ooow" "fo" "dr3d" "table" "number" "of" "calcext" "tableooo" "drawooo" "dom" "field" "math" "form" "script" "xhtml")
        (cxml:with-element* ("office" "document-styles")
          (cxml:attribute* "office" "version" *odf-version*)
          (output-font-face-decls)
          (cxml:with-element* ("office" "styles")
            (cxml:with-element* ("style" "default-style")
              (cxml:attribute* "style" "family" "table")
              (cxml:with-element* ("style" "table-properties")
                (cxml:attribute* "table" "border-model" "collapsing")))
            (cxml:with-element* ("style" "default-style")
              (cxml:attribute* "style" "family" "table-row")
              (cxml:with-element* ("style" "table-row-properties")
                (cxml:attribute* "fo" "keep-together" "auto")))
            (cxml:with-element* ("style" "default-style")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:with-element* ("style" "paragraph-properties")
                (cxml:attribute* "fo" "orphans" "2")
                (cxml:attribute* "fo" "widows" "2")
                (cxml:attribute* "fo" "hyphenation-ladder-count" "no-limit")
                (cxml:attribute* "style" "text-autospace" "ideograph-alpha")
                (cxml:attribute* "style" "punctuation-wrap" "hanging")
                (cxml:attribute* "style" "line-break" "strict")
                (cxml:attribute* "style" "tab-stop-distance" "1.251cm")
                (cxml:attribute* "style" "writing-mode" "page"))
              (cxml:with-element* ("style" "text-properties")
                (cxml:attribute* "style" "use-window-font-color" "true")
                ;; (cxml:attribute* "loext" "opacity" "0%")
                (cxml:attribute* "style" "font-name" "liberation serif")
                (cxml:attribute* "fo" "font-size" "12pt")
                (cxml:attribute* "fo" "language" "it")
                (cxml:attribute* "fo" "country" "it")
                (cxml:attribute* "style" "letter-kerning" "true")
                (cxml:attribute* "style" "font-name-asian" "dejavu sans")
                (cxml:attribute* "style" "font-size-asian" "10.5pt")
                (cxml:attribute* "style" "language-asian" "zh")
                (cxml:attribute* "style" "country-asian" "cn")
                (cxml:attribute* "style" "font-name-complex" "freesans")
                (cxml:attribute* "style" "font-size-complex" "12pt")
                (cxml:attribute* "style" "language-complex" "hi")
                (cxml:attribute* "style" "country-complex" "in")
                (cxml:attribute* "fo" "hyphenate" "false")
                (cxml:attribute* "fo" "hyphenation-remain-char-count" "2")
                (cxml:attribute* "fo" "hyphenation-push-char-count" "2")
                ;;(cxml:attribute* "loext" "hyphenation-no-caps" "false")
                ))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "standard")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "class" "text"))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "heading")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "parent-style-name" "standard")
              (cxml:attribute* "style" "next-style-name" "text_20_body")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "paragraph-properties")
                (cxml:attribute* "fo" "margin-top" "0.423cm")
                (cxml:attribute* "fo" "margin-bottom" "0.212cm")
                (cxml:attribute* "style" "contextual-spacing" "false")
                (cxml:attribute* "fo" "keep-with-next" "always"))
              (cxml:with-element* ("style" "text-properties")
                (cxml:attribute* "style" "font-name" "liberation sans")
                (cxml:attribute* "fo" "font-family" "'liberation sans'")
                (cxml:attribute* "style" "font-family-generic" "swiss")
                (cxml:attribute* "style" "font-pitch" "variable")
                (cxml:attribute* "fo" "font-size" "12pt")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "heading_20_1")
              (cxml:attribute* "style" "display-name" "heading 1")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "parent-style-name" "heading")
              (cxml:attribute* "style" "next-style-name" "text_20_body")
              (cxml:attribute* "style" "default-outline-level" "1")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "paragraph-properties")
                (cxml:attribute* "fo" "margin-top" "0.423cm")
                (cxml:attribute* "fo" "margin-bottom" "0.212cm")
                (cxml:attribute* "style" "contextual-spacing" "false"))
              (cxml:with-element* ("style" "text-properties")
                (cxml:attribute* "fo" "font-size" "100%")
                (cxml:attribute* "fo" "font-weight" "bold")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "italic")
              (cxml:attribute* "style" "family" "text")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "text-properties")
                (cxml:attribute* "fo" "font-style" "italic")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "text_20_body")
              (cxml:attribute* "style" "display-name" "text body")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "parent-style-name" "standard")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "paragraph-properties")
                (cxml:attribute* "fo" "margin-top" "0cm")
                (cxml:attribute* "fo" "margin-bottom" "0.247cm")
                (cxml:attribute* "style" "contextual-spacing" "false")
                (cxml:attribute* "fo" "line-height" "115%")))
            (cxml:with-element* ("text" "linenumbering-configuration")
              (cxml:attribute* "text" "number-lines" "false")
              (cxml:attribute* "text" "offset" "0.499cm")
              (cxml:attribute* "style" "num-format" "1")
              (cxml:attribute* "text" "number-position" "left")
              (cxml:attribute* "text" "increment" "5")))
          (cxml:with-element* ("office" "automatic-styles")
            (cxml:with-element* ("style" "page-layout")
              (cxml:attribute* "style" "name" "mpm1")
              (cxml:with-element* ("style" "page-layout-properties")
                (cxml:attribute* "fo" "page-width" "21.0cm")
                (cxml:attribute* "fo" "page-height" "29.7cm")
                (cxml:attribute* "style" "num-format" "1")
                (cxml:attribute* "style" "print-orientation" "portrait")
                (cxml:attribute* "fo" "margin-top" "2cm")
                (cxml:attribute* "fo" "margin-bottom" "2cm")
                (cxml:attribute* "fo" "margin-left" "2cm")
                (cxml:attribute* "fo" "margin-right" "2cm")
                (cxml:attribute* "style" "writing-mode" "lr-tb")
                (cxml:attribute* "style" "footnote-max-height" "0cm")
                (cxml:with-element* ("style" "footnote-sep")
                  (cxml:attribute* "style" "width" "0.018cm")
                  (cxml:attribute* "style" "distance-before-sep" "0.101cm")
                  (cxml:attribute* "style" "distance-after-sep" "0.101cm")
                  (cxml:attribute* "style" "line-style" "solid")
                  (cxml:attribute* "style" "adjustment" "left")
                  (cxml:attribute* "style" "rel-width" "25%")
                  (cxml:attribute* "style" "color" "#000000")))
              (cxml:with-element* ("style" "header-style"))
              (cxml:with-element* ("style" "footer-style"))))
          (cxml:with-element* ("office" "master-styles")
            (cxml:with-element* ("style" "master-page")
              (cxml:attribute* "style" "name" "standard")
              (cxml:attribute* "style" "page-layout-name" "mpm1"))))))))


;;;; Backend
(defun view-odt (file)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname file))
    (let* ((file-new (format nil "~a" (merge-pathnames (make-pathname :type "odt") (pathname-name file))))
	       (command "libreoffice"))
      (uiop:run-program (list command file-new) :output t))))

(defclass prog-odt-backend (backend)
  ()
  (:default-initargs :view-fn #'view-odt))

(defmethod standard-output-file (file (backend prog-odt-backend))
  (merge-pathnames (merge-pathnames (pathname-name file) "odt/prova.odt") file))

(defmethod export-file :around ((file t) (backend prog-odt-backend))
  (labels ((tmp-odt-dir (path)
             (let ((name (format nil "~a" (a:make-gensym (pathname-name path)))))
               (merge-pathnames (concatenate 'string "scliba-odt/" name "/") (uiop:temporary-directory)))))
    (let* ((*top-level-document* t)
	       (outfile (standard-output-file file backend))
           (dir (tmp-odt-dir file))
           (manifest-file (merge-pathnames "META-INF/manifest.xml" dir))
           (content-file (merge-pathnames "content.xml" dir))
           (meta-file (merge-pathnames "meta.xml" dir))
           (styles-file (merge-pathnames "styles.xml" dir))
           (mimetype-file (merge-pathnames "mimetype" dir))
           (mimetype "application/vnd.oasis.opendocument.text"))
      (uiop:ensure-all-directories-exist (list outfile manifest-file))
       ;; mimetype
       (with-open-file (*standard-output* mimetype-file :direction :output :if-exists :supersede)
         (format t mimetype))
       ;; manifestfile
       (write-manifest-file manifest-file mimetype)
       ;; meta xml file
       (write-meta-file meta-file "Andrea De Michele")
       ;; manifest.rdf TODO
       ;; thumbnails TODO
       ;; style file
       (write-style-file styles-file)

      ;; Rivedere usando (call-next-method)
      (with-open-file (stream content-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (let ((*outstream* stream)
              (*output-file* outfile))
          (export-document (read-file file) backend)))

      (zip:with-output-to-zipfile (zip outfile :if-exists :supersede)
         (zip:write-zipentry zip "mimetype" mimetype-file :deflate nil)
         (zip:write-zipentry zip "content.xml" content-file)
         (zip:write-zipentry zip "meta.xml" meta-file)
         (zip:write-zipentry zip "styles.xml" styles-file)
         (zip:write-zipentry zip "META-INF/manifest.xml" manifest-file))
       ;; pulisci directory temporanea
      outfile)))

;;;; export-document

