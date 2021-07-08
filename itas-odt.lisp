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


(defmacro attributes* (ns prop-alist)
  (a:once-only (ns prop-alist)
    `(loop for (prop val) in ,prop-alist do
      (cxml:attribute* ,ns prop val))))

(defmacro with-style-named-element ((element name) &body body)
  `(cxml:with-element* ("style" ,element)
     (cxml:attribute* "style" "name" ,name)
     ,@body))

(defmacro odt-table ((&key (columns 1) (style "Tbl")) &body body)
  (a:once-only (columns)
    `(cxml:with-element* ("table" "table")
       (cxml:attribute* "table" "style-name" ,style)
       (typecase ,columns
         (number
          (cxml:with-element* ("table" "table-column")
            (cxml:attribute* "table" "style-name" (format nil "colonna-~d" ,columns))
            (cxml:attribute* "table" "number-columns-repeated" (format nil "~d" ,columns))))
         (list (loop for x in ,columns do
           (cxml:with-element* ("table" "table-column")
             (cxml:attribute* "table" "style-name" x)))))
       ,@body)))

(defmacro odt-table-row ((&key (style "TblR")) &body body)
  `(cxml:with-element* ("table" "table-row")
     (cxml:attribute* "table" "style-name" ,style)
     ,@body))

(defmacro odt-table-cell ((&key (style "TblCell") (span nil)) &body body)
  `(progn
     (cxml:with-element* ("table" "table-cell")
       (cxml:attribute* "table" "style-name" ,style)
       (cxml:attribute* "office" "value-type" "string")
       (when ,span
         (cxml:attribute* "table" "number-columns-spanned" (format nil "~d" ,span)))
       ,@body)
     (when ,span
       (loop repeat ,span do (cxml:with-element* ("table" "covered-table-cell"))))))

(defmacro odt-paragraph ((&key (style "Text_20_body")) &body body)
  `(cxml:with-element* ("text" "p")
     (cxml:attribute* "text" "style-name" ,style)
     ,@body))

(defmacro odt-span ((style) &body body)
  `(cxml:with-element* ("text" "span")
     (cxml:attribute* "text" "style-name" ,style)
     ,@body))

(defmacro odt-italic (&body body)
  `(cxml:with-element* ("text" "span")
     (cxml:attribute* "text" "style-name" "Italic")
     ,@body))

(defmacro odt-bold (&body body)
  `(cxml:with-element* ("text" "span")
     (cxml:attribute* "text" "style-name" "Bold")
     ,@body))

(defmacro odt-underline (&body body)
  `(cxml:with-element* ("text" "span")
     (cxml:attribute* "text" "style-name" "Underline")
     ,@body))

(defmacro odt-list ((&key (style "L1")) &body body)
  `(cxml:with-element* ("text" "list")
     (cxml:attribute* "text" "style-name" ,style)
     ,@body))

(defmacro odt-list-item ((&key (style nil)) &body body)
  `(cxml:with-element* ("text" "list-item")
     (odt-paragraph (:style "list-item-paragraph")
       ,@body)))

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
      (cxml:attribute* "style" "name" "Liberation Serif")
      (cxml:attribute* "svg" "font-family" "'Liberation Serif'")
      (cxml:attribute* "style" "font-family-generic" "roman")
      (cxml:attribute* "style" "font-pitch" "variable"))
    (cxml:with-element* ("style" "font-face")
      (cxml:attribute* "style" "name" "liberation sans")
      (cxml:attribute* "svg" "font-family" "'Liberation Sans'")
      (cxml:attribute* "style" "font-family-generic" "swiss")
      (cxml:attribute* "style" "font-pitch" "variable"))
    (cxml:with-element* ("style" "font-face")
      (cxml:attribute* "style" "name" "DejaVu Sans")
      (cxml:attribute* "svg" "font-family" "'DejaVu Sans'")
      (cxml:attribute* "style" "font-family-generic" "system")
      (cxml:attribute* "style" "font-pitch" "variable"))
    (with-style-named-element ("font-face" "OpenSymbol")
      (attributes* "svg" '(("font-family" "OpenSymbol")))
      (attributes* "style" '(("font-charset" "x-symbol"))))))

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
                          (cxml:text "scliba-odt-backend"))
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
            ;; default-styles
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
                (cxml:attribute* "style" "font-name" "Liberation Serif")
                (cxml:attribute* "style" "font-family-generic" "roman")
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
            ;; styles
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Standard")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "class" "text"))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Text_20_body")
              (cxml:attribute* "style" "display-name" "Text body")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "parent-style-name" "Standard")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "paragraph-properties")
                (cxml:attribute* "fo" "margin-top" "0cm")
                (cxml:attribute* "fo" "margin-bottom" "0.247cm")
                (cxml:attribute* "fo" "text-align" "justify")
                (cxml:attribute* "style" "contextual-spacing" "false")
                (cxml:attribute* "fo" "line-height" "115%")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Titoli")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "parent-style-name" "Standard")
              (cxml:attribute* "style" "next-style-name" "Text_20_body")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "paragraph-properties")
                (cxml:attribute* "fo" "margin-top" "0cm")
                (cxml:attribute* "fo" "margin-bottom" "0cm")
                (cxml:attribute* "style" "contextual-spacing" "false")
                (cxml:attribute* "fo" "text-align" "left")
                (cxml:attribute* "fo" "keep-with-next" "always"))
              (cxml:with-element* ("style" "text-properties")
                (cxml:attribute* "style" "font-pitch" "variable")                  
                (cxml:attribute* "fo" "font-size" "100%")
                (cxml:attribute* "fo" "font-weight" "bold")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Titoli-destra")
              (cxml:attribute* "style" "display-name" "Titoli a destra")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "parent-style-name" "Titoli")
              (cxml:attribute* "style" "next-style-name" "Text_20_body")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "paragraph-properties")
                (cxml:attribute* "fo" "text-align" "right")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Titoli-centro")
              (cxml:attribute* "style" "display-name" "Titoli al centro")
              (cxml:attribute* "style" "family" "paragraph")
              (cxml:attribute* "style" "parent-style-name" "Titoli")
              (cxml:attribute* "style" "next-style-name" "Text_20_body")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "paragraph-properties")
                (cxml:attribute* "fo" "text-align" "center")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Italic")
              (cxml:attribute* "style" "family" "text")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "text-properties")
                (cxml:attribute* "fo" "font-style" "italic")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Bold")
              (cxml:attribute* "style" "family" "text")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "text-properties")
                (cxml:attribute* "fo" "font-weight" "bold")))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Underline")
              (cxml:attribute* "style" "family" "text")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "text-properties")
                (attributes* "style" '(("text-underline-style" "solid")
                                       ("text-underline-width" "auto")
                                       ("text-underline-color" "font-color")))))
            (cxml:with-element* ("style" "style")
              (cxml:attribute* "style" "name" "Unbold")
              (cxml:attribute* "style" "family" "text")
              (cxml:attribute* "style" "class" "text")
              (cxml:with-element* ("style" "text-properties")
                (cxml:attribute* "fo" "font-weight" "normal")))
            (with-style-named-element ("style" "Dash_20_Symbols")
              (attributes* "style" '(("display-name" "Dash Symbols") ("family" "text")))
              (cxml:with-element* ("style" "text-properties")
                (attributes* "style" '(("font-name" "OpenSymbol") ("font-charset" "x-symbol")))
                (attributes* "fo" '(("font-family" "OpenSymbol"))))))
          ;; pages layout
          (cxml:with-element* ("office" "automatic-styles")
            (cxml:with-element* ("style" "page-layout")
              (cxml:attribute* "style" "name" "Mpm1")
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
              (cxml:attribute* "style" "name" "Standard")
              (cxml:attribute* "style" "page-layout-name" "Mpm1"))))))))


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
      (unwind-protect
           (progn
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
                 (cxml:with-xml-output (cxml:make-character-stream-sink *outstream* :indentation *xml-indentation*)
                   (export-document (read-file file) backend))))

             (zip:with-output-to-zipfile (zip outfile :if-exists :supersede)
               (zip:write-zipentry zip "mimetype" mimetype-file :deflate nil)
               (zip:write-zipentry zip "content.xml" content-file)
               (zip:write-zipentry zip "meta.xml" meta-file)
               (zip:write-zipentry zip "styles.xml" styles-file)
               (zip:write-zipentry zip "META-INF/manifest.xml" manifest-file)))
        ;; pulisci directory temporanea
        (dolist (x (list manifest-file content-file meta-file styles-file mimetype-file))
          (delete-file x))
        (uiop:delete-empty-directory (merge-pathnames "META-INF/" dir))
        (uiop:delete-empty-directory dir))
      outfile)))

;;;; export-document
(defun odt-table-style ()
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "Tbl")
    (cxml:attribute* "style" "family" "table")
    (cxml:with-element* ("style" "table-properties")
      (cxml:attribute* "style" "width" "17.0cm")
      (cxml:attribute* "fo" "margin-left" "0cm")
      (cxml:attribute* "style" "page-number" "auto")
      (cxml:attribute* "table" "align" "left")
      (cxml:attribute* "style" "writing-mode" "lr-tb")))
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "TblCol")
    (cxml:attribute* "style" "family" "table-column"))
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "TblR")
    (cxml:attribute* "style" "family" "table-row")
    (cxml:with-element* ("style" "table-row-properties")
      (cxml:attribute* "fo" "keep-together" "auto")
      (cxml:attribute* "style" "min-row-height" "36pt")))
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "TblCell")
    (cxml:attribute* "style" "family" "table-cell")
    (cxml:with-element* ("style" "table-cell-properties")
      (cxml:attribute* "style" "vertical-align" "middle")
      (cxml:attribute* "fo" "padding-left" "0.191cm")
      (cxml:attribute* "fo" "padding-right" "0.191cm")
      (cxml:attribute* "fo" "padding-top" "0cm")
      (cxml:attribute* "fo" "padding-bottom" "0cm")
      (cxml:attribute* "fo" "border" "0.5pt solid #000000")
      (cxml:attribute* "style" "writing-mode" "lr-tb")))
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "TblCellTop")
    (cxml:attribute* "style" "family" "table-cell")
    (cxml:with-element* ("style" "table-cell-properties")
      (cxml:attribute* "style" "vertical-align" "top")
      (cxml:attribute* "fo" "padding-left" "0.191cm")
      (cxml:attribute* "fo" "padding-right" "0.191cm")
      (cxml:attribute* "fo" "padding-top" "0cm")
      (cxml:attribute* "fo" "padding-bottom" "0cm")
      (cxml:attribute* "fo" "border" "0.5pt solid #000000")
      (cxml:attribute* "style" "writing-mode" "lr-tb")))
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "TabellaSubH.L")
    (cxml:attribute* "style" "family" "table-column")
    (cxml:with-element* ("style" "table-column-properties")
      (cxml:attribute* "style" "column-width" "12cm")))
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "TabellaSubH.R")
    (cxml:attribute* "style" "family" "table-column")
    (cxml:with-element* ("style" "table-column-properties")
      (cxml:attribute* "style" "column-width" "5cm")))
  (with-style-named-element ("style" "colonna-3")
    (attributes* "style" '(("family" "table-column")))
    (cxml:with-element* ("style" "table-column-properties")
      (cxml:attribute* "style" "column-width" "5.67cm")))
  (with-style-named-element ("style" "colonna-2")
    (attributes* "style" '(("family" "table-column")))
    (cxml:with-element* ("style" "table-column-properties")
      (cxml:attribute* "style" "column-width" "8.5cm")))
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "TabellaSubH.L1")
    (cxml:attribute* "style" "family" "table-cell")
    (cxml:with-element* ("style" "table-cell-properties")
      (cxml:attribute* "style" "vertical-align" "middle")
      (cxml:attribute* "fo" "padding-left" "0.191cm")
      (cxml:attribute* "fo" "padding-right" "0.191cm")
      (cxml:attribute* "fo" "padding-top" "0cm")
      (cxml:attribute* "fo" "padding-bottom" "0cm")
      (cxml:attribute* "fo" "border-top" "0.5pt solid #000000")
      (cxml:attribute* "fo" "border-bottom" "0.5pt solid #000000")
      (cxml:attribute* "fo" "border-left" "0.5pt solid #000000")
      (cxml:attribute* "style" "writing-mode" "lr-tb")))
  (cxml:with-element* ("style" "style")
    (cxml:attribute* "style" "name" "TabellaSubH.R1")
    (cxml:attribute* "style" "family" "table-cell")
    (cxml:with-element* ("style" "table-cell-properties")
      (cxml:attribute* "style" "vertical-align" "middle")
      (cxml:attribute* "fo" "padding-left" "0.191cm")
      (cxml:attribute* "fo" "padding-right" "0.191cm")
      (cxml:attribute* "fo" "padding-top" "0cm")
      (cxml:attribute* "fo" "padding-bottom" "0cm")
      (cxml:attribute* "fo" "border-top" "0.5pt solid #000000")
      (cxml:attribute* "fo" "border-bottom" "0.5pt solid #000000")
      (cxml:attribute* "fo" "border-right" "0.5pt solid #000000")
      (cxml:attribute* "style" "writing-mode" "lr-tb"))))

(defun odt-list-style ()
  (cxml:with-element* ("style" "style")
    (attributes* "style" '(("name" "list-item-paragraph")
                           ("family" "paragraph")
                           ("parent-style-name" "Standard")
                           ("list-style-name" "L1")))
    (cxml:with-element* ("style" "paragraph-properties")
      (cxml:attribute* "fo" "text-align" "justify"))
    (cxml:with-element* ("style" "text-properties")
      (attributes* "fo" '(("font-size" "10pt")))))
  (cxml:with-element* ("text" "list-style")
    (cxml:attribute* "style" "name" "L1")
    (cxml:with-element* ("text" "list-level-style-bullet")
      (attributes* "text" '(("level" "1")
                            ("style-name" "Dash_20_Symbols")
                            ("bullet-char" "-")))
      (cxml:with-element* ("style" "list-level-properties")
        (cxml:attribute* "text" "list-level-position-and-space-mode" "label-alignment")
        (cxml:with-element* ("style" "list-level-label-alignment")
          (attributes* "text" '(("label-followed-by" "space") ("list-tab-stop-position" "0cm")))
          (attributes* "fo" '(("text-indent" "0cm") ("margin-left" "0cm"))))))))

(defmethod export-document :around ((document authoring-document) (backend prog-odt-backend))
  (with-namespaces ("css3t" "grddl" "xhtml" "formx" "xsi" "rpt" "dc" "chart"  "svg" "draw" "text" "oooc" "style" "ooow" "meta" "xlink" "ooo" "fo" "office"  "dr3d" "table" "number" "of" "calcext" "tableooo" "drawooo" "dom" "field" "xsd" "math" "form" "script" "xforms")
    (cxml:with-element* ("office" "document-content")
      (cxml:attribute* "office" "version" *odf-version*) 
      (cxml:with-element* ("office" "scripts"))
      (output-font-face-decls)
      (cxml:with-element* ("office" "automatic-styles")
        (cxml:with-element* ("style" "style")
          (cxml:attribute* "style" "name" "P1")
          (cxml:attribute* "style" "family" "paragraph")
          (cxml:attribute* "style" "parent-style-name" "Text_20_body"))
        (odt-table-style)
        (odt-list-style))
      (cxml:with-element* ("office" "body")
        (cxml:with-element* ("office" "text")
          (call-next-method))))))


(defmethod export-document ((document programmazione) (backend prog-odt-backend))
  (odt-table (:columns '("TabellaSubH.L" "TabellaSubH.R"))
    (odt-table-row ()
      (odt-table-cell (:span 2)
        (odt-paragraph (:style "Titoli")
          (cxml:text "DOCENTE:")
          (odt-italic (cxml:text (get-argument document :docente))))))
    (odt-table-row ()
      (odt-table-cell (:style "TabellaSubH.L1")
        (odt-paragraph (:style "Titoli")
          (cxml:text "MATERIA: ")
          (odt-italic (cxml:text (get-argument document :materia)))))
      (odt-table-cell (:style "TabellaSubH.R1")
        (odt-paragraph (:style "Titoli-destra")
          (cxml:text "CLASSE: ")
          (odt-italic (cxml:text (get-argument document :classe)))))))
  (cxml:with-element* ("text" "p") (cxml:text " ")))

(defmethod export-document ((document modulo0) (backend prog-odt-backend))
  (with-document-arguments (title descrizione prerequisiti metodologie valutazione) document
    (odt-table (:columns 3)
      (odt-table-row ()
        (odt-table-cell (:span 3)
          (odt-paragraph (:style "Titoli")
            (cxml:text (format nil "MODULO N°~d: " (counter-modulo-val)))
            (odt-span ("Unbold")
              (cxml:text (format nil "~a" title))))
          (odt-paragraph (:style "Titoli-destra")
            (cxml:text (format nil "durata ore: " ))
            (odt-span ("Unbold") (cxml:text  (format nil "~d" (durata-ore document)))))))
      (odt-table-row ()
        (odt-table-cell (:span 3)
          (odt-paragraph () (cxml:text descrizione))))
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Prerequisiti")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Metodologie e strategie operative")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Modalità di valutazione"))))
      (odt-table-row ()
        (odt-table-cell (:style "TblCellTop")
          (export-document prerequisiti backend))
        (odt-table-cell (:style "TblCellTop") (export-document metodologie backend))
        (odt-table-cell (:style "TblCellTop")(export-document valutazione backend))))))

(defmethod export-document :around ((documnent itemize) (backend prog-odt-backend))
  (odt-list () (call-next-method)))

(defmethod export-document :around ((documnent item) (backend prog-odt-backend))
  (odt-list-item () (call-next-method)))

(defmethod export-document  ((document string) (backend prog-odt-backend))
  (cxml:text document))

(defmethod export-document :around ((document bf) (backend prog-odt-backend))
  (odt-bold (call-next-method)))

(defmethod export-document :around ((document underbar) (backend prog-odt-backend))
  (odt-underline (call-next-method)))

(defmethod export-document ((document u-didattica0) (backend prog-odt-backend))
  (counter-unita-inc)
  (with-document-arguments (title attivita obiettivi) document    
    (odt-table (:columns 2)
      (odt-table-row ()
        (odt-table-cell (:span 2)
          (odt-paragraph (:style "Titoli")
            (cxml:text (format nil "UNITÀ DIDATTICA N°~d.~d: " (counter-modulo-val) (counter-unita-val)))
            (odt-span ("Unbold")
              (cxml:text (format nil "~a" title))))
          (odt-paragraph (:style "Titoli-destra")
            (cxml:text (format nil "durata ore: " ))
            (odt-span ("Unbold") (cxml:text  (format nil "~d" (durata-ore document)))))))
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Attività")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Obiettivi"))))
      (odt-table-row ()
        (odt-table-cell (:style "TblCellTop") (export-document attivita backend))
        (odt-table-cell (:style "TblCellTop")(export-document obiettivi backend))))))

(defmethod export-document :around ((document modulo) (backend prog-odt-backend))
  (counter-modulo-inc)
  (counter-unita-set)
  (with-document-arguments (title competenze conoscenze livelli-minimi prerequisiti metodologie valutazione) document
    (odt-table (:columns 3)
      (odt-table-row ()
        (odt-table-cell (:span 3)
          (odt-paragraph (:style "Titoli")
            (cxml:text (format nil "MODULO N°~d: " (counter-modulo-val)))
            (odt-span ("Unbold")
              (cxml:text (format nil "~a" title))))
          (odt-paragraph (:style "Titoli-destra")
            (cxml:text (format nil "durata ore: " ))
            (odt-span ("Unbold") (cxml:text  (format nil "~d" (durata-ore document)))))))
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Competenze")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Conoscenze")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Livelli minimi"))))
      (odt-table-row ()
        (odt-table-cell (:style "TblCellTop")
          (export-document competenze backend))
        (odt-table-cell (:style "TblCellTop") (export-document conoscenze backend))
        (odt-table-cell (:style "TblCellTop")(export-document livelli-minimi backend)))
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Prerequisiti")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Metodologie e strategie operative")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Modalità di valutazione"))))
      (odt-table-row ()
        (odt-table-cell (:style "TblCellTop")
          (export-document prerequisiti backend))
        (odt-table-cell (:style "TblCellTop") (export-document metodologie backend))
        (odt-table-cell (:style "TblCellTop")(export-document valutazione backend)))))
  (call-next-method)
  (with-document-arguments (recupero valutazione-modulo) document
    (odt-table ()
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli") (cxml:text "ATTIVITÀ DI RECUPERO"))))
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph () (export-document recupero backend)))))
    (odt-table ()
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli") (cxml:text "VALUTAZIONE SOMATIVA DEL MODULO"))))
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph () (export-document  valutazione-modulo backend)))))))

(defmethod export-document ((document u-didattica) (backend prog-odt-backend))
  (counter-unita-inc)
  (with-document-arguments (title competenze conoscenze livelli-minimi prerequisiti metodologie valutazione) document
    (odt-table (:columns 3)
      (odt-table-row ()
        (odt-table-cell (:span 3)
          (odt-paragraph (:style "Titoli")
            (cxml:text (format nil "UNITÀ DIDATTICA N°~d.~d: " (counter-modulo-val) (counter-unita-val)))
            (odt-span ("Unbold")
              (cxml:text (format nil "~a" title))))
          (odt-paragraph (:style "Titoli-destra")
            (cxml:text (format nil "durata ore: " ))
            (odt-span ("Unbold") (cxml:text  (format nil "~d" (durata-ore document)))))))
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Competenze")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Conoscenze")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Livelli minimi"))))
      (odt-table-row ()
        (odt-table-cell (:style "TblCellTop")
          (export-document competenze backend))
        (odt-table-cell (:style "TblCellTop") (export-document conoscenze backend))
        (odt-table-cell (:style "TblCellTop")(export-document livelli-minimi backend)))
      (odt-table-row ()
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Prerequisiti")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Metodologie e strategie operative")))
        (odt-table-cell ()
          (odt-paragraph (:style "Titoli-centro")
            (cxml:text "Modalità di valutazione"))))
      (odt-table-row ()
        (odt-table-cell (:style "TblCellTop")
          (export-document prerequisiti backend))
        (odt-table-cell (:style "TblCellTop") (export-document metodologie backend))
        (odt-table-cell (:style "TblCellTop")(export-document valutazione backend))))))
