
(define-derived-mode scliba-mode lisp-mode "sCLiba"
  "major mode for editing sCLiba code."
  )


(defvar scliba-mode-map (make-sparse-keymap)
  "Keymap for scliba-mode.")

(define-key scliba-mode-map "\C-c\C-c"  'scliba-compila-guarda-file)


(define-skeleton scliba-skeleton-pq-format
  "Box per chiedere il Nome degli alunni nelle verifiche"
  nil
  "(pq-format " _ ")")

(define-skeleton scliba-skeleton-imath
  "Box per chiedere il Nome degli alunni nelle verifiche"
  nil
  "(imath " _ ")")

(define-abbrev-table 'scliba-mode-abbrev-table
  '(("pqf" "" scliba-skeleton-pq-format)
    ("imath" "" scliba-skeleton-imath)))



