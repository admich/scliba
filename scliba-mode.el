
(define-derived-mode scliba-mode lisp-mode "sCLiba"
  "major mode for editing sCLiba code."
  )


(defvar scliba-mode-map (make-sparse-keymap)
  "Keymap for scliba-mode.")

(define-key scliba-mode-map "\C-c\C-c"  'scliba-compila-guarda-file)
