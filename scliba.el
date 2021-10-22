(require 'cl)
(load "skeletons-cl-authoring" nil)

(defun pedb-store-dir ()
  (substring (second (slime-eval '(swank:eval-and-grab-output "(directory-namestring pedb::*esercizi-directory*)"))) 1 -1))

(defun pedb-preview-dir ()
  (substring (second (slime-eval '(swank:eval-and-grab-output "(directory-namestring pedb::*esercizi-preview-directory*)"))) 1 -1))

(defvar *pedb-raccolta-buffer* nil "nome file e buffer del compito associato")

(defun pedb-esercizi-argomenti ()
  (car (read-from-string (second (slime-eval '(swank:eval-and-grab-output "pedb::*esercizi-argomenti*"))))))

(defun pedb-esercizi-tipi ()
  (car (read-from-string (second (slime-eval '(swank:eval-and-grab-output "pedb::*esercizi-tipi*"))))))

(defun pedb-max-num ()
  "return the maximum number of exercize"
  (apply 'max (mapcar (lambda (x) (string-to-number (substring x -9 -4)))  (remove-if-not (lambda (x) (string-match-p "^q-.*.[0-9]\." x)) (directory-files (pedb-store-dir))))))

(defun pedb-new-esercizio ()
  (interactive)
  (let* ((argomento (cdr (assoc (completing-read "Argomento: " (pedb-esercizi-argomenti)) (pedb-esercizi-argomenti))))
	 (tipo (cdr (assoc (completing-read "Tipo: " (pedb-esercizi-tipi)) (pedb-esercizi-tipi))))
	 (esnum (+ 1 (pedb-max-num)))
	 (name (format "q-%s-%s-%05d" argomento tipo esnum))
	 (fname (concat name ".lisp")))
    (find-file (expand-file-name fname (pedb-store-dir)))
    (lisp-mode)
    (pcase tipo
      ("ch" (cl-auth-skeleton-esercizio-scelte))
      ;; ("tf" (context-skeleton-esercizio-truefalse))
      (_ (cl-auth-skeleton-esercizio)))))

(defun pedb-new-compito ()
  (interactive)
  (let* ((fname (read-from-minibuffer "File Name (es: 15-al-i-q1c1.lisp): "))
	 (file-to-open
	  (second (slime-eval `(swank:eval-and-grab-output
				,(concat "(pedb::new-compito (merge-pathnames \"" fname "\" pedb::*compiti-directory*))"))))))
    (find-file (read file-to-open))
    (lisp-mode)))

;; (defun exe-new-esercitazione ()
;;   (interactive)
;;   (let ((fname (read-from-minibuffer "File Name (es: esercizi-argomento.tex): ")))
;;     (find-file (expand-file-name fname *exe-esercitazioni-dir*))
;;     (context-mode)
;;     (context-skeleton-esercitazione)))

(defun pedb-all-exercises ()
  "Return all the exercises"
  (first  (read-from-string
           (second
            (slime-eval
             '(swank:eval-and-grab-output "(map 'list (lambda (x) (format nil \"~a\" x)) (pedb:tutti-esercizi))"))))))

(defvar pedb-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "e" 'exe-edit-exercise)
    (define-key map (kbd "RET") 'exe-edit-exercise)
    (define-key map "E" 'pedb-new-esercizio)
    (define-key map "c" 'pedb-genera-esercizio)
    (define-key map "v" 'pedb-view-exercise)
    (define-key map "V" 'pedb-compile-and-view-exercise)
    (define-key map "N" 'pedb-new-compito)
    (define-key map "s" 'pedb-set-raccolta)
    ;; (define-key map "o" 'exe-open-compilation)
    (define-key map "a" 'pedb-add-esercizio)
    ;; (define-key map "A" 'exe-add-exercise-component)
    map)
  "Local keymap for `pedb-list-mode' buffers.")

;; (defun exe-open-compilation ()
;;   "open the compilation buffer"
;;   (interactive)
;;   (switch-to-buffer-other-window *exe-compilation-buffer*))

(defun pedb-set-raccolta (arg buf)
  "Set the compito or esercitazione buffer"
  (interactive "P\nbbuffer:")
  (let ((buf-db (get-buffer (concat "*PEDB*:" *pedb-raccolta-buffer*))))
    (with-current-buffer buf-db
      (setq *pedb-raccolta-buffer* buf)
      (rename-buffer (concat "*PEDB*:" *pedb-raccolta-buffer*)))))

(define-derived-mode pedb-list-mode tabulated-list-mode "PEDB"
  "Major mode for browsing a list of exercise.
Letters do not insert themselves; instead, they are commands."
; \\<package-menu-mode-map>
; \\{package-menu-mode-map}
; "
  (setq tabulated-list-format
        `[("N." 8 t)
	  ("File" 20 t)
	  ("Argomento" 20 t)
	  ("Tipoplogia" 15 t)
	  ("Modificato" 15 t)])
  (setq tabulated-list-padding 2)
  ;(add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
  (tabulated-list-init-header))

(defun pedb-exe-get-topic (exe)
  "Return the argument of an exercise"
  (second (split-string exe "-")))

(defun pedb-exe-get-number (exe)
  "Return the number of an exercise"
  (fourth (split-string exe "[-.]")))

(defun pedb-exe-get-type (exe)
  "Return the type of an exercise"
  (third (split-string exe "[-.]")))

(defun pedb-exe-get-modified (exe)
  "Return the last modified date of an exercise"
  (format-time-string "%Y-%m-%d %H:%M" (sixth (file-attributes exe))))

(defun exe-edit-exercise ()
  (interactive)
  (message "%s" (tabulated-list-get-id))
  (find-file-other-window (tabulated-list-get-id)))

(defun pedb-add-esercizio ()
  (interactive)
  (let ((exe (tabulated-list-get-id)))
    (with-current-buffer *pedb-raccolta-buffer*
      (insert "\"" (subseq exe 0 -5) "\" \n"))))

(defun pedb-esercizio--print-info (exe)
  (let ((exe-name (file-name-base exe)))
    (list exe `[,(pedb-exe-get-number exe-name)
		,(file-name-base exe) ;	      ,(list (file-name-base exe))
		,(pedb-exe-get-topic exe-name)
		,(pedb-exe-get-type exe-name)
		,(pedb-exe-get-modified exe)])))

(defun pedb-show-db ()
  "Dispay exercize"
  (interactive)
  (let ((buf (get-buffer-create (concat "*PEDB*:" *pedb-raccolta-buffer*))))
    (with-current-buffer buf
      (pedb-list-mode)
      (setq default-directory (pedb-store-dir))
      (setq tabulated-list-entries (mapcar #'pedb-esercizio--print-info  (pedb-all-exercises)))
      (tabulated-list-print)
      ;(dolist (i (exe-all-exercise)) (insert (file-name-base i)) (insert "\n"))
      )
    (switch-to-buffer buf)))

;;; compile
(defun scliba-compila-guarda-file ()
  (interactive)
  (let* ((file-name (buffer-file-name))
         (comand (format "(scliba:compila-guarda %S)" file-name)))
    (slime-eval `(swank:eval-and-grab-output ,comand))
    (message "Compilato file %s" file-name)))

(defun pedb-genera-esercizio ()
  (interactive)
  (let ((comand
	 (concat "(pedb:compila-esercizio-preview \"" (tabulated-list-get-id) "\")")))

    (slime-eval `(swank:eval-and-grab-output ,comand))
    (message "Compilato esercizio %s" (tabulated-list-get-id))))

(defun pedb-view-exercise ()
  (interactive)
  (let ((command (concat "(funcall (scliba::backend-view-fn scliba:*default-backend*) (scliba:standard-output-file \"" (tabulated-list-get-id) "\" scliba:*default-backend*))")))
    (slime-eval `(swank:eval-and-grab-output ,command))))

(defun pedb-compile-and-view-exercise ()
  (interactive)
  (save-excursion (pedb-genera-esercizio))
  (pedb-view-exercise))

;;; prove
;(slime-eval-async `(swank:eval-and-grab-output "(+ 1 2)"))

