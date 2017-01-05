(require 'cl)
(load "skeletons-cl-authoring" t)
(defun pedb-store-dir ()
  (substring (second (slime-eval '(swank:eval-and-grab-output "(directory-namestring scliba::*esercizi-directory*)"))) 1 -1))

(defun pedb-preview-dir ()
  (substring (second (slime-eval '(swank:eval-and-grab-output "(directory-namestring scliba::*esercizi-preview-directory*)"))) 1 -1))

(defvar *pedb-raccolta-buffer* nil "nome file e buffer del compito associato")

;; (defvar *exe-compiti-dir* "/home/admich/Documenti/scuola/my-didattica/pedb/compiti/" "nome del file dove salvare compiti")

;; (defvar *exe-esercitazioni-dir* "/home/admich/Documenti/scuola/my-didattica/pedb/esercitazioni/" "nome del file dove salvare esercitazioni")


(defun pedb-esercizi-argomenti ()
  (car (read-from-string (second (slime-eval '(swank:eval-and-grab-output "scliba::*esercizi-argomenti*"))))))
(defun pedb-esercizi-tipi ()
  (car (read-from-string (second (slime-eval '(swank:eval-and-grab-output "scliba::*esercizi-tipi*"))))))





(defun pedb-max-num ()
  "return the maximum number of exercize"
  (apply 'max (mapcar (lambda (x) (string-to-number (substring x -9 -4)))  (remove-if-not (lambda (x) (string-match-p "^q-.*.[0-9]\." x)) (directory-files (pedb-store-dir))))))


(defun pedb-new-esercizio ()
  (interactive)
  (let* ((argomento (cdr (assoc (completing-read "Argomento: " (pedb-esercizi-argomenti)) (pedb-esercizi-argomenti))))
	 (tipo (cdr (assoc (completing-read "Tipo: " (pedb-esercizi-tipi)) (pedb-esercizi-tipi))))
	 (esnum (+ 1 (pedb-max-num)))
	 (name (format "q-%s-%s-%05d" argomento  tipo esnum))
	 (fname (concat name ".lisp")))
    (find-file (expand-file-name fname (pedb-store-dir)))
    (lisp-mode)
    (pcase tipo
      ("ch" (cl-auth-skeleton-esercizio-scelte))
      ;; ("tf" (context-skeleton-esercizio-truefalse))
      (_ (cl-auth-skeleton-esercizio)))
    ))

;; (defun exe-new-compito-lisp ()
;;   (interactive)
;;   (let ((fname (read-from-minibuffer "File Name (es: 15-al-i-q1c1.lisp): ")))
;;     (find-file (expand-file-name fname *exe-lisp-dir*))
;;     (lisp-mode)
;;     (cl-auth-skeleton-compito)))

;; (defun exe-new-compito ()
;;   (interactive)
;;   (let ((fname (read-from-minibuffer "File Name (es: 15-al-i-q1c1.tex): ")))
;;     (find-file (expand-file-name fname *exe-compiti-dir*))
;;     (context-mode)
;;     (context-skeleton-compito)))

;; (defun exe-new-esercitazione ()
;;   (interactive)
;;   (let ((fname (read-from-minibuffer "File Name (es: esercizi-argomento.tex): ")))
;;     (find-file (expand-file-name fname *exe-esercitazioni-dir*))
;;     (context-mode)
;;     (context-skeleton-esercitazione)))


(defun pedb-all-exercises ()
  "Return all the exercises"
  (map 'list (lambda (x) (concat x ".lisp")) (first (read-from-string (second (slime-eval '(swank:eval-and-grab-output "(map 'list #'pathname-name (scliba::tutti-esercizi))")))))))

  
;; (defun exe-all-exercises-with-topic (topic)
;;   "Return all the exercise of with a given topic"
;;   (remove-if-not (lambda (x) (string= topic (exe-get-topic x)))(directory-files *exe-store-dir* nil "\\.tex$")))

(defvar pedb-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "e" 'exe-edit-exercise)
    (define-key map (kbd "RET") 'exe-edit-exercise)
    (define-key map "E" 'pedb-new-esercizio)
    (define-key map "c" 'pedb-genera-esercizio)
    (define-key map "v" 'pedb-view-exercise)
    (define-key map "V" 'pedb-compile-and-view-exercise)
    ;; (define-key map "N" 'exe-new-compito)
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
  (find-file-other-window (concat (pedb-store-dir) (tabulated-list-get-id))))

(defun pedb-add-esercizio ()
  (interactive)
  (let ((exe (tabulated-list-get-id)))
    (with-current-buffer *pedb-raccolta-buffer*
      (insert "\"" (subseq exe 0 -5) "\" "))))

;; (defun exe-add-exercise-component ()
;;   (interactive)
;;   (let ((exe (tabulated-list-get-id)))
;;     (with-current-buffer *exe-compilation-buffer*
;;       (insert "\n\\component " (subseq exe 0 -4)))))


(defun pedb-genera-esercizio ()
  (interactive)
  (let ((comand (concat "(scliba::genera-esercizio-preview \"" (file-name-base (tabulated-list-get-id)) "\")")))
;    (message comand)
    (slime-eval `(swank:eval-and-grab-output ,comand))
    (message "Compilato esercizio %s" (tabulated-list-get-id))))
    ;(slime-eval '(swank:eval-and-grab-output comand))))

(defun pedb-view-exercise ()
  (interactive)
  (let ((pdf (concat (pedb-preview-dir) (file-name-base (tabulated-list-get-id)) ".pdf")))
    (if (not (file-exists-p pdf)) (pedb-genera-esercizio))
    (if (file-exists-p pdf)
	(save-excursion (find-alternate-file-other-window pdf))
      (message "pdf non presente"))))

(defun pedb-compile-and-view-exercise ()
  (interactive)
  (save-excursion (pedb-genera-esercizio))
  (let ((pdf (concat (pedb-preview-dir) (file-name-base (tabulated-list-get-id)) ".pdf")))
    (if (file-exists-p pdf)
	(save-excursion (find-alternate-file-other-window pdf))
      (message "pdf non presente"))))

(defun pedb-esercizio--print-info (exe)
  (list exe `[,(pedb-exe-get-number exe)
	      ,(file-name-base exe)			;	      ,(list (file-name-base exe))
	      ,(pedb-exe-get-topic exe)
	      ,(pedb-exe-get-type exe)
	      ,(pedb-exe-get-modified exe)]))

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


;;; prove
;(slime-eval-async `(swank:eval-and-grab-output "(+ 1 2)"))

