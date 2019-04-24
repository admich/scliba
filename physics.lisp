(in-package #:physics)
(set-reader-in-file)

(defmacro define-scliba-physical-constant (name pq &key (precision 3) (exponent 0) (s-o-u *s-o-u*) new-unit)
  `(progn
     (define-physical-constant ,name ,pq)
     (defun ,name (&key (precision ,precision) (exponent ,exponent) (s-o-u ',s-o-u) (new-unit ,new-unit))
       (pq-format ,name :precision precision :exponent exponent :s-o-u s-o-u :new-unit new-unit))))

(define-physical-constant big-g
    #_6.67408d-11_N*m^2/kg^2)

(define-physical-constant little-g
    #_1_gs)

(define-scliba-physical-constant costante-stefan-boltzmann
    #_5.67e-8_W/m^2*K^4 :exponent nil :new-unit "W m^{-2} K^{-4}")

(define-scliba-physical-constant carica-e
    #_1.602176565e-19_coulomb :exponent nil)

(define-physical-constant raggio-terra
    #_6373_km)

(define-scliba-physical-constant k-elettrica
    #_8.98755e9_N*m^2/coulomb^2 :exponent nil :new-unit "N m^{2} C^{-2}")

(define-scliba-physical-constant carica-e
    #_1.602176565d-19_coulomb)


(defmacro tabella-costanti (name init)
  (let ((table-name (alexandria:symbolicate '* name '-tabella*)))
    `(progn
       (defparameter ,table-name (make-hash-table :test #'equalp))
       (dolist (x ,init)
         (setf (gethash (car x) ,table-name) (cdr x)))
       (defun ,name (element)
         (gethash element ,table-name))
       (export (list ',name ',table-name)))))

(tabella-costanti densita '(("mercurio" . #_1.36e4_kg/m^3)
                            ("alluminio" . #_2.7e3_kg/m^3)))

(tabella-costanti dilatazione-volumetrica '(("mercurio" . #_1.8e-4_/kelvin)
                                            ("acetone" . #_1.4e-3_/kelvin)
                                            ("etanolo" . #_1.1e-3_/kelvin)
                                            ("benzina" . #_9.4e-3_/kelvin)
                                            ("etere" . #_1.6e-3_/kelvin)
                                            ("glicerina" . #_5.3e-4_/kelvin)
                                            ("olio" . #_7.2e-4_/kelvin)))

(tabella-costanti calore-specifico '(("Alluminio" . #_880_J/kg-K)
			     ("Acciaio" . #_502_J/kg-k)
			     ("Acqua" . #_4186_J/kg-k)
			     ("Aria" . #_1005_J/kg-k)
			     ("Diamante" . #_502_J/kg-k)
			     ("Etanolo" . #_2460_J/kg-k)
			     ("Ferro" . #_460_J/kg-k)
			     ("Ghiaccio" . #_2090_J/kg-k)
			     ("Glicerina" . #_2260_J/kg-k)
			     ("Grafite" . #_720_J/kg-k)
			     ("Idrogeno" . #_14435_J/kg-k)
			     ("Mercurio" . #_139_J/kg-k)
			     ("Olio"  . #_2000_J/kg-k)
			     ("Oro" . #_129_J/kg-k)
			     ("Ottone" . #_377_J/kg-k)
			     ("Piombo" . #_130_J/kg-k)
			     ("Polistirene" . #_1450_J/kg-k)
			     ("Rame" . #_385_J/kg-k)
			     ("Stagno" . #_228_J/kg-k)
			     ("Zinco" . #_388_J/kg-k)))

(tabella-costanti conducibilita-termica
                  '(("argento" . #_460_W/m-K)
			        ("rame" . #_390_W/m-K)
			        ("oro" . #_320_W/m-K)
			        ("alluminio" . #_290_W/m-K)
			        ("ottone" . #_111_W/m-K)
			        ("piombo" . #_35_W/m-K)
			        ("acciaio" . #_17_W/m-K)
			        ("vetro" . #_1_W/m-K)
			        ("acqua" . #_0.6_W/m-K)
			        ("laterizi" . #_0.5_W/m-K)
			        ("carta" . #_0.2_W/m-K)
			        ("legno" . #_0.2_W/m-K)
			        ("sughero" . #_0.05_W/m-K)
			        ("lana" . #_0.04_W/m-K)
			        ("poliuretano" . #_0.03_W/m-K)
			        ("aria" . #_0.02_W/m-K)))

(tabella-costanti resistivita
                  '(("argento" . #_1.6e-8_ohm-m)
			        ("rame" . #_1.7e-8_ohm-m)
			        ("oro" . #_2.4e-8_ohm-m)
			        ("alluminio" . #_2.7e-8_ohm-m)
			        ("tungsteno" . #_5.6e-8_ohm-m)
                    ("ferro" . #_9.7e-8_ohm-m)
                    ("stagno" . #_1.1e-7_ohm-m)
                    ("costantana" . #_4.9e-7_ohm-m)
                    ("germanio" .  #_4.6e-1_ohm-m)
                    ("silicio" . #_6.4e2_ohm-m)
                    ("vetro" . (#_1e11_ohm-m . #_1e15_ohm-m))
                    ("legno" . (#_1e14_ohm-m . #_1e16_ohm-m))
                    ("quarzo" . #_7.5e17_ohm-m)
                    ("teflon" . (#_1e23_ohm-m . #_1e25_ohm-m))))

(tabella-costanti resistivita-coefficiente-termico
                  '(("argento" . #_0.0038_/K)
			        ("rame" . #_0.00404_/K)
			        ("oro" . #_0.0034_/K)
			        ("alluminio" . #_0.0039_/K)
			        ("tungsteno" . #_0.0045_/K)
                    ("ferro" . #_0.005_/K)
                    ("stagno" . #_0.0045_/K)
                    ("costantana" . #_0.000008_/K)
                    ("germanio" .  #_-0.048_/K)
                    ("silicio" . #_-0.075_/K)))
