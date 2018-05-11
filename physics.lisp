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

(define-physical-constant costante-stefan-boltzmann
    #_5.67e-8_W/m^2*K^4)

(define-scliba-physical-constant carica-e
    #_1.602176565e-19_coulomb :exponent nil)

(define-physical-constant raggio-terra
    #_6373_km)

(define-scliba-physical-constant k-elettrica
    #_8.98755e9_N*m^2/coulomb^2 :exponent nil :new-unit "N m^{2} C^{-2}")


