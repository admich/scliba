(in-package #:scliba)
;; hacking maybe to remove
(define-units 'time '((hour (* 3600 second) (hr hours) "h")))
(define-units 'length '((decimeter     0.1       (dm decimeters) "dm")))
(define-units 'current '((ampere     1       (amperes amps amp) "A")
			 (milliampere     (* milli ampere) (milliamp milliamps ma) "mA")
			 (microampere     (* micro ampere) (microamp microamps ua) "\\mu A")))

(define-units 'electric-potential
    '((volt      (/ (* kilogram meter meter)
		  (* ampere second second second))
		 (v volts) "V")
      (millivolt (* milli volt)  (mv millivolts) "mV")
      (microvolt (* micro volt)  (uv microvolts) "\\mu V")
      ))

(define-units 'resistance
    '((ohm      (/ (* kilogram meter meter)
		   (* ampere ampere second second second))
		(ohms) "\\Omega")
      (kilohm   (* kilo ohm)     (kilohms) "k\\Omega")
      (megohm   (* mega ohm)     (megohms) "M\\Omega")
      
       ))

(define-units 'power
    '((watt       (/ (* kilogram meter meter) (* second second second))
		  (w watts) "W")
      (milliwatt  (* milli watt) (mlw milli-watt milli-watts) "mW")
      (microwatt  (* micro watt) (uw micro-watt micro-watts) "\\mu W")
      (kilowatt   (* kilo watt) (kw kilowatts) "kW")
      (megawatt   (* mega watt) (mw megawatts mega-watt mega-watts) "MW")
      (gigawatt   (* giga watt) (gw gigawatts giga-watt giga-watts) "GW")
      (horsepower (* 550 (/ (* foot pound-force) second)) (hp) "hp")))

(define-units 'temperature
    '((kelvin      1.0       (k kelvin kelvins) "K")
      (celsius     1.0       (celsius) "°C")
      (rankine     5/9       ()) ))
