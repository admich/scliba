(in-package #:pedb)
(compito (:title "Verifica di fisica"  :rfoot (format nil " ott. 2016 (W43) f~d" *i-random*) :bodyfont 12 :soluzioni t)
  (infoform)
  (framedtext (:context "location=middle, bodyfont=8pt") [Punteggio min. 1pt. Esercizi 1--6 0.5pt. Esercizi 7--9 2pt. TOT: 10pt])  

    (randomize ()
      (esercizi "q-cin2-ch-00087"
		"q-cin2-ch-00088"
		"q-cin2-ch-00089"
		"q-cin2-ch-00090"
		"q-cin2-ch-00091"
		"q-cin2-ch-00166"
		))
      (esercizi "q-cin2-fr-00092")
  
  (soluzioni ()))
