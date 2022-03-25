#lang play
(require math/flonum)

#|
Complete sus datos personales:
NOMBRE Y APELLIDO: Tomás Maturana
RUT: 19.186.973-7
|#

;; Parte a)
; CFraction ::=  <val>
;      | { <val> <val> <CFraction> }
(deftype CFraction
  (simple s)
  (compound a b d))



;; Parte b)
;; eval :: CFraction -> Rational
;; Evalúa una fracción continua, devolviendo el número racional que representa.
(define (eval CF)
  (match CF
    [(simple s) s]
    [(compound a b d) (+ a (/ b (eval d)))]))



;; Parte c)
;; degree ::  CFraction -> Integer
;; Devuelve el grado de una fracción continua.
(define (degree CF)
  (match CF
    [(simple s) 0]
    [(compound a b d) (+ 1 (degree d))]))



;; Parte d)
;; fold :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; Captura el esquema de recursión asociado aCFraction
(define (fold f g)
  (λ (CF)
    (match CF
      [(simple s) (f s)]
      [(compound a b d) (g a b ((fold f g) d))])))



;; Parte e)
;; eval2 :: CFraction -> Rational
(define (eval2 CF)
  ((fold identity (λ (a b D) (+ a (/ b D)))) CF))

;; degree2 ::  CFraction -> Integer
(define (degree2 CF)
  ((fold (λ x 0) (λ (a b D) (+ 1 D))) CF))



;; Parte f)

;Primera versión, con valor por defecto (NO VÁLIDA PARA LA CORRECCIÓN, sólo la pongo porque me di una vuelta por aquí para llegar a la solución final)
;(define (sumCF n CF)
;  (match CF
;    [(simple s) (simple (+ s n))]
;    [(compound a b d) (compound (+ a n) b d)]))
;
;(define (mysterious-cf i [counter 1])
;  (cond [(< i 0) (print "Error: argumento negativo")]
;        [else (if (>= i counter)
;                  (compound 3 (expt (- (* 2 counter) 1) 2) (sumCF 3 (mysterious-cf i (+ counter 1))))
;                  (simple 3))]))


;sumCFD :: CFraction Integer Simple -> CFraction
;Le suma x/y al denominador del CFraction de mayor grado, dentro del CFraction que se le entrega a la función
(define (sumCFD CF x y)
  (match CF
    [(simple s) (compound s x y)]
    [(compound a b d) (compound a b (if (simple? d)
                                        (compound (eval d) x y)
                                        (sumCFD d x y)))]))

; mysterious-cf :: Integer -> CFraction
;Genera secuencias de fracciones continuas de grado i según un patrón en particular (un patrón misterioso?)
(define (mysterious-cf i)
  (cond [(< i 0) (error "Error: argumento negativo")]
        [(zero? i) (simple 3)]
        [else (sumCFD (mysterious-cf (- i 1)) (expt (- (* 2 i) 1) 2) (simple 6))]))



;; Parte g)
;; from-to :: Integer -> Integer -> listOf Integer
;; construye una lista de enteros comprendidos entre dos enteros, X e Y respectivamente
(define (from-to x y)
  (cond [(= x y) (list y)]
        [(< x y) (cons x (from-to (+ x 1) y))]
        [(> x y) empty]))
      

;; mysterious-list :: Integer -> listOf Float
;; devuelve la lista de los primeros n valores de mysterious-cf
(define (mysterious-list i)
  (map fl (map eval (map mysterious-cf (from-to 0 i)))))

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?
;; Tiende a pi (3.14159265359...)


;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; transforma un número racional no-negativo en su representación en formade fracción continua.
(define (rac-to-cf r)
  (if (integer? r)
      (simple r)
      (compound (floor r) 1 (rac-to-cf (/ (- r (floor r)))))))











