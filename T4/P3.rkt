#lang play
(require math/matrix)
(require math/array)


(defmac (equilibrium (state : (action → target) ...) ...)
  #:keywords : →
  (let ([matr (matrix-transpose (matrix ([action ...] ...)))])
    (letrec ([markov
              (λ (acum)
                (if (equal? (matrix* matr acum)
                            acum)
                    ; if step(t)==step(t+1)
                    (matrix->list acum)
                    (markov (matrix* matr acum))))])
      (markov (matrix-col matr 0)))))  ;comentar esta línea y "descomentar" la línea 22 para comprobar
                                        ;rápidamente el funcionamiento de la macro para el ejemplo "mouse"


    ;(markov (list*->matrix '((1/3) (1/3) (1/3)))))))
    ;(markov (list*->matrix '((0) (1/2) (1/2)))))))
    ;(markov (list*->matrix '((12/53) (20/53) (21/53)))))))  ;;;;esta es la prueba de que funciona... (se
                                                                ;parte desde el estado de equilibrio y
                                                                ;retorna el mismo estado de equilibrio)

;;"descomentar" aquí y linea 22 si se quiere comprobar equilibrium desde el estado de equilibrio
;(define mouse
;  (equilibrium
;   [run   : (0 → 'run)
;          (1/2 → 'eat)
;          (1/2 → 'sleep)]
;   [eat   : (1/4 → 'run)
;          (0 → 'eat)
;          (3/4 → 'sleep)]
;   [sleep : (1/3 → 'run)
;           (2/3 → 'eat)
;           (0 → 'sleep)]))
