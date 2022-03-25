;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 2                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P2.rkt")
(print-only-errors #t)

;; Test parser
(test (parse'True) (bool #t))
(test (parse'(A ^ False)) (band (id 'A) (bool #f)))
(test (parse'(False ^ A)) (band (bool #f) (id 'A)))
(test (parse'(C ^ (A v B))) (band (id 'C) (bor (id 'A) (id 'B))))
(test (parse'((A v B) ^ C)) (band (bor (id 'A) (id 'B)) (id 'C)))
(test (parse'(with (A True) (False ^ A))) (with 'A (bool #t) (band (bool #f) (id 'A))))
(test (parse'(with (A True) (A ^ False))) (with 'A (bool #t) (band (id 'A) (bool #f))))
(test (parse'(with (A False) (True v A))) (with 'A (bool #f) (bor (bool #t) (id 'A))))
(test (parse'((with (A True) (False ^ A)) v (with (A False) (True v A)))) (bor (with 'A (bool #t) (band (bool #f) (id 'A))) (with 'A (bool #f) (bor (bool #t) (id 'A)))))
(test (parse'(with (A False) (A ^ (with (A False) (True v A))))) (with 'A (bool #f) (band (id 'A) (with 'A (bool #f) (bor (bool #t) (id 'A))))))
(test (parse'(with (A True) (A ^ (with (A False) (True v A))))) (with 'A (bool #t) (band (id 'A) (with 'A (bool #f) (bor (bool #t) (id 'A))))))
(test (parse'(with (A True) (A ^ (with (A False) (True ^ A))))) (with 'A (bool #t) (band (id 'A) (with 'A (bool #f) (band (bool #t) (id 'A))))))
(test (parse'(with (A (with (A False) (True v A))) (False ^ A))) (with 'A (with 'A (bool #f) (bor (bool #t) (id 'A))) (band (bool #f) (id 'A))))
(test (parse'(with (A (with (A (with (A False) (True v A))) (False v A))) (False v A))) (with 'A (with 'A (with 'A (bool #f) (bor (bool #t) (id 'A))) (bor (bool #f) (id 'A))) (bor (bool #f) (id 'A))))


;; Test intérprete 
(test (interp (parse'True) empty-env) (BoolV #t))
(test/exn (interp (parse'(A ^ False)) empty-env) "Identificador A no definido")
(test/exn (interp (parse'(False ^ A)) empty-env) "Identificador A no definido")
(test/exn (interp (parse'(C ^ (A v B))) empty-env) "Identificador B no definido")
(test/exn (interp (parse'((A v B) ^ C)) empty-env) "Identificador C no definido")
(test (interp (parse'(with (A True) (False ^ A))) empty-env) (BoolV #f))
(test (interp (parse'(with (A True) (A ^ False))) empty-env) (BoolV #f))
(test (interp (parse'(with (A False) (True v A))) empty-env) (BoolV #t))
(test (interp (parse'((with (A True) (False ^ A)) v (with (A False) (True v A)))) empty-env) (BoolV #t))
(test (interp (parse'(with (A False) (A ^ (with (A False) (True v A))))) empty-env) (BoolV #f))
(test (interp (parse'(with (A True) (A ^ (with (A False) (True v A))))) empty-env) (BoolV #t))
(test (interp (parse'(with (A True) (A ^ (with (A False) (True ^ A))))) empty-env) (BoolV #f))
(test (interp (parse'(with (A (with (A False) (True v A))) (False ^ A))) empty-env) (BoolV #f))
(test (interp (parse'(with (A (with (A (with (A False) (True v A))) (False v A))) (False v A))) empty-env) (BoolV #t))
(test/exn (interp (parse'(with (A (with (A (with (A False) (J v A))) (False v A))) (False v A))) empty-env) "Identificador J no definido")
(test/exn (interp (parse'(with (A (with (A (with (A False) (True v A))) (False v A))) (False v J))) empty-env) "Identificador J no definido")


;; Test lookup
(define x 'x)
(define y 'y)
(define e (extend-env x (BoolV #t) (extend-env y (BoolV #t) (extend-env x (BoolV #f) mtEnv))))
(test (env-lookup x e) (BoolV #t))
(test (env-lookup y e) (BoolV #t))
(test/exn (env-lookup x empty-env) "Identificador x no definido")


;; Test lband
(test (lband (BoolV #t) (BoolV #t)) (BoolV #t))
(test (lband (BoolV #f) (BoolV #t)) (BoolV #f))
(test (lband (BoolV #t) (BoolV #f)) (BoolV #f))
(test (lband (BoolV #f) (BoolV #f)) (BoolV #f))


;; Test lbor
(test (lbor (BoolV #t) (BoolV #t)) (BoolV #t))
(test (lbor (BoolV #f) (BoolV #t)) (BoolV #t))
(test (lbor (BoolV #t) (BoolV #f)) (BoolV #t))
(test (lbor (BoolV #f) (BoolV #f)) (BoolV #f))


