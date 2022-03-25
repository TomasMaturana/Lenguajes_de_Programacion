;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 2                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P3.rkt")
(print-only-errors #t)

;; Test parse
(test (parse '8) (num 8))
(test (parse '(+ 3 6)) (add (num 3) (num 6)))
(test (parse 'x) (id 'x))
(test (parse '(+ 1 (2)i)) (comp 1 2))
(test (parse '(+ 3 (+ 1 (2)i))) (add (num 3) (comp 1 2)))
(test (parse '(fun (x) (+ 2 x))) (fun 'x (add (num 2) (id 'x))))
(test (parse '(f (+ 2 3))) (app (id 'f) (add (num 2) (num 3))))
(test (parse '((+ y 5) where y = (+ 2 4))) (app (fun 'y (add (id 'y) (num 5))) (add (num 2) (num 4))))
(test (parse '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x)))) (app (fun 'f (app (id 'f) (add (num 2) (comp 1 2)))) (fun 'x (add (id 'x) (id 'x)))))
(test (parse '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x ((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x))))))) (app (fun 'f (app (id 'f) (add (num 2) (comp 1 2)))) (fun 'x (add (id 'x) (app (fun 'f (app (id 'f) (add (num 2) (comp 1 2)))) (fun 'x (add (id 'x) (id 'x))))))))


;; Test num+
(test (num+ (realV 2) (realV 5)) (realV 7))
(test (num+ (realV 2) (compV 5 3)) (compV 7 3))
(test (num+ (compV 2 3) (realV 5)) (compV 7 3))
(test (num+ (compV 2 3) (compV 5 3)) (compV 7 6))


;; Test lookup
(define x 'x)
(define y 'y)
(define e (extend-env x (realV 1) (extend-env y (compV 2 3) (extend-env x (realV 8) mtEnv))))
(test (env-lookup x e) (realV 1))
(test (env-lookup y e) (compV 2 3))
(test/exn (env-lookup x empty-env) "Identificador x no definido")


;; Test eval
(test (eval (parse '8) empty-env) (realV 8))
(test (eval (parse '(+ 3 6)) empty-env) (realV 9))
(test/exn (eval (parse 'x) empty-env) "Identificador x no definido")
(test (eval (parse 'x) e) (realV 1))
(test (eval (parse '(+ 1 (2)i)) empty-env) (compV 1 2))
(test (eval (parse '(+ 3 (+ 1 (2)i))) empty-env) (compV 4 2))
(test (eval (parse '(fun (x) (+ 2 x))) empty-env) (closureV 'x (add (num 2) (id 'x)) (mtEnv)))
(test/exn (eval (parse '(f (+ 2 3))) empty-env) "Identificador f no definido")
(test (eval (parse '((+ y 5) where y = (+ 2 4))) empty-env) (realV 11))
(test (eval (parse '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x)))) empty-env) (compV 6 4))
(test (eval (parse '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x ((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x))))))) empty-env) (compV 9 6))


;; Test run
(test (run '8) (realV 8))
(test (run '(+ 3 6)) (realV 9))
(test/exn (run 'x) "Identificador x no definido")
(test (run '(+ 1 (2)i)) (compV 1 2))
(test (run '(+ 3 (+ 1 (2)i))) (compV 4 2))
(test (run '(fun (x) (+ 2 x))) (closureV 'x (add (num 2) (id 'x)) (mtEnv)))
(test/exn (run '(f (+ 2 3))) "Identificador f no definido")
(test (run '((+ y 5) where y = (+ 2 4))) (realV 11))
(test (run '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x)))) (compV 6 4))
(test (run '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x ((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x))))))) (compV 9 6))