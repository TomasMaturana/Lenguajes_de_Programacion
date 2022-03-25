;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 3                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P1.rkt")
(print-only-errors #t)

;; División lazy:
(test/exn (run '(with (halve (fun (n) (/ n 0))) (halve 8)))
          "quotient: undefined for 0")

(test (run '(with (halve (fun (n) (/ n 2)))(halve 7)))
      (numV 3))

(test (run '(with (halve (fun (n) (/ n 0))) 8))
      (numV 8))

(test (run '(with (five (fun (n) 5)) (five (/ n 0))))
      (numV 5))


;; Operaciones consss
(test (cabeza (consss 1 (consss 2 (consss 3 nill))))
      1)

(test (cola (consss 1 (consss 2 (consss 3 nill))))
      (consss 2 (consss 3 nill)))

(test (cola (cola (cola (consss 1 (consss 2 (consss 3 nill))))))
      nill)


;; Listas lazy:
;;parse
(test (parse '(with (l (cons 1 (cons 2 (cons 3 nil)))) (match l as (nil => 0) (cons x xs => (+ x 5)))))
      (app (fun 'l (matchhh (id 'l) 'x 'xs (num 0) (add (id 'x) (num 5)))) (consss (num 1) (consss (num 2) (consss (num 3) (nil))))))

(test (parse '(with (l nil) (match l as (nil => 0) (cons x xs => (+ x 5)))))
      (app (fun 'l (matchhh (id 'l) 'x 'xs (num 0) (add (id 'x) (num 5)))) (nil)))

(test (parse '(with (l (cons 1 (cons (/ 2 0) nil ))) (match l as (nil => 0) (cons y ys => (+ 10 y)))))
      (app (fun 'l (matchhh (id 'l) 'y 'ys (num 0) (add (num 10) (id 'y)))) (consss (num 1) (consss (div (num 2) (num 0)) (nil)))))

(test (parse '(with (l (cons (/ 1 0) (cons (/ 2 0) nil ))) (match l as (nil => 0) (cons z zs => 1))))
      (app (fun 'l (matchhh (id 'l) 'z 'zs (num 0) (num 1))) (consss (div (num 1) (num 0)) (consss (div (num 2) (num 0)) (nil)))))

(test (parse '(with (l (cons (+ 1 2) (cons 3 nil ))) (match l as (nil => 0) (cons y ys => y))))
      (app (fun 'l (matchhh (id 'l) 'y 'ys (num 0) (id 'y))) (consss (add (num 1) (num 2)) (consss (num 3) (nil)))))

(test (parse '(with (x 1)(with (l (cons (+ 5 x) (cons 200 nil))) (match l as (nil => 0) (cons x xs => (+ 10 x))))))
      (app (fun 'x (app (fun 'l (matchhh (id 'l) 'x 'xs (num 0) (add (num 10) (id 'x)))) (consss (add (num 5) (id 'x)) (consss (num 200) (nil))))) (num 1)))

(test (parse '(with (l (cons (cons 1 (cons (/ 2 0) nil )) (cons (cons 3 nil) nil ))) (match l as (nil => 0) (cons x xs => (match x as ( nil => 1) (cons y ys => y))))))
      (app (fun 'l (matchhh (id 'l) 'x 'xs (num 0) (matchhh (id 'x) 'y 'ys (num 1) (id 'y)))) (consss (consss (num 1) (consss (div (num 2) (num 0)) (nil))) (consss (consss (num 3) (nil)) (nil)))))


;;run
(test (run '(with (l (cons 1 (cons 2 (cons 3 nil)))) (match l as (nil => 0) (cons x xs => (+ x 5)))))
      (numV 6))

(test (run '(with (l nil) (match l as (nil => 0) (cons x xs => (+ x 5)))))
      (numV 0))

(test (run '(with (l (cons 1 (cons 2 (cons 3 nil)))) (match l as (nil => 0) (cons x xs => xs))))
      (exprV (consss (num 2) (consss (num 3) (nil))) (mtEnv) '#&#f))

(test (run '(with (l (cons 1 (cons (/ 2 0) nil ))) (match l as (nil => 0) (cons y ys => (+ 10 y)))))
      (numV 11))

(test (run '(with (l (cons (/ 1 0) (cons (/ 2 0) nil ))) (match l as (nil => 0) (cons z zs => 1))))
      (numV 1))

(test (run '(with (l (cons (+ 1 2) (cons 3 nil ))) (match l as (nil => 0) (cons y ys => y))))
      (exprV (add (num 1) (num 2)) (mtEnv) '#&#f))

(test (run '(with (x 1)(with (l (cons (+ 5 x) (cons 200 nil))) (match l as (nil => 0) (cons x xs => (+ 10 x))))))
      (numV 16))

(test (run '(with (l (cons (cons 1 (cons (/ 2 0) nil )) (cons (cons 3 nil) nil ))) (match l as (nil => 0) (cons x xs => (match x as ( nil => 1) (cons y ys => y))))))
      (exprV (num 1) (mtEnv) '#&#f))