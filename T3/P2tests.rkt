;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 3                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P2.rkt")
(print-only-errors #t)


;; rec

(test (run '(rec (len (fun (l) (match l as (nil => 0)(cons x xs => (+ 1 (len xs))))))(len (cons 1 (cons 2 (cons (/ 2 0) nil ))))))
      (numV 3))

(test (run '(rec (ones (cons 1 ones))(match ones as ( nil => 0)(cons x xs => (match xs as ( nil => 0)(cons y ys => (+ x y)))))))
      (numV 2))



;; reductor

(test (reductor (consss (consss (add (num 1) (num 2)) (nil)) (consss (consss (add (num 3) (num 4)) (nil)) (nil))) empty-env)
      (consss (consss (numV 3) (nil)) (consss (consss (numV 7) (nil)) (nil))))

(test (reductor (add (num 1) (num 2)) empty-env)
      (numV 3))

(test/exn (reductor (div (add (num 1) (num 2)) (num 0)) empty-env)
      "quotient: undefined for 0")


;; deeprun

(test (deeprun '(with (l (cons (cons (+ 1 2) nil)
                               (cons (cons (+ 3 4) nil) nil )))
                      l))
      (lclosureV (consss (consss (numV 3) (nil)) (consss (consss (numV 7) (nil)) (nil))) (mtEnv)))


(test (deeprun '(with (l (cons (+ 1 2) (cons 3 nil)))
                      (match l as (nil => 0) (cons y ys => y))))
      (numV 3))



;; En estos casos no comprendo qué sucede... son los únicos que no pasan de los test aquí presentes.
;; Le dediqué mucho tiempo a la tarea en general y ya me rindo con esta parte :c
;; Al menos funciona cuando no hay valores en cajas (^ test de arriba ^)
;; Creo que estuve casi casi en la respuesta correcta, pero ya estoy odiando esta tarea dkhskgsadjfh 
(test (deeprun '(with (l (cons (+ 1 1) (cons (+ 2 2) (cons (+ 3 3) nil))))
            (rec (taken (fun (n)
                             (fun (l) (if0 n
                                           nil
                                           (match l as
                                             (nil => nil)
                                             (cons x xs => (cons x ((taken (- n 1)) xs))))))))
              ((taken 2) l))))
      (lclosureV (consss (numV 2) (consss (numV 4) (nil))) (mtEnv)))


(test (deeprun '(with (l (cons (+ 1 1) (cons (+ 2 2) (cons (/ 3 0) nil ))))
                      (rec (taken (fun (n)
                                       (fun (l) (if0 n
                                                      nil
                                                      (match l as
                                                        (nil => nil)
                                                        (cons x xs => (cons x ((taken (- n 1))xs))))))))
                        ((taken 2) l))))
      (lclosureV (consss (numV 2) (consss (numV 4) (nil))) (mtEnv)))


(test (deeprun '(rec (ones (cons 1 ones))
                  (rec (taken (fun (n)
                                   (fun (l) (if0 n
                                                 nil
                                                 (match l as
                                                   (nil => nil)
                                                   (cons x xs => (cons x ((taken (- n 1)) xs))))))))
                    ((taken 4) ones))))
      (lclosureV (consss (numV 1) (consss (numV 1) (consss (numV 1) (consss (numV 1) (nil))))) (mtEnv)))



