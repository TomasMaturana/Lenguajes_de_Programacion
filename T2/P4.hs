{-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P4 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOMBRE APELLIDO: Tomás Maturana
-}
stirling :: Integer -> Integer -> Integer
stirling 0 0 = 1
stirling 0 k = 0
stirling n 0 = 0
stirling n k = stirling (n - 1) (k - 1) + k * stirling (n - 1) k


stirling_list :: Integer -> Integer -> [Integer]
stirling_list 0 0 = [stirling 0 0]
stirling_list n 0 = [stirling n 0]
stirling_list n k = stirling n k : stirling_list n (k-1)


stirling_aux :: Integer -> [[Integer]]
stirling_aux n = reverse (stirling_list n n) : stirling_aux (n+1)


triangulo_stirling :: [[Integer]]
triangulo_stirling = stirling_aux 0 
