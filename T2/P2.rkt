;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P2 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOMBRE APELLIDO: Tomás Maturana
;; Mucho éxito :)

#lang play


#| ================================
                PARTE A
   ================================|#
;; Interfaz del tipo de dato abstracto que
;; representa un subconjunto de la lógica proposicional.
#| <logic> ::= 
    | (bool <bool>)
    | (id <id>)
    | (band <logic> <logic>)
    | (bor <logic> <logic>)
    | (with <id> <logic> <logic>)
|#

(deftype Logic
  (bool b)
  (id x)
  (band exprA exprB)
  (bor exprA exprB)
  (with x b body))

#| ================================
                PARTE B
   ================================ |#

#| <s-expr> ::= <sym>
   | (list 'band <s-expr> <s-expr>)
   | (list 'bor <s-expr> <s-expr>)
   | (list 'with <id> <bool> <s-expr>)
|#

;; parse :: s-expr -> logic
;; Transforma una expresión válida al tipo Logic
(define (parse sexpr)
  (match sexpr
    ['True (bool #t)]
    ['False (bool #f)]
    [ x #:when (symbol? x) (id x) ]
    [(list l 'v r) (bor (parse l) (parse r))]
    [(list l '^ r) (band (parse l) (parse r))]
    [(list 'with (list x b) body) #:when (symbol? x) (with x (parse b) (parse body))]))

#| ================================
                PARTE C 
   ================================ |#
#| <lvalue> ::= <BoolV>
|#
(deftype LValue
  (BoolV b))


#| ================================
                PARTE D
   ================================ |#

;; Interfaz del tipo de dato abstracto que
;; representa los ambientes de identificadores.
;;<env> ::= mtEnv
;;        | (aEnv <id> <lvalue> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; empty-env  :: Env
(define empty-env (mtEnv))

;; extend-env :: Sym LValue Env -> Env
(define extend-env aEnv)

;; env-lookup :: Sym Env -> LValue
;; Verifica si x fue definido en ambiente env
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "Identificador ~a no definido" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                        val
                        (env-lookup x rest))]))



;; interp :: Expr Env -> LValue
;; Evalúa una expresión del tipo Logic en el ambiente env y devuelve su valor con tipo LValue
(define (interp expr env)
  (match expr
    [(bool b) (BoolV b)]
    [(band l r) (lband (interp r env) (interp l env))]
    [(bor l r) (lbor (interp r env) (interp l env))]
    [(with x e b) (def new-env (extend-env x (interp e env) env))
       (interp b new-env)]
    [(id x) (env-lookup x env)]))


;; lband :: LValue -> LValue
;; Y lógico para el tipo LValue
(define (lband l r)
  (BoolV (and (match l [(BoolV lb) lb])
       (match r [(BoolV rb) rb]))))

;; lbor :: LValue -> LValue
;; O lógico para el tipo LValue
(define (lbor l r)
  (BoolV (or (match l [(BoolV lb) lb])
       (match r [(BoolV rb) rb]))))












