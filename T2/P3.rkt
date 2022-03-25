;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P3 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOMBRE APELLIDO: Tomás Maturana
;; Mucho éxito :)

#lang play

#| ================================
                PARTE A
   ================================|#
#|
<expr> ::= (real <num>)
         | {comp <num> <num>}
         | {add <expr> <expr>}
         | <id>
         | {fun <sym> <expr>}
         | {app <expr> <expr>}
|#
(deftype Expr
  (num n)
  (comp n i)
  (add l r)
  (id s)
  (fun id body)
  (app fun-expr arg-expr))



#| ================================
                PARTE B
   ================================ |#

#|
<s-expr> ::= <num>
           | <sym>
           | (list '+ <num> <num> 'i
           | (list '+ <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list <s-expr> 'where <sym> '= <s-expr>  <- syntactical sugar
|#
;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '+ l (list r) 'i) (comp l r)]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list body 'where var '= val)
     (app (fun var (parse body)) (parse val))]))


#| ================================
                PARTE C
   ================================ |#

;; Values of Expressions
#|
<value> ::=
          | (realV <num>
          | (compV <num> <num>
          | (closureV <sym> <expr> <env>
|#
(deftype Value
  (realV n)
  (compV n i)
  (closureV id body env))

;; Maneja las sumas del lenguaje, tanto para las sumas de números reales, como complejos
;; num+ :: Value Value -> Value
(define (num+ l r)
  (match* (l r)
    [((realV ln) (realV rn)) (realV (+ ln rn))]
    [((realV ln) (compV rn ri)) (compV (+ ln rn) ri)]
    [((compV ln li) (realV rn)) (compV (+ ln rn) li)]
    [((compV ln li) (compV rn ri)) (compV (+ ln rn) (+ li ri))]))



#| ================================
                PARTE D
   ================================ |#

;; Interfaz del tipo de dato abstracto que
;; representa los ambientes de identificadores.
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; empty-env  :: Env
(def empty-env  (mtEnv))

;; extend-env :: Symbol Value Env -> Env
(def extend-env aEnv)

;; Verifica si x fue definido en ambiente env
;; env-lookup :: Symbol Env -> Value
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "Identificador ~a no definido" x)]
    [(aEnv id val rest)
     (if (symbol=? id x)
         val
         (env-lookup x rest))]))


;; Evalúa una expresión en el ambiente env y devuelve su valor con tipo Value
;; eval :: Expr Env -> Value
(define (eval expr env)
  (match expr
    [(num n) (realV n)]
    [(comp l r) (compV l r)]
    [(fun id body) (closureV id body env)]
    [(add l r) (num+ (eval l env) (eval r env))]
    [(id x) (env-lookup x env)]
    [(app fun-expr arg-expr)
     (def (closureV id body fenv) (eval fun-expr env))
     (eval body
             (extend-env id
                         (eval arg-expr env)
                         fenv))]))

;; run :: s-expr -> Value
(define (run prog) (eval (parse prog) empty-env))



