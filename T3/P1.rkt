#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <id> <expr>)
         | (app <expr> <expr>)
         | (div <expr> <expr>)
         | (matchhh <id> <id> <id> <expr> <expr>)
         | (consss <expr> <expr>)
         | (nil)
|#
;; Inductive type for representing (the abstract syntax
;; of) an aritmetical language with first-class functions
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg)
  (div l r)
  (matchhh l x xs nilC otherC)
  (consss l r)
  (nil))

(define nill (nil))


;; cabeza :: consss -> consss
;; retorna la cabeza de la lista c
(define (cabeza c)
  (match c
    [(consss l r) l]
    [nil nil]))

;; cola :: consss -> consss
;; retorna la cola de la lista c
(define (cola c)
  (match c
    [(consss l r) r]
    [nil nil]))


;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list '/  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactic sugar
           | (list 'consss <s-expr> <s-expr>)
           | (list 'match <sym> 'as (list 'nil '=> <s-expr>) (list 'cons <sym> <sym> '=> <s-expr>))
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    ['nil nill]
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '/ l r) (div (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]
    ;[(list 'with (list x e) (list 'match id 'as
    ;                              (list 'nil '=> nilCase)
    ;                              (list 'cons x xs '=> otherCase))) #:when (symbol? x)
    ;                                                                (match e
    ;                                                                  ['nil (app (fun x (parse nilCase)) (parse e))]
    ;                                                                  [_ (app (fun x (parse otherCase)) (parse e))])]
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]
    [(list 'cons l r) (consss (parse l) (parse r))]
    [(list 'match ID 'as
           (list 'nil '=> nilCase)
           (list 'cons x xs '=> otherCase))
     (matchhh (parse ID) x xs (parse nilCase) (parse otherCase))]))



;; Interface of the Abstract Dada Type (ADT) for  
;; representing idenfifier environments

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))



;; values of expressions
(deftype Value
  (numV n)
  (fclosureV id body env)
  (exprV expr env cache)
  (lclosureV id env))


;; binop :: (Num Num -> Num) -> (Value Value -> Value)
;; Lifts a binary numeric operator to (numeric) Value's 
(define (binop op)
  (λ (n1 n2)
    (def (numV v1) n1) (def (numV v2) n2) (numV (floor (op v1 v2)))))

;; unop :: (Num -> A) -> (Value -> A)
;; Lifts a function over Num to (numeric) Value 
(define (unop op)
  (λ (n) (def (numV v) n) (op v)))

;; Further reduces a Value to a numV or closureV
;; and caches the reduced value
;; strict :: Value -> Value [without exprV]
(define (strict v)
  (match v
    [(exprV expr env cache)
     (if (not (unbox cache))
         (let ([val (strict (eval expr env))])
         (set-box! cache val)
         val)
         (unbox cache))]
    [ _ v]))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given environment
;; using static scope and lazy evaluation
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (fclosureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) ((binop +) (strict (eval l env)) (strict (eval r env)))]
    [(sub l r) ((binop -) (strict (eval l env)) (strict (eval r env)))]
    [(div l r) (if (equal? (strict (eval r env)) (numV 0))
                   (error "quotient: undefined for 0")
                   ((binop /) (strict (eval l env)) (strict (eval r env))))]
    [(if0 c t f) (if  ((unop zero?) (strict (eval c env)))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (fclosureV the-arg the-body the-clos-env) (strict (eval f env)))
               (def the-ext-env (extend-env the-arg
                                            (exprV e env (box #f))
                                            the-clos-env))
               (eval the-body the-ext-env)]
    [(matchhh l x xs nilC otherC) (def (lclosureV id the-clos-env) (strict (eval l env)))
                                  (def the-ext-env (extend-env x
                                                               (exprV (cabeza id) the-clos-env (box #f))
                                                               env))
                                  (def the-ext-env2 (extend-env xs
                                                               (exprV (cola id) the-clos-env (box #f))
                                                               the-ext-env))
                                  (if (nil? id)
                                      (eval nilC env)
                                      (eval otherC the-ext-env2))]
    [(consss l r) (lclosureV (consss l r) env)]
    [(nil) (lclosureV nill env)]))


;; run :: s-expr -> value
;; evaluates an expression using static scope and lazy evaluation 
(define (run prog)
   ;(strict (eval (parse prog) empty-env)))
  (eval (parse prog) empty-env))




