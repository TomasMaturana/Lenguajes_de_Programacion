;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  BASE - P2                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Nombre y Apellido: Tomás Maturana
- Modificar y guardar como P2.rkt
- Recuerde hacer los tests en P2_tests.rkt
|#

#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym list> <expr>)
         | (app <expr> <expr list>)
         | {seq <Expr> <Expr>}
         | {set <id> <Expr>}
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
  (seq e1 e2)
  (set id expr))



;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactical sugar
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun x b) (fun (map parse x) (parse b))]   
    [(list 'with (list x e) b) ;#:when (symbol? x)
         (app (fun x (parse b)) (parse e))]
    [(list 'seq e1 e2) (seq (parse e1) (parse e2))]
    [(list 'set i e) (set i (parse e))]
    [(list f a) (app (parse f) (parse a))]
    [ l #:when (list? l) (map parse l) ]))


;(parse expr1)
;(app (fun 'f (app (id 'f) (num 4))) (fun 'y (id 'y)))

(defmac (mlist listt)
  (letrec ([iter
            (lambda (l) (match l
                          ['() (void)]
                          ;[(list a) (parse a)]
                          [(cons x xs) (list (parse x) (iter xs))]))])
    ( iter listt )))

(defmac (mlet ([id val] ...) body)
  ((λ (id ...) body) val ...))

;; Interface of the Abstract Dada Type (ADT) for  
;; representing idenfifier environments

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; lookup-env :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env mtEnv)
 
(define extend-env aEnv)
 
(define (lookup-env x env)
  (match env
    [(mtEnv) (error 'lookup-env "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (lookup-env x rest))]))


; store (memoria)
; Sto
; empty-sto :: Sto
; extend-sto :: Loc x Val x Sto -> Sto
; lookup-sto :: Loc x Sto -> Val (o memory error! (segfault))
(deftype Sto
  (mtSto)
  (aSto loc val next)) 

(define empty-sto mtSto)
(define extend-sto aSto)
(define (lookup-sto loc sto)
  (match sto
    [(mtSto) (error "invalid address:" loc)]
    [(aSto l v n)
     (if (equal? loc l)
         v
         (lookup-sto loc n))]))


;; values of expressions
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>) 
(deftype Value
  (numV n)
  (closureV id body env)
  (refclosV x b e)
  (voidV))


(deftype Value*Store
  (v*s val sto))


;; Auxiliary functions handling numeric values
;; binop :: (Num Num -> Num) -> (Value Value -> Value)
;; Lifts a binary numeric operator to (numeric) Value's 
(define (binop op)
  (λ (n1 n2)
    (def (numV v1) n1) (def (numV v2) n2) (numV (op v1 v2))))

;; unop :: (Num -> A) -> (Value -> A)
;; Lifts a function over Num to (numeric) Value 
(define (unop op)
  (λ (n) (def (numV v) n) (op v)))



(define (malloc sto)
  (match sto
    [(mtSto) 0]
    [(aSto _ _ s) (add1 (malloc s))]))


(define (desID i)
  (match i
    [(id x) x]))

;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env sto)
  (match expr
    [(num n) (v*s (numV n) sto)]
    [(fun i body)
     (match i
       ;[(list) ]
       [ x #:when (symbol? x) (v*s (closureV x body env) sto)]
       [(id x) (v*s (closureV x body env) sto)]
       [(cons l r) (v*s (closureV (desID l) body env) sto)])]
    [(id x) (v*s (lookup-sto (lookup-env x env) sto) sto)]
    [(add l r)
     (def (v*s l-val l-sto) (eval l env sto))
     (def (v*s r-val r-sto) (eval r env l-sto))
     (v*s ((binop +) l-val r-val) r-sto)]
    [(sub l r)
     (def (v*s l-val l-sto) (eval l env sto))
     (def (v*s r-val r-sto) (eval r env l-sto))
     (v*s ((binop -) l-val r-val) r-sto)]
    [(if0 c t f)
     (def (v*s c-val c-sto) (eval c env sto))
     (if ((unop zero?) c-val)
         (eval t env c-sto)
         (eval f env c-sto))]
    [(app fun-expr arg-expr)
     (def (v*s fun-val fun-sto) (eval fun-expr env sto))
     (match fun-val
       [(closureV id body fenv) 
        (def (v*s arg-val arg-sto) (eval arg-expr env fun-sto))
        (def new-loc (malloc arg-sto))
        (eval body
                (extend-env id new-loc fenv)
                (extend-sto new-loc arg-val arg-sto))]
       [(refclosV id body fenv) 
        (eval body   
                (extend-env id (lookup-env (id-x arg-expr) env) fenv)
                sto)]
       [_ (error "not a function")])]
    [(seq e1 e2)
     (def (v*s _ e1-sto) (eval e1 env sto))
     (eval e2 env e1-sto)]
    [(set var expr)
     (def var-loc (lookup-env var env)) 
     (def (v*s val-val val-sto) (eval expr env sto))
     (v*s (voidV) (extend-sto var-loc val-val val-sto))]))


;(define (storee fun-expr env sto)
;  (match fun-expr
;    ['() (void)]
;    [(cons x xs) (def (v*s fun-val fun-sto) (eval fun-expr env sto))
;                 (storee xs env fun-sto)]))


       

;; is-ref-expr?:: sym -> boolean
;; Retorna verdadero si un simbolo corresponde a una expresion de referencia ('&var), o falso sino.
(define (is-ref-expr? expr)
  (def symstr (symbol->string expr))
  (equal? (string-ref symstr 0) #\&)) 

;; get-id:: sym -> sym
;; Retorna el valor de una id de referencia ('&var) en su id real ('var).
(define (get-id id)
  (string->symbol (substring (symbol->string id) 1)))


;; run :: s-expr -> value
;; evaluates an expression using static scoping
(define (run prog)
  (match (eval (parse prog) (empty-env) (empty-sto))
    [(v*s (numV n) _) (numV n)]
    [(v*s (closureV x b e) _) '<procedure>]
    [(v*s (voidV) _) '<void>]))


;; some testing
(define expr1 '(with (f (fun (y) y)) (f 4)))
(test (run expr1) (numV 4))
(define expr2 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (f 4))))
(test (run expr2) (numV 7))
(define expr3 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (with (x 5) (+ x (f 4))))))
(test (run expr3) (numV 12))