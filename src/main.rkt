#lang play
(require "machine.rkt")
(print-only-errors #t) 
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {- <s-expr> <s-expr>}
         | {with {<s-expr> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <s-expr>} [: <type>] <expr>}
         | {<expr> <expr>}         
 
<type> ::= Num
         | {<type> -> <type>}}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s) 
  (fun id targ body tbody)
  (fun-db body) ;tambien se usa en la p3
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg ret))
#|
Definición del ambiente de variables con sus tipos
|#
(deftype TypeEnv
  (mtEnv)
  (tpEnv id type next))
(define empty-type-env (mtEnv))
(define extend-type-env tpEnv)
#|
lookup-type-env ::= id TypeEnv -> Type (o error)
Busca el tipo de la variable x en el ambiente, si no esta tira error de identificador libre.
|#
(define (lookup-type-env x env)
  (match env
    [(mtEnv) (error "Type error: free identifier:" x)]
    [(tpEnv id type next)
     (if (equal? x id)
         type
         (lookup-type-env x next))]))


#|
parse-type: List[Symbol] -> Type (o error)
Parsea el tipo de el contrato de una función.
Por ejemplo: parse-type {Num -> Num} debe dar (TNum) (TNum)
|#
(define (parse-type s-expr)
  (match s-expr
    ['() (error "Parse error")]
    [(quote Num) (TNum)]
    ['(Num) (TNum)]
    [(list arg-expr '-> res-expr) (TFun (parse-type arg-expr) (parse-type res-expr))]
    [(list arg-expr '->) (error "Parse error")]
    [(list '-> res-expr) (error "Parse error")]))

#|
parse: List[Symbol] -> Expr
Parsea una lista de simbolos a un AST del lenguaje con tipos.
|#
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)] ;parseo de numeros
    [(? symbol?) (id s-expr)] ;parseo de identificadores
    [(list '+ l r) (add (parse l) (parse r))] ;parseo de suma
    [(list '- l r) (sub (parse l) (parse r))] ;parseo de resta
    [(list 'fun {list args ': args-type} ': ret-type body) ;parseo de funciones
     (fun args (parse-type args-type) (parse body) (parse-type ret-type))]
    [(list 'fun {list args ': args-type} body) ;parseo de funciones sin retorno
     (fun args (parse-type args-type) (parse body) #f)]
    [(list 'with {list args ': args-type expr-value} body) ;parseo del with
     (app (parse {list 'fun {list args ': args-type} body}) (parse expr-value))]
    [(list expr1 expr2) (app (parse expr1) (parse expr2))]))

(define (prettify t-expr)
  (match t-expr
  [(TNum) 'Num]
  [(TFun t-arg t-ret) (list (prettify t-arg) '-> (prettify t-ret))]))
#|
typeof :: Expr Env -> Type (o error)
Dada una expresion parseada y un ambiente, determina el tipo de esa expresión.
|#
(define (typeof expr [env empty-type-env])
  (match expr
    [(num n) (TNum)]
    [(id x) (lookup-type-env x env)]
    [(add l r) (def type-right (typeof r env))
                (def type-left (typeof l env))
                (if (and (equal? type-right (TNum)) (equal? type-left (TNum)))
                   (TNum)
                   (if (not (equal? type-right (TNum)))
                       (error "Type error in expression + position 2: expected Num found" (prettify type-right))
                       (error "Type error in expression + position 1: expected Num found" (prettify type-left))))]
    [(sub l r) (def type-right (typeof r env))
                (def type-left (typeof l env))
                (if (and (equal? type-right (TNum)) (equal? type-left (TNum)))
                   (TNum)
                   (if (not (equal? type-right (TNum)))
                       (error "Type error in expression - position 2: expected Num found" (prettify type-right))
                       (error "Type error in expression - position 1: expected Num found" (prettify type-left))))]
    [(fun args args-type body ret-type)
     (def body-type (typeof body (extend-type-env args args-type env)))
     (if (or (equal? ret-type body-type) (equal? ret-type #f))
         (TFun args-type body-type)
         (error "Type error in expression fun position 1: expected" (prettify ret-type) 'found (prettify body-type)))] ;TODO,el error que se debe lanzar y ver el caso cuando ret-type vale falso.
    [(app expr1 expr2) ;aplicacion de funciones
     (match expr1 ;veo pattern maching de la primera expresion
       [(id x) ;si es un id, lo busco en el ambiente y debe ser tipo funcion
        (match (lookup-type-env x env)
          [(TNum) (error "Type error in expression app position 1: expected (T -> S) found Num")]
          [(TFun args-type ret-type) (if (equal? args-type (typeof expr2))
                                                 ret-type
                                                 (error "Type error in expression app position 2: expected" (prettify args-type) 'found (prettify (typeof expr2))))])]; si x es TNum, esta mal, si es un TFun veo que el tipo de de args sea el tipo de expr2 y retorno el tipo de su body.
       [(fun args args-type body #f) ;si es tipo funcion sin retorno, veo si los argumentos calzan y devuelvo el typo funcion con el tipo de su body
        (if (equal? args-type (typeof expr2 env))
            (typeof body (extend-type-env args args-type env))
            (error "Type error in expression app position 2: expected" (prettify args-type) 'found (prettify (typeof expr2))))] ; veo si el tipo de argumentos es el mismo que el de los parametros y extiendo el ambiente
       [(fun args args-type body body-type) ;si tiene tipo funcion anotado, veo si los argumentos y el body calzan
        (if (equal? args-type (typeof expr2 env))
            (if (equal? (typeof body (extend-type-env args args-type env)))
                (TFun args-type body-type)
                (error "Type error in expression app position 1: expected" (prettify body-type) 'found (prettify (typeof body))))
            (error "Type error in expression app position 2: expected" (prettify args-type) 'found (prettify (typeof expr2)))
            )]       
       [ _ (error "Type error in expression app position 1: expected (T -> S) found" (prettify (typeof expr1 env)))] ;cualquier otra cosa esta mala
    )]))
#|
TODO
|#
(define (typecheck s-expr)
  (prettify (typeof (parse s-expr))))


;;;;;;;;;
;;;P3;;;;
;;;;;;;;;
(deftype Env
  (BruijnEnv id next))
(define empty-bruijn-env (mtEnv))
(define extend-bruijn-env BruijnEnv)
#|
lookup-bruijn-env::= <id> Env -> <acc> (error)
Busca en el ambiente de bruijn index el id x, y da la expression acc con
su indice asociado.
|#
(define (lookup-bruijn-env x env [pos 0])
  (match env
    [(mtEnv) (error "Free identifier:" x)]
    [(BruijnEnv id next)
     (if (equal? id x)
         (acc pos)
         (lookup-bruijn-env x next (+ pos 1)))]))
#|
TODO
|#
(define (deBruijn expr [env empty-bruijn-env])
  (match expr
    [(num n) (num n)]
    [(id x) (lookup-bruijn-env x env)]
    [(add l r) (add (deBruijn l env) (deBruijn r env))]
    [(sub l r) (sub (deBruijn l env) (deBruijn r env))]
    [(fun args args-type body body-type) (fun-db (deBruijn body (extend-bruijn-env args env)))]
    [(app (fun args args-type body body-type) expr2) (app (fun-db (deBruijn body (extend-bruijn-env args env))) (deBruijn expr2 env))]
    [(app (id x) expr2) (app (lookup-bruijn-env x env) (deBruijn expr2 env))]))

(define (compile expr)
  (match expr
    [(num n) (list (INT-CONST n))]
    [(acc n) (list (ACCESS n))]
    [(add l r) (append (compile r) (compile l) (list (ADD)))]
    [(sub l r) (append (compile r) (compile l) (list (SUB)))]
    [(fun-db body) (list (CLOSURE (append (compile body) (list (RETURN)))))]
    [(app expr1 expr2) (append (compile expr2) (compile expr1) (list (APPLY)))]))

(define (typed-compile s-expr) #f)